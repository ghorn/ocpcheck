{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language RankNTypes #-}

module CslSteady ( main ) where 

import qualified Data.Vector as V
import GHC.Generics

import Dyno.Vectorize
import Dyno.View
import qualified Dyno.TypeVecs as TV
import Dyno.Ipopt
import Dyno.Nlp
import Dyno.NlpSolver

import Dyno.Ocp
import Dyno.DirectCollocation
import Dyno.Cov
import Dyno.DirectCollocation.Dynamic ( toMeta, ctToDynamic )
import Dyno.DirectCollocation.Formulate ( makeGuess )


import GliderShared
import ServerSender ( withCallback )
import Dyno.Nats
import Dyno.Server.Accessors ( Lookup(..) )

import CslData

type NCollStages = D100
type CollDeg = D3

data BX a = BX (CX a) (CX a) deriving (Eq, Functor, Generic, Generic1, Show)
instance Vectorize BX
instance (Lookup a, Generic a) => Lookup (BX a)

daeM :: Floating a => Dae CX None U None CX None a
daeM x' x _ u _ _ = (modelForces' x' x u kp kt, None)

ocp :: (Vectorize bc, Vectorize pc) =>
       (forall a . Floating a => a -> CX a -> CX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a)
       -> (forall a . Floating a => CX a -> None a -> U a -> None a -> None a -> a -> a)
       -> (forall a . Floating a => Dae CX None U None CX None a)
       -> (forall a . Floating a => CX a -> CX a -> bc a) 
       -> (forall a . Floating a => CX a -> None a -> U a -> None a -> None a -> a -> pc a)
       -> (forall a . Floating a => (Maybe a, Maybe a))
       -> OcpPhase CX None U None CX None bc pc JNone JNone JNone
ocp mayer lagrange dae bc pathc tbd = OcpPhase { ocpMayer = mayer
               , ocpLagrange = lagrange
               , ocpDae = dae
               , ocpBc = bc
               , ocpPathC = pathc
               , ocpPathCBnds = fill (Just 0, Just 0)
               , ocpBcBnds = fill (Just 0, Just 0)
               , ocpXbnd = xbnd
               , ocpUbnd = ubnd
               , ocpZbnd = None
               , ocpPbnd = None
               , ocpTbnd = tbd

               , ocpSq = 0
               , ocpSbnd = jfill (Nothing,Nothing)
               , ocpSbc = \_ _ -> cat JNone
               , ocpSbcBnds = cat JNone
               , ocpSh = \_ _ -> cat JNone
               , ocpShBnds = cat JNone
               }

pathc0 :: Floating a => CX a -> z a -> U a -> p a -> None a -> a -> None a
pathc0 _ _ _ _ _ _ = None

mayer0 :: Floating a => a -> CX a -> CX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer0 _ _ _ _ _ = 0

lagrange0 ::  Floating a => a -> CX a -> None a -> U a -> None a -> None a -> a -> a
lagrange0 df (CX d _ _ _ _) _ (U u) _ _ _ = 100*(u)**2+1000*(d-df)**2

xbnd :: CX (Maybe Double, Maybe Double)
xbnd = CX { deltaDot = (Just 0, Just 10)
          , alpha = (Just (-pi/2), Just (pi/2))
          , alphaDot = (Nothing, Nothing)
          , beta = (Just (-pi/2), Just (pi/2))
          , betaDot = (Nothing, Nothing)
          }

ubnd :: U (Maybe Double, Maybe Double)
ubnd = U (Just (-1), Just 1) 

bc0 :: Floating a => CX a -> CX a -> CX a
bc0 (CX d0 alp0 alpd0 b0 bd0) _ = CX (d0-0) (alp0-pi/2) (alpd0-0) (b0-0) (bd0-0)
  
unpack :: (Floating a, Vectorize x, Vectorize z, Vectorize u, Vectorize p, View s, TV.Dim n, TV.Dim deg)
         => J (CollTraj x z u p s n deg) (V.Vector Double) -> x a
unpack ct = fmap realToFrac xf
 where
   xf = fmap V.head (unJV (split xf'))
   CollTraj _ _ _ _ xf' = split ct

initialGuess :: Double -> CX Double -> CollTraj CX None U None JNone NCollStages CollDeg (V.Vector Double)
initialGuess tf (CX df af adf bf bdf) = makeGuess tf guessX (\_ ->  None) guessU None
  where
    guessX = (\_ -> CX { deltaDot = df 
                       , alpha = af
                       , alphaDot = adf
                       , beta = bf
                       , betaDot = bdf
                       })
    guessU = (\_ -> U $ 0) --0.5*( 1/(1+exp(-10*t+30)) - 1/(1+exp(-10*t+50)) ) + 0.5*( 1/(1+exp(-10*t+130)) - 1/(1+exp(-10*t+110)) ) )

kp :: (Floating a) => a
kp = 0

kt :: (Floating a) => a
kt = 0

main :: IO ()
main = do
  putStrLn $ "using ip \""++gliderUrl++"\""
  putStrLn $ "using channel \""++gliderChannelName++"\""
  let guess = cat $ initialGuess 40 $ CX 1.5 (pi/2) 0 0 0
  withCallback gliderUrl gliderChannelName $ \cb -> do
      let cb' :: J (CollTraj CX None U None JNone NCollStages CollDeg) (V.Vector Double) -> IO Bool
          cb' traj = cb (ctToDynamic traj, toMeta traj)

      res <- steadyState cb' 1.5 0.1 ([],[],[]) guess 60
      print res

steadyState :: (J (CollTraj CX None U None JNone NCollStages CollDeg) (V.Vector Double) -> IO Bool)
               -> Double -> Double -> ([Double], [Double], [Double]) 
               -> J (CollTraj CX None U None JNone NCollStages CollDeg) (V.Vector Double)
               -> Int
               -> IO ([Double], [Double], [Double])
steadyState _ _ _ res _ 0 = return res
steadyState cb' d step (d0,a0,b0) guess count = do
  nlp0 <- makeCollNlp $ ocp mayer0 (lagrange0 (realToFrac d)) daeM bc0 pathc0 (Just 40, Just 40)

  (msg,opt') <- solveNlp' ipoptSolver (nlp0 { nlpX0' = guess }) (Just cb')
  opt <- case msg of Left msg' -> error msg'
                     Right _ -> return opt'
  let steady1 = (xOpt' opt) :: J (CollTraj CX None U None JNone NCollStages CollDeg) (V.Vector Double)
      CX _ a1 _ b1 _ = unpack steady1
      guess' = cat $ initialGuess 40 $ CX (d+step) a1 0 b1 0
      temp = (d:d0,a1:a0,b1:b0)
  res <- steadyState cb' (d+step) step temp guess' (count-1)
  return res
