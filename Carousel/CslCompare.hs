{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language RankNTypes #-}

module CslCompare ( main ) where 

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

import CslDataBis

type NCollStages = D100
type CollDeg = D3

data BX a = BX (CX a) (CX a) deriving (Eq, Functor, Generic, Generic1, Show)
instance Vectorize BX
instance (Lookup a, Generic a) => Lookup (BX a)

daeM :: Floating a => Dae CX None U Cd CX None a
daeM x' x _ u c _ = (modelForces x' x u c, None)

ocp :: (Vectorize bc, Vectorize pc) =>
       (forall a . Floating a => a -> CX a -> CX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a)
       -> (forall a . Floating a => CX a -> None a -> U a -> Cd a -> None a -> a -> a)
       -> (forall a . Floating a => Dae CX None U Cd CX None a)
       -> (forall a . Floating a => CX a -> CX a -> bc a) 
       -> (forall a . Floating a => CX a -> None a -> U a -> Cd a -> None a -> a -> pc a)
       -> (forall a . Floating a => (Maybe a, Maybe a))
       -> OcpPhase CX None U Cd CX None bc pc JNone JNone JNone
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
               , ocpPbnd = fill (Just (-1), Just 1.6)
               , ocpTbnd = tbd

               , ocpSq = 0
               , ocpSbnd = jfill (Nothing,Nothing)
               , ocpSbc = \_ _ -> cat JNone
               , ocpSbcBnds = cat JNone
               , ocpSh = \_ _ -> cat JNone
               , ocpShBnds = cat JNone
               }

pathc0 :: Floating a => CX a -> z a -> U a -> Cd a -> None a -> a -> None a
pathc0 _ _ _ _ _ _ = None

mayer0 :: Floating a => a -> CX a -> CX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer0 _ _ _ _ _ = 0

lagrange0 :: Floating a => a -> a -> CX a -> None a -> U a -> Cd a -> None a -> a -> a
lagrange0 df aT (CX d af _ _ _) _ (U u) _ _ _ = 100*(u)**2+1000*(d-df)**2+ 10*(aT-af)**2

xbnd :: CX (Maybe Double, Maybe Double)
xbnd = CX { deltaDot = (Just 0, Just 5)
          , alpha = (Just (-pi/2), Just (pi/2))
          , alphaDot = (Nothing, Nothing)
          , beta = (Just (-pi/2), Just (pi/2))
          , betaDot = (Nothing, Nothing)
          }

ubnd :: U (Maybe Double, Maybe Double)
ubnd = U (Just (-1), Just 1)

bc0 :: Floating a => CX a -> CX a -> CX a
bc0 (CX d0 alp0 alpd0 b0 bd0) _ = CX (d0-0) (alp0-pi/2) (alpd0-0) (b0-0) (bd0-0)
  

unpackX :: (Floating a, Vectorize x, Vectorize z, Vectorize u, Vectorize p, View s, TV.Dim n, TV.Dim deg)
         => J (CollTraj x z u p s n deg) (V.Vector Double) -> x a
unpackX ct = fmap realToFrac xf
 where
   xf = fmap V.head (unJV (split xf'))
   CollTraj _ _ _ _ xf' = split ct

unpackP :: (Floating a, Vectorize x, Vectorize z, Vectorize u, Vectorize p, View s, TV.Dim n, TV.Dim deg)
         => J (CollTraj x z u p s n deg) (V.Vector Double) -> p a
unpackP ct = fmap realToFrac xf
 where
   xf = fmap V.head (unJV (split xf'))
   CollTraj _ _ xf' _ _ = split ct

initialGuess :: Double -> Double -> CX Double 
                -> CollTraj CX None U Cd JNone NCollStages CollDeg (V.Vector Double)
initialGuess tf cd (CX df af adf bf bdf) = makeGuess tf guessX (\_ ->  None) guessU (Cd cd)
  where
    guessX = (\_ -> CX { deltaDot = df 
                       , alpha = af
                       , alphaDot = adf
                       , beta = bf
                       , betaDot = bdf
                       })
    guessU = (\_ -> U $ 0)


main :: Double -> Double -> IO ()
main dstart dend = do
  putStrLn $ "using ip \""++gliderUrl++"\""
  putStrLn $ "using channel \""++gliderChannelName++"\""

  nlp0' <- makeCollNlp $ ocp mayer0 (lagrange0 1.55 0.87) daeM bc0 pathc0 (Just 40, Just 40)
  nlp0 <- makeCollNlp $ ocp mayer0 (lagrange0 (realToFrac dstart) 0.867) daeM bc0 pathc0 (Just 40, Just 40)
  nlp1 <- makeCollNlp $ ocp mayer0 (lagrange0 (realToFrac dend) 0.777) daeM bc0 pathc0 (Just 40, Just 40)

  withCallback gliderUrl gliderChannelName $ \cb -> do
    let guess = cat $ initialGuess 40 0.71 $ CX 1.55 (pi/2) 0 0 0
        cb' :: J (CollTraj CX None U Cd JNone NCollStages CollDeg) (V.Vector Double) -> IO Bool
        cb' traj = cb (ctToDynamic traj, toMeta traj)

    (msg,opt') <- solveNlp' ipoptSolver (nlp0' { nlpX0' = guess }) (Just cb')
    opt <- case msg of Left msg' -> error msg'
                       Right _ -> return opt'
    let steady = (xOpt' opt) :: J (CollTraj CX None U Cd JNone NCollStages CollDeg) (V.Vector Double)
        CX _ a0 _ b0 _ = unpackX steady
        Cd c0 = unpackP steady
        guess0 = cat $ initialGuess 40 c0 $ CX dstart a0 0 b0 0
    (msg0,opt0') <- solveNlp' ipoptSolver (nlp0 { nlpX0' = guess0 }) (Just cb')
    opt0 <- case msg0 of Left msg' -> error msg'
                         Right _ -> return opt0'

    let steady1 = (xOpt' opt0) :: J (CollTraj CX None U Cd JNone NCollStages CollDeg) (V.Vector Double)
        Cd c1 = unpackP steady1
        guess' = cat $ initialGuess 40 c0 $ CX dend a0 0 b0 0
    (msg1,opt1') <- solveNlp' ipoptSolver (nlp1 { nlpX0' = guess' }) (Just cb')
    opt1 <- case msg1 of Left msg' -> error msg'
                         Right _ -> return opt1'

    let steady2 = (xOpt' opt1) :: J (CollTraj CX None U Cd JNone NCollStages CollDeg) (V.Vector Double)
        Cd c2 = unpackP steady2

    print $ (unpackX steady1 :: CX Double)
    print $ (unpackX steady2 :: CX Double)
    print $ ((c0,c1,c2) :: (Double, Double,Double))

    return ()
