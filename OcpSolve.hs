{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}

module OcpSolve ( main ) where

import Data.Vector ( Vector )
import Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import qualified Data.Foldable as F
import Linear

import Dyno.Vectorize
import Dyno.View
import Dyno.Ipopt
--import Dyno.Casadi.SXElement
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Ocp
import Dyno.DirectCollocation
import Dyno.Cov
import Dyno.Nats
import Dyno.TypeVecs

import PureOcp ( ItsAnOcp(..), Ocp(..) , FeasibleOcp(..), runGenWithSeed )

data Bcs n a = Bcs (Vec n a) (Vec n a) deriving (Functor, Generic1, Show)
data X n a = X (Vec n a) deriving (Functor, Generic1, Show)
data U m a = U (Vec m a) deriving (Functor, Generic1, Show)

instance Dim n => Vectorize (X n)
instance Dim m => Vectorize (U m)
instance Dim n => Vectorize (Bcs n)

mayer :: Floating a => a -> x -> x -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer _ _ _ _ _ = 0

lagrange :: Floating a => X n a -> None a -> U m a -> None a -> None a -> a -> a
lagrange _ _ (U vec) _ _ _ = F.sum $ fmap (\x -> x*x) vec

myDae :: forall n m a . (Dim n, Dim m, Floating a)
         => Ocp n m -> Dae (X n) None (U m) None (X n) None a
myDae myOcp (X x') (X x) _ (U u) _ _ = (X (x' ^-^ force), None)
  where
    a' :: Vec n (Vec n a)
    a' = fmap (fmap realToFrac) (a myOcp)

    b' :: Vec n (Vec m a)
    b' = fmap (fmap realToFrac) (b myOcp)

    force :: Vec n a
    force = (a' !* x)  ^+^  (b' !* u)

bc :: forall n m a . (Dim n, Floating a) => Ocp n m -> X n a -> X n a -> Bcs n a
bc myOcp (X x0) (X xF) = Bcs
                         (x0 ^-^ x0')
                         (xF ^-^ xF')
  where
    x0',xF' :: Vec n a
    x0' = fmap realToFrac $ xInit myOcp
    xF' = fmap realToFrac $ xFinal myOcp

pathc :: x a -> z a -> u a -> p a -> None a -> a -> None a
pathc _ _ _ _ _ _ = None

anOcp :: FeasibleOcp D3 D2
anOcp = runGenWithSeed 42 arbitrary

toOcpPhase :: (Dim n, Dim m, ItsAnOcp a n m)
              => a -> OcpPhase (X n) None (U m) None (X n) None (Bcs n) None JNone JNone JNone
toOcpPhase myOcp' =
  OcpPhase { ocpMayer = mayer
           , ocpLagrange = lagrange
           , ocpDae = myDae myOcp
           , ocpBc = bc myOcp
           , ocpPathC = pathc
           , ocpPathCBnds = None
           , ocpBcBnds = fill (Just 0, Just 0)
           , ocpXbnd = fill (Nothing, Nothing)
           , ocpUbnd = fill (Nothing, Nothing)
           , ocpZbnd = None
           , ocpPbnd = None
           , ocpTbnd = (Just 4, Just 4)

           , ocpSq = 0
           , ocpSbnd = jfill (Nothing,Nothing)
           , ocpSbc = \_ _ -> cat JNone
           , ocpSbcBnds = cat JNone
           , ocpSh = \_ _ -> cat JNone
           , ocpShBnds = cat JNone
           }
  where
    myOcp = getOcp myOcp'

mySolver :: NlpSolverStuff IpoptSolver
mySolver = ipoptSolver { options = [("max_iter", Opt (100 :: Int))] }

type DimN = D3
type DimM = D2

main :: IO()
main = do
  putStrLn "What seed ?"
  seed <- getLine
  let s = read seed
      myocp = runGenWithSeed s arbitrary :: FeasibleOcp DimN DimM
      --ocptest = Ocp [1,1,1,0,1,0,0,0,1] [1,0,2,3,-1,-1] [0,0,0] [1,2,1]
      guess = jfill 0 :: J (CollTraj (X DimN) None (U DimM) None JNone D100 D3) (Vector Double)
      --cb' :: J (CollTraj X None U None JNone D100 D2) (Vector Double) -> IO Bool
      --cb' traj = cb (ctToDynamic traj, toMeta traj)
  nlp <- makeCollNlp $ toOcpPhase myocp

  (msg,opt') <- solveNlp' mySolver (nlp { nlpX0' = guess }) Nothing
  print myocp
  _ <- case msg of Left msg' -> error msg'
                   Right _ -> return opt'
  return ()
