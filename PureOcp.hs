{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}

module PureOcp ( Ocp(..), FeasibleOcp(..), InfeasibleOcp(..)
               , ItsAnOcp(..)
               , runGenWithSeed
               ) where

import Control.Monad ( when )
import qualified Data.Foldable as F
import Linear.V ( Dim(..), reifyDim )
import Data.Proxy ( Proxy(..) )
import Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import Test.QuickCheck.Gen ( Gen(..), choose, vectorOf )
import Test.QuickCheck.Random
import Data.Packed.Matrix
import Data.MemoTrie ( memo2 )

import Numeric.LinearAlgebra

import Dyno.TypeVecs hiding ( reifyDim )

--import Dyno.Nats

reasonableLimit :: (Double, Double)
reasonableLimit = (-1e6, 1e6)

-- real OCP dx/dt = A x + B u
data Ocp n m = Ocp { a :: Vec n (Vec n Double) -- A
                   , b :: Vec n (Vec m Double) -- B
                   , xInit :: Vec n Double  -- Starting point
                   , xFinal :: Vec n Double -- Ending point
                   } deriving (Show, Eq)
data FeasibleOcp n m  = FeasibleOcp (Ocp n m) deriving Show
data InfeasibleOcp n m  = InfeasibleOcp (Ocp n m) deriving Show

class ItsAnOcp a n m where
  getOcp :: a -> Ocp n m
instance ItsAnOcp (Ocp n m) n m where
  getOcp = id
instance ItsAnOcp (FeasibleOcp n m) n m where
  getOcp (FeasibleOcp ocp) = ocp
instance ItsAnOcp (InfeasibleOcp n m) n m where
  getOcp (InfeasibleOcp ocp) = ocp

genVec :: forall n a . Dim n => Gen a -> Gen (Vec n a)
genVec gen = do
  let n = reflectDim (Proxy :: Proxy n)
  xs <- vectorOf n gen
  return (mkVec' xs)

genVecs :: forall n m a . (Dim n, Dim m) => Gen a -> Gen (Vec n (Vec m a))
genVecs gen = genVec (genVec gen)

createOcpWithProb :: forall n m . (Dim n, Dim m) => Double -> Gen (Ocp n m)
createOcpWithProb prob = do
  let n = reflectDim (Proxy :: Proxy n)
      --m = reflectDim (Proxy :: Proxy m)
      x0 = replicate n 0
  as <- genVecs (sparseDouble prob)
  bs <- genVecs (sparseDouble prob)
  xF <- vectorOf n (choose reasonableLimit)
  return $ Ocp as bs (mkVec' x0) (mkVec' xF)

instance (Dim n, Dim m) => Arbitrary (FeasibleOcp n m) where
  arbitrary = do
    let prob = bestProbability (Proxy :: Proxy n) (Proxy :: Proxy m)
    ocp <- createOcpWithProb prob
    if controllable ocp
      then return (FeasibleOcp ocp)
      else arbitrary

instance (Dim n, Dim m) => Arbitrary (InfeasibleOcp n m) where
  arbitrary = do
    let prob = bestProbability (Proxy :: Proxy n) (Proxy :: Proxy m)
    ocp <- createOcpWithProb prob
    if controllable ocp
      then arbitrary
      else return (InfeasibleOcp ocp)


controllable :: forall n m . (Dim n, Dim m) => Ocp n m -> Bool
controllable ocp = rank bab == n
  where
    ma = concat $ fmap F.toList $ F.toList $ a ocp
    mb = concat $ fmap F.toList $ F.toList $ b ocp
    n = reflectDim (Proxy :: Proxy n)
    m = reflectDim (Proxy :: Proxy m)
    bab = createBAB n ((n><n) ma) ((n><m) mb)

createBAB :: Int -> Matrix Double -> Matrix Double -> Matrix Double
createBAB 1  _ matb = matb
createBAB nn mata matb = matAB
  where
    matAAB = createBAB (nn-1) mata (mata <> matb)
    matAB = fromLists $ zipWith (++) (toLists matb) (toLists matAAB)


-- create a double which is non-zero with some given probability
sparseDouble :: Double -> Gen Double
sparseDouble prob = do
  testProb <- choose (0,1) :: Gen Double
  if testProb <= prob
    then do
      nz <- choose reasonableLimit :: Gen Double
      if abs nz >= 1e-12
        then return nz
        else sparseDouble prob
    else return 0

getProb'' :: forall n m . (Dim n, Dim m) => Double -> Int -> Proxy n -> Proxy m -> Gen Double
getProb'' prob numRuns pn pm = do
  when (prob > 1.0) $ error "getProbs': prob > 1.0"
  ocps <- vectorOf numRuns (createOcpWithProb prob) :: Gen [Ocp n m]
  let fractionControllable = (fromIntegral (length (filter controllable ocps))) / (fromIntegral numRuns :: Double)
  if fractionControllable >= 0.5
    then return prob
    else getProb'' (prob + 0.01) numRuns pn pm

getProb' :: Int -> Int -> Gen Double
getProb' n m = reifyDim m $ reifyDim n $ getProb'' 0.0 numRuns
  where
    numRuns = 100

getProb :: Int -> Int -> Double
getProb n m = runGenWithSeed 42 (getProb' n m)

bestProbability' :: Int -> Int -> Double
bestProbability' = memo2 getProb

bestProbability :: (Dim n, Dim m) => Proxy n -> Proxy m -> Double
bestProbability pn pm = bestProbability' (reflectDim pn) (reflectDim pm)

runGenWithSeed :: Int -> Gen a -> a
runGenWithSeed k gen = (unGen gen) (mkQCGen k) k
