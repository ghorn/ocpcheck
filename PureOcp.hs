{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language InstanceSigs #-}


module PureOcp ( bestProbability , createFeas , Ocp(..) , FeasibleOcp(..) , createFeas'' ) where

import Linear.V ( Dim(..) )
import Data.Proxy ( Proxy(..) )
import Dyno.Nats
import Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import Test.QuickCheck.Gen ( Gen(..), suchThat )
import Test.QuickCheck.Random
import System.Random
import Data.Packed.Matrix
import Data.MemoTrie ( memo2 )

import Numeric.LinearAlgebra
--import Numeric.LinearAlgebra.Algorithms ( rank )

createBAB' :: Int -> Matrix Double -> Matrix Double -> Matrix Double
createBAB' 1  _ matb = matb
createBAB' nn mata matb = matAB
  where
    matAAB = createBAB' (nn-1) mata (mata <> matb)
    matAB = fromLists $ zipWith (++) (toLists matb) (toLists matAAB)

createSparse :: Double -> [Double]
createSparse p = sparse
  where
    probs = pureRandoms $ suchThat (arbitrary :: Gen Double) (\w -> w <= 1 && w > 0)
    mat = pureRandoms $ suchThat (arbitrary :: Gen Double) (0 /=)
    sparse = zipWith (\pp alp -> if pp < p then alp else 0) probs mat

createSparse' :: Int -> Double -> [Double]
createSparse' s p = sparse
  where
    probs = pureRandoms'' s $ suchThat (arbitrary :: Gen Double) (\w -> w <= 1 && w > 0)
    mat = pureRandoms'' s $ suchThat (arbitrary :: Gen Double) (0 /=)
    sparse = zipWith (\pp alp -> if pp < p then alp else 0) probs mat

getProb :: Int -> Int -> Double
getProb n m = testProb n m 0
  where
    testProb :: Int -> Int -> Double -> Double
    testProb n1 m1 p 
      | p == 1 = p
      | countF < countI = testProb n1 m1 (p+0.01)
      | otherwise = p
      where
        (countF, countI) = manyTries n m 1000 sparse 
        sparse = createSparse p

    manyTries :: Int -> Int -> Int -> [Double] -> (Double, Double)
    manyTries _ _ 0 _ = (0,0)
    manyTries n2 m2 nb sparse
      | rank (createBAB' n2 ((n2><n2) ma) ((n2><m2) mb)) == n2 = (tempF+1, tempI)
      | otherwise = (tempF, tempI+1)
      where
        (tempF, tempI) = manyTries n2 m2 (nb-1) rest
        (ma,ra) = splitAt (n2*n2) $ sparse
        (mb, rest) = splitAt (n2*m2) ra
        

bestProbability :: Int -> Int -> Double
bestProbability = memo2 getProb 


test :: [(Double,Double,Double)]
test = [(n,m,p) | (n,m,p) <- zipWith (\(x,y) z -> (x,y,z)) [(x,y) | x <- [1..4], y<-[1..4]] (map (\(x,y) -> bestProbability x y) [(x,y) | x <- [1..4], y<-[1..4]]) ]

pureRandoms :: Gen a -> [a]
pureRandoms = pureRandoms' (mkQCGen 0) 2
  where
    pureRandoms' :: QCGen -> Int -> Gen a -> [a]
    pureRandoms' qcgen0 seed gen = unGen gen qcgen0 seed : pureRandoms' qcgen1 seed gen
      where
        qcgen1 = snd $ next qcgen0
-- Choose the seed you use
pureRandoms'' :: Int -> Gen a -> [a]
pureRandoms'' s = pureRandoms' (mkQCGen 0) s
  where
    pureRandoms' :: QCGen -> Int -> Gen a -> [a]
    pureRandoms' qcgen0 seed gen = unGen gen qcgen0 seed : pureRandoms' qcgen1 seed gen
      where
        qcgen1 = snd $ next qcgen0

-- real OCP dx/dt = a x + b u
data Ocp n m = Ocp { a :: [Double]                   -- force 
               , b :: [Double]                   -- force on control
               , xInit :: [Double]                  -- Starting point
               , xFinal :: [Double]                  -- Ending point
               } deriving (Show, Eq)
data FeasibleOcp n m  = FeasibleOcp (Ocp n m) deriving Show
data InfeasibleOcp n m  = InfeasibleOcp (Ocp n m) deriving Show

createFeas :: forall n m . (Dim n, Dim m) => FeasibleOcp n m
createFeas = focp 
  where
    n = reflectDim (Proxy :: Proxy n)
    m = reflectDim (Proxy :: Proxy m)
    p = bestProbability n m
    sparse = createSparse p
    x = replicate n 0
    xF = take n $ pureRandoms (arbitrary :: Gen Double)
    (ma,mb) = createFeas' sparse
    focp = FeasibleOcp $ Ocp ma mb x xF
      
    createFeas' :: [Double] -> ([Double], [Double])
    createFeas' sp 
      | rank (createBAB' n ((n><n) ma) ((n><m) mb)) == n = (ma,mb)
      | otherwise = createFeas' inCase
        where
          (ma, rest)  = splitAt  (n*n) sp
          (mb, inCase) = splitAt (n*m) rest

createInfeas :: forall n m . (Dim n, Dim m) => InfeasibleOcp n m
createInfeas = iocp 
  where
    n = reflectDim (Proxy :: Proxy n)
    m = reflectDim (Proxy :: Proxy m)
    p = bestProbability n m
    sparse = createSparse p
    x = replicate n 0
    xF = take n $ pureRandoms (arbitrary :: Gen Double)
    (ma,mb) = createInfeas' sparse
    iocp = InfeasibleOcp $ Ocp ma mb x xF
      
    createInfeas' :: [Double] -> ([Double], [Double])
    createInfeas' sp 
      | rank (createBAB' n ((n><n) ma) ((n><m) mb)) < n = (ma,mb)
      | otherwise = createInfeas' inCase
        where
          (ma, rest)  = splitAt  (n*n) sp
          (mb, inCase) = splitAt (n*m) rest

createFeas'' :: forall n m . (Dim n, Dim m) => Int -> Ocp n m
createFeas'' s = focp 
  where
    n = reflectDim (Proxy :: Proxy n)
    m = reflectDim (Proxy :: Proxy m)
    p = bestProbability n m
    sparse = createSparse' s p
    x = replicate n 0
    xF = take n $ pureRandoms'' s (arbitrary :: Gen Double)
    (ma,mb) = createFeas' sparse
    focp = Ocp ma mb x xF
      
    createFeas' :: [Double] -> ([Double], [Double])
    createFeas' sp 
      | rank (createBAB' n ((n><n) ma) ((n><m) mb)) == n = (ma,mb)
      | otherwise = createFeas' inCase
        where
          (ma, rest)  = splitAt  (n*n) sp
          (mb, inCase) = splitAt (n*m) rest

createInfeas'' :: forall n m . (Dim n, Dim m) => Int -> Ocp n m
createInfeas'' s = iocp 
  where
    n = reflectDim (Proxy :: Proxy n)
    m = reflectDim (Proxy :: Proxy m)
    p = bestProbability n m
    sparse = createSparse' s p
    x = replicate n 0
    xF = take n $ pureRandoms'' s (arbitrary :: Gen Double)
    (ma,mb) = createInfeas' sparse
    iocp = Ocp ma mb x xF
      
    createInfeas' :: [Double] -> ([Double], [Double])
    createInfeas' sp 
      | rank (createBAB' n ((n><n) ma) ((n><m) mb)) < n = (ma,mb)
      | otherwise = createInfeas' inCase
        where
          (ma, rest)  = splitAt  (n*n) sp
          (mb, inCase) = splitAt (n*m) rest
