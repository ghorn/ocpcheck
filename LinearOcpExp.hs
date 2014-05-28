{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}

module LinearOcpExp
       ( LinearOcp(..), IsLinearOcp(..)
       , FeasibleLinearOcp(..), InfeasibleLinearOcp(..)
       , runGenWithSeed
       , roughPrint, fromListsToOcp
       ) where

import qualified Data.Foldable as F
import qualified Data.Vector as V
import Linear.V ( Dim(..) )
--import Data.Proxy ( Proxy(..) )
--import Data.List ( transpose )
import Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import Test.QuickCheck.Gen ( Gen(..), choose, vectorOf, infiniteListOf, sample )
import Test.QuickCheck.Random
import Data.Packed.Matrix
--import Data.MemoTrie ( memo2 )

import Numeric.LinearAlgebra

import Dyno.Vectorize
import Dyno.TypeVecs hiding ( reifyDim )
import Dyno.Nats

--reasonableLimit :: (Double, Double)
--reasonableLimit = (-1e3, 1e3)

maximumTime :: Double
maximumTime = 1.0

-- real OCP dx/dt = A x + B u
data LinearOcp n m =
  LinearOcp
  { loA :: Vec n (Vec n Double) -- A
  , loB :: Vec n (Vec m Double) -- B
  , loXb :: Vec n (Double, Double) -- Bounds for x
  , loUb :: Vec m (Double, Double) -- Bounds for u
  , loX0 :: Vec n Double        -- Starting point
  , loXF :: Vec n Double        -- Ending point
  , loT :: Double               -- Final time
  } deriving (Eq)
data FeasibleLinearOcp n m  = FeasibleLinearOcp (LinearOcp n m) 
data InfeasibleLinearOcp n m  = InfeasibleLinearOcp (LinearOcp n m) 

instance (Dim n, Dim m) => Show (LinearOcp n m) where
  show = prettyPrint
instance (Dim n, Dim m) => Show (FeasibleLinearOcp n m) where
  show (FeasibleLinearOcp ocp ) = prettyPrint ocp
instance (Dim n, Dim m) => Show (InfeasibleLinearOcp n m) where
  show (InfeasibleLinearOcp ocp )= prettyPrint ocp

class IsLinearOcp a n m | a -> n, a -> m where
  getLinearOcp :: a -> LinearOcp n m
instance IsLinearOcp (LinearOcp n m) n m where
  getLinearOcp = id
instance IsLinearOcp (FeasibleLinearOcp n m) n m where
  getLinearOcp (FeasibleLinearOcp ocp) = ocp
instance IsLinearOcp (InfeasibleLinearOcp n m) n m where
  getLinearOcp (InfeasibleLinearOcp ocp) = ocp

fromListsToOcp :: (Dim n, Dim m) => [[Double]] -> [[Double]] -> [(Double, Double)] -> [(Double, Double)] -> [Double] -> [Double] -> Double -> LinearOcp n m
fromListsToOcp listA listB listX listU listX0 listXf time = LinearOcp matA matB xB uB x0 xf time
  where
    matA = devectorize $ V.fromList $ map (devectorize . V.fromList) listA
    matB = devectorize $ V.fromList $ map (devectorize . V.fromList) listB
    xB = devectorize $ V.fromList listX
    uB = devectorize $ V.fromList listU
    x0 = devectorize $ V.fromList listX0
    xf = devectorize $ V.fromList listXf

genVec :: forall n a . Dim n => Gen a -> Gen (Vec n a)
genVec gen = do
  let n = reflectDim (Proxy :: Proxy n)
  xs <- vectorOf n gen
  return (mkVec' xs)

genVecs :: forall n m a . (Dim n, Dim m) => Gen a -> Gen (Vec n (Vec m a))
genVecs gen = genVec (genVec gen)

smallIntAsDouble :: Gen Double
smallIntAsDouble = do
  k <- choose (-3,3::Int)
  return $ realToFrac k


createLinearOcpWithoutBounds :: forall n m . (Dim n, Dim m) => Gen (LinearOcp n m)
createLinearOcpWithoutBounds = do
  let n = reflectDim (Proxy :: Proxy n)
      nn = floor $ (realToFrac n :: Double) / 2
  ps <- genVecs (smallIntAsDouble) :: Gen (Vec n (Vec n Double))
  p <- choose (0,nn)
  r <- choose (0,n-1)
  lambda <- vectorOf (n-2*p) smallIntAsDouble
  a <- vectorOf p smallIntAsDouble
  b <- vectorOf p smallIntAsDouble 
  bs <- genVecs (smallIntAsDouble)
  xo <- genVec (smallIntAsDouble)
  let upA = map (max 2) $ map (\x -> 2 * exp(maximumTime * x)) $ take (2*p) $ cycle a
      upL = map (max 1) $ map (\x -> exp(maximumTime * x)) lambda
      matP = vecToMat ps
      matS = createS a b lambda
      matN = createN r n
      matA = inv matP <> (matS + matN) <> matP
      boundsA = devectorize $ V.fromList $ map (\x -> (-x,x)) $ upA ++ upL
      bdsA = matTimesBounds (matToVec (inv matP)) $ boundsTimeMat boundsA ps
  if rank matP == n && rank matA == n 
  then  return $ LinearOcp (matToVec matA) bs bdsA undefined xo undefined undefined
  else createLinearOcpWithoutBounds

createN :: Int -> Int -> Matrix Double
createN r n = (n><n) $ top ++ bottom
  where
    top = replicate (n*(n-r)) 0
    bottom = if r > 0 
             then concat $ toLists $ diagRect 0 (fromList (replicate r 1)) r n
             else []

createS :: [Double] -> [Double] -> [Double] -> Matrix Double
createS a b lambda 
  | lambda == [] = diagBlock top
  | otherwise = diagBlock $ top ++ [bottom]
  where
    top = zipWith (\x y -> (2><2) [x,-y,y,x]) a b
    bottom = diag $ fromList lambda


generateStates :: forall n m . (Dim n, Dim m) => LinearOcp n m -> Gen [(Vec n Double, Vec m Double, Double)]
generateStates ocp = do
  time' <- infiniteListOf $ choose (0,maximumTime)
  controls <- infiniteListOf $ genVec smallIntAsDouble 
  end <- choose (2,5)
  let matA = vecToMat $ loA ocp
      matB = vecToMat $ loB ocp
      time = map (\x -> realToFrac (floor (10*x+1) ::Int) / 10) time'
      n = reflectDim (Proxy :: Proxy n)
      nextStep x u t = matToCol x1
        where
          expAt = expm $ scale t matA
          x1 = expAt <> colToMat x + inv matA <> (expAt - diag (fromList (replicate n 1))) <> matB <> colToMat u  

      allSteps :: (Dim n, Dim m) => Vec n Double -> [Vec m Double] -> [Double] -> [(Vec n Double, Vec m Double, Double)]
      allSteps start control steps = (start, head control, head steps) : allSteps next (tail control) (tail steps)
        where
          next = nextStep start (head control) (head steps)
        
  return $ take end $ allSteps (loX0 ocp) controls time   

wrapInBounds :: (Dim n) => [Vec n Double] -> Vec n (Double, Double)  
wrapInBounds list = tvzipWith (\x y -> (x*0.9, y*1.1)) minV maxV
  where
    minV = fmap (realToFrac . (floor :: Double -> Int)) $ foldl1 (tvzipWith min) list
    maxV = fmap (realToFrac . (ceiling :: Double -> Int)) $ foldl1 (tvzipWith max) list

boundsForState :: (Dim n, Dim m) => [(Vec n Double, Vec m Double, Double)] -> Vec n (Double, Double) -> LinearOcp n m -> Vec n (Double, Double)
boundsForState states bdsA ocp = fmap (\(x,y) -> (realToFrac (floor x :: Int), realToFrac (ceiling y :: Int))) temp
  where
    matA = inv $ vecToMat $ loA ocp
    matB = vecToMat $ loB ocp
    abu = map (\(_,u,_) -> matToCol $ matA <> matB <> colToMat u) states
    bounds = zipWith (\(x,_,_) u -> tvzipWith (\(l,h) y -> (l-y,h-y)) (boundsTimeVec' bdsA (tvzipWith (+) x u)) u)
                     states
                     abu
    temp = foldl1 (tvzipWith (\(x,y) (l,u) -> (min x l, max y u))) bounds

boundsTimeVec' :: (Dim n) => Vec n (Double, Double) -> Vec n Double -> Vec n (Double, Double)    
boundsTimeVec' bounds mat = fmap (boundsTimeVec mat) bounds

boundsTimeVec :: (Dim n ) => Vec n Double -> (Double, Double) -> (Double, Double)
boundsTimeVec mat bounds = (minv, maxv)
  where
    minv = sum $ fmap (\x -> min (fst bounds * x) (snd bounds * x)) $ V.toList $ vectorize mat
    maxv = sum $ fmap (\x -> max (fst bounds * x) (snd bounds * x)) $ V.toList $ vectorize mat

boundsTimeMat :: (Dim n) => Vec n (Double, Double) -> Vec n (Vec n Double) -> Vec n (Double, Double)
boundsTimeMat bounds mat = F.foldl1 (tvzipWith (\(x,y) (l,u) -> (min x l, max y u))) temp
  where
    temp = fmap (boundsTimeVec' bounds) $ tvtranspose mat

vecTimesBounds :: (Dim n) => Vec n (Double, Double) -> Vec n Double -> (Double, Double)
vecTimesBounds bounds vec = (minv, maxv)
  where
    minv = sum $ V.toList $ vectorize $ tvzipWith (\x (l,u) -> min (x*l) (x*u)) vec bounds
    maxv = sum $ V.toList $ vectorize $ tvzipWith (\x (l,u) -> max (x*l) (x*u)) vec bounds

matTimesBounds :: (Dim n) => Vec n (Vec n Double) -> Vec n (Double, Double) -> Vec n (Double, Double)
matTimesBounds mat bounds = fmap (vecTimesBounds bounds) mat

vecToMat :: (Dim n, Dim m) => Vec n (Vec m Double) -> Matrix Double
vecToMat vec = fromLists $ V.toList $ vectorize $ fmap (V.toList . vectorize) vec

matToVec :: (Dim n, Dim m) => Matrix Double -> Vec n (Vec m Double)
matToVec mat = mkVec' $ map mkVec' $ toLists mat

colToMat :: (Dim n) => Vec n Double -> Matrix Double
colToMat col =  fromLists $ V.toList $ vectorize $ fmap (:[]) col

matToCol :: (Dim n) =>  Matrix Double -> Vec n Double 
matToCol mat = mkVec' $ concat $ toLists mat	

makeLastStateNice :: (Dim n) => [Vec n Double] -> Vec n Double
makeLastStateNice states = xf
  where
    xF' = last states
    xbl' = last $ init states
    xf = tvzipWith makeInt xF' xbl'
      where
        makeInt xF xbl
          | xbl <= xF = realToFrac (floor (10*xF) :: Int) / 10
          | xbl >= xF = realToFrac (ceiling (10*xF) :: Int) / 10
          | otherwise = error "impossible to make nice"
           

instance (Dim n, Dim m) => Arbitrary (FeasibleLinearOcp n m) where
  arbitrary = do
    ocp <- createLinearOcpWithoutBounds
    states <- generateStates ocp
    let xB = boundsForState states (loXb ocp) ocp
        uB = wrapInBounds $ map (\(_,u,_) -> u) $ states
        xf = makeLastStateNice $ map (\(x,_,_) -> x) states
        time = sum $ map (\(_,_,t) -> t) $ init states
    if definitelyControllable ocp
      then return $ FeasibleLinearOcp $ LinearOcp (loA ocp) (loB ocp) xB uB (loX0 ocp) xf time
      else arbitrary


instance (Dim n, Dim m) => Arbitrary (InfeasibleLinearOcp n m) where
  arbitrary = do
    --let prob = bestProbability (Proxy :: Proxy n) (Proxy :: Proxy m)
    ocp <- createLinearOcpWithoutBounds
    if definitelyUncontrollable ocp
      then return (InfeasibleLinearOcp ocp)
      else arbitrary

-- full rank and well conditioned
definitelyControllable :: forall n m . (Dim n, Dim m) => LinearOcp n m -> Bool
definitelyControllable ocp = rank bab == n && rcond bab >= 1e-11
  where
    n = reflectDim (Proxy :: Proxy n)
    bab = createBAB ocp

-- not full rank
definitelyUncontrollable :: forall n m . (Dim n, Dim m) => LinearOcp n m -> Bool
definitelyUncontrollable ocp = rank bab < n
  where
    n = reflectDim (Proxy :: Proxy n)
    bab = createBAB ocp

createBAB :: forall n m . (Dim n, Dim m) => LinearOcp n m -> Matrix Double
createBAB ocp = bab
  where
    ma = concat $ fmap F.toList $ F.toList $ loA ocp
    mb = concat $ fmap F.toList $ F.toList $ loB ocp
    n = reflectDim (Proxy :: Proxy n)
    m = reflectDim (Proxy :: Proxy m)
    bab = createBAB' n ((n><n) ma) ((n><m) mb)

createBAB' :: Int -> Matrix Double -> Matrix Double -> Matrix Double
createBAB' 1  _ matb = matb
createBAB' nn mata matb = matAB
  where
    matAAB = createBAB' (nn-1) mata (mata <> matb)
    matAB = fromLists $ zipWith (++) (toLists matb) (toLists matAAB)


runGenWithSeed :: Int -> Gen a -> a
runGenWithSeed k gen = (unGen gen) (mkQCGen k) k

prettyPrint :: (Dim n, Dim m) => LinearOcp n m -> String
prettyPrint ocp = text
  where
    matA = "\nA = " ++ show ( vecToMat (loA ocp))
    matB = "\nB = " ++ show ( vecToMat (loB ocp))
    matXb = "\nXb = " ++ show ( F.toList (loXb ocp))
    matUb = "\nUb = " ++ show ( F.toList (loUb ocp))
    matXo = "\nxo = " ++ show ( F.toList (loX0 ocp))
    matXf = "\nxf = " ++ show ( F.toList (loXF ocp))
    time = "\nT = " ++ show (loT ocp)
    (multBU, _) = eig $ vecToMat (loA ocp)
    multU = "\nEig = " ++ show multBU
    text = matA ++ matB ++ matXb ++ matUb ++ matXo ++ matXf ++ time ++ multU ++ "\n"

roughPrint :: (Dim n, Dim m, IsLinearOcp a n m) => a -> String
roughPrint ocp = text
  where
    myOcp = getLinearOcp ocp
    matA = V.toList $ vectorize $ fmap (V.toList . vectorize) $ loA myOcp
    matB = V.toList $ vectorize $ fmap (V.toList . vectorize) $ loB myOcp
    matX = V.toList $ vectorize $ loXb myOcp
    matU = V.toList $ vectorize $ loUb myOcp
    matXo = V.toList $ vectorize $ loX0 myOcp
    matXf = V.toList $ vectorize $ loXF myOcp
    text = "solveAlone (fromListsToOcp "++show matA ++" "++ show matB ++" "++ show matX ++" "++show matU ++" "++show matXo ++" "++show matXf ++" "++show (loT myOcp) ++ " :: LinearOcp D2 D1)\n"
