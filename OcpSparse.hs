{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language InstanceSigs #-}

module OcpSparse ( main, runManually ) where

import Linear.V (Dim(..))
import Test.QuickCheck ( quickCheck )
import Test.QuickCheck ( Args(..), quickCheckWith, stdArgs )
import Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import Test.QuickCheck.Gen
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework ( Test, defaultMain, testGroup )
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property

import qualified Dyno.OcpMonad as M
import Dyno.Ipopt
import Dyno.Casadi.SXElement ( SXElement )
import Dyno.NlpSolver 

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Algorithms (rank)

import Data.List (transpose)
import qualified Data.Vector as V
import Data.Proxy (Proxy (..))
import Dyno.Nats

import PureOcp ( bestProbability )


-- Functions to test the controllability matrix

createBAB' :: Int -> Mat n n -> Mat n m -> Matrix Double
createBAB' 1  _ (Mat matb) = matb
createBAB' nn (Mat mata) (Mat matb) = matAB
  where
    matAAB = createBAB' (nn-1) (Mat mata) (Mat (mata <> matb))
    matAB = fromLists $ zipWith (++) (toLists matb) (toLists matAAB)

testBAB :: forall n m . (Dim n, Dim m) => Ocp n m -> Matrix Double
testBAB ocp = bab
  where
    n = reflectDim (Proxy :: Proxy n)
    m = reflectDim (Proxy :: Proxy m)
    bab = createBAB' n (Mat ((n><n) (a ocp)) :: Mat n n) (Mat ((n><m) (b ocp)) :: Mat n m)

newtype Mat n m = Mat (Matrix Double) deriving Show

data MyOde n m = MyOde { matA :: Mat n n
                       , matB :: Mat n m
                       } deriving Show

instance (Dim n, Dim m) => Arbitrary (Mat n m)  where
  arbitrary :: Gen (Mat n m)
  arbitrary = do
    let n = reflectDim (Proxy :: Proxy n)
        m = reflectDim (Proxy :: Proxy m)
        p = bestProbability n m
    k1 <- infiniteListOf $ suchThat (arbitrary :: Gen Double) (\w -> w <= 1 && w > 0)
    k2 <- infiniteListOf $ suchThat (arbitrary :: Gen Double) (0 /=)
    let ks = zipWith (\pp alp -> if pp < p then alp else 0) k1 k2
    return $ Mat $ (n><m) ks
        
instance (Dim n, Dim m) => Arbitrary (MyOde n m) where
  arbitrary = do
    ma <- arbitrary
    mb <- arbitrary
    return (MyOde ma mb)

test :: (Dim n, Dim m) => Int -> MyOde n m -> Bool
test nn ode = rank k == nn
  where
    k = createBAB' nn (matA ode) (matB ode)


-- Create an Ode with control bounds to minimize
quietIpoptSolver :: NlpSolverStuff IpoptSolver
quietIpoptSolver =
  ipoptSolver { defaultOptions =
                   defaultOptions ipoptSolver ++
                   [ ("print_time", Opt False)
                   , ("print_level", Opt (0 :: Int))
                   ]
              }

-- real OCP dx/dt = a x + b u
data Ocp n m = Ocp { a :: [Double]                   -- force 
               , b :: [Double]                   -- force on control
               , x0 :: [Double]                  -- Starting point
               , xF :: [Double]                  -- Ending point
               } deriving (Show, Eq)
data FeasibleOcp n m  = FeasibleOcp (Ocp n m) deriving Show
data InfeasibleOcp n m  = InfeasibleOcp (Ocp n m) deriving Show

-- make it always feasible 
instance (Dim n, Dim m) => Arbitrary (FeasibleOcp n m) where
  arbitrary = do
    k1 <- infiniteListOf (suchThat (arbitrary :: Gen Double) (0 /=))
    let n = reflectDim (Proxy :: Proxy n)
        m = reflectDim (Proxy :: Proxy m)
        kF = take n k1
        x = replicate n 0
    ode <- arbitrary :: Gen (MyOde n m)
    let Mat ka = matA ode
        Mat kb = matB ode
        kka = concat $ toLists ka
        kkb = concat $ toLists kb
    if test n ode
    then return (FeasibleOcp Ocp {a = kka, b = kkb, x0 = x, xF = kF})
    else arbitrary

-- make it always infeasible 
instance (Dim n, Dim m) => Arbitrary (InfeasibleOcp n m) where
  arbitrary = do
    k1 <- infiniteListOf (suchThat (arbitrary :: Gen Double) (0 /=))
    let n = reflectDim (Proxy :: Proxy n)
        m = reflectDim (Proxy :: Proxy m)
        kF = take n k1
        x = replicate n 0
    ode <- arbitrary :: Gen (MyOde n m)
    let Mat ka = matA ode
        Mat kb = matB ode
        kka = concat $ toLists ka
        kkb = concat $ toLists kb
    if not $ test n ode
    then return (InfeasibleOcp Ocp {a = kka, b = kkb, x0 = x, xF = kF})
    else arbitrary


-- this is like "isSolveable"
canSolve :: (Dim n, Dim m) => Ocp n m -> IO (Bool)
canSolve ocp = do
  (status, _) <- M.solveStaticOcp quietIpoptSolver (ocpToDae ocp) (mayer ocp) (boundaryConditions ocp) (myOcp ocp) tbnds nN deg Nothing
  if status == Right "Solve_Succeeded"
  then return True
  else return False
  where
    nN = 100
    deg = 3
    tbnds = (Just 4, Just 4)

standAlone :: (Dim n, Dim m) => Ocp n m -> IO()
standAlone ocp = do
  feas <- M.solveStaticOcp ipoptSolver (ocpToDae ocp) (mayer ocp) (boundaryConditions ocp) (myOcp ocp) tbnds nN deg Nothing
  print feas
  where
    nN = 100
    deg = 3
    tbnds = (Just 4, Just 4)

canSolve' :: (Dim n, Dim m) => Ocp n m -> IO (Maybe Double)
canSolve' ocp = do
  (status, opt) <- M.solveStaticOcp quietIpoptSolver (ocpToDae ocp) (mayer ocp) (boundaryConditions ocp) (myOcp ocp) tbnds nN deg Nothing
  return $ case status of
    Right _ -> Just (V.head opt)
    Left _ -> Nothing
  where
    nN = 100
    deg = 3
    tbnds = (Just 4, Just 4)

--these are useful functions for the creation of solve arguments
unwrap :: (Monad m) => [m a] -> m [a]
unwrap [] = return []
unwrap xx = do
  yy <- head xx
  end <- unwrap (tail xx)
  return (yy:end)

createLin :: [SXElement] -> [Double] -> SXElement
createLin xx qq = foldl (+) 0 (zipWith (*) qS xx)
   where
     qS = map realToFrac qq :: [SXElement]

-- this is to create a DaeMonad from a Qp
ocpToDae :: forall n m . (Dim n, Dim m) => Ocp n m -> SXElement -> M.DaeMonad ()
ocpToDae ocp time = do
  let n = reflectDim (Proxy :: Proxy n)
      m = reflectDim (Proxy :: Proxy m)
  x <- return (map M.diffState (zipWith (\v w -> [v,w]) (replicate n 'x') ['1'..]))
  u <- return (map M.control (zipWith (\v w -> [v,w]) (replicate m 'u') ['1'..]))
  --(theta, _) <- M.diffState "theta"
  xUnwrap <- unwrap x
  uUnwrap <- unwrap u
  let xx = map fst xUnwrap
      force = zipWith (+) (createFce xx (a ocp) n) (createFce uUnwrap (b ocp) m)
      --obj = theta 
      obj = sum $ zipWith (*) (xx++uUnwrap) (xx++uUnwrap)
  M.output "obj" obj
  --M.output "force" force
  createEq (map snd xUnwrap) force 
  where
    createFce :: [SXElement] -> [Double] -> Int -> [SXElement]
    createFce xx aa nn
      | aa == [] = []
      | otherwise = fce
      where
        (hd,tl) = splitAt nn aa
        fce = (createLin xx hd) : createFce xx tl nn
        
    createQuad :: [SXElement] -> [Double] -> SXElement
    createQuad [] _  = 0
    createQuad xx qq = res
      where
        hd = head qq
        now = realToFrac hd :: SXElement
        temp = (head xx) * now * (head xx)
        res = temp + createQuad (tail xx) (tail qq) 
    
    createEq :: [SXElement] -> [SXElement] -> M.DaeMonad ()
    createEq [] _  = return ()
    createEq _ []  = return ()
    createEq (p:tlp) (f:tlf) = do
      p M.=== f
      createEq tlp tlf


-- this is to create the boundary conditions
boundaryConditions :: forall n m . (Dim n, Dim m) => Ocp n m -> (String -> M.BCMonad SXElement) -> (String -> M.BCMonad SXElement) -> M.BCMonad ()
boundaryConditions ocp get0 getF = do
  let n = reflectDim (Proxy :: Proxy n)
      m = reflectDim (Proxy :: Proxy m)
  xo <- return (map get0 (zipWith (\v w -> [v,w]) (replicate n 'x') ['1'..]))
  xf <- return (map getF (zipWith (\v w -> [v,w]) (replicate n 'x') ['1'..]))
  xUnwrap0 <- unwrap xo
  xUnwrapF <- unwrap xf
  createEq xUnwrap0 (map realToFrac (x0 ocp) :: [SXElement])
  createEq xUnwrapF (map realToFrac (xF ocp) :: [SXElement])
  where
    createEq :: [SXElement] -> [SXElement] -> M.BCMonad ()
    createEq [] _  = return ()
    createEq _ []  = return ()
    createEq (p:tlp) (f:tlf) = do
      p M.=== f
      createEq tlp tlf

-- this is to create the mayer
mayer :: (Dim n, Dim m, Floating a, Monad mm) => Ocp n m -> a -> (String -> mm a) -> (String -> mm a) -> mm a
mayer ocp endTime get0 getF = do
  --xf <- return (map getF (zipWith (\v w -> [v,w]) (replicate n 'x') ['1'..]))
  --xUnwrapF <- unwrap xf
  --let res = foldl (+) 0 (zipWith (*) (map realToFrac (c ocp)) (xUnwrapF))
  return 0

--this is to create the ocp to feed to solveStaticOcp
myOcp :: forall n m . (Dim n, Dim m) => Ocp n m -> SXElement -> (String -> M.OcpMonad SXElement) -> M.OcpMonad ()
myOcp ocp time get = do
  let n = reflectDim (Proxy :: Proxy n)
      m = reflectDim (Proxy :: Proxy m)
  x <- return (map get (zipWith (\v w -> [v,w]) (replicate n 'x') ['1'..]))
  xUnwrap <- unwrap x 
  u <- return (map get (zipWith (\v w -> [v,w]) (replicate m 'u') ['1'..]))
  uUnwrap <- unwrap u
  --theta <- get "theta"
  --force <- get "force"
  obj <- get "obj"
  --createInEq uUnwrap theta
  M.lagrangeTerm obj 
  where
    createInEq :: [SXElement] -> SXElement -> M.OcpMonad ()
    createInEq [] _ = return ()
    createInEq (w:tlw) l = do
      w M.<== l
      (-l) M.<== w
      createInEq tlw l

-- this tests that a feasible ocp is solvable
feasibleOcpIsFeasibleIO :: (Dim n, Dim m) => FeasibleOcp n m -> Property
feasibleOcpIsFeasibleIO (FeasibleOcp ocp) = monadicIO $ do
  feas <- run $ canSolve ocp
  if feas 
  then stop (succeeded {reason = "success"})
  else stop (failed {reason = "is not feasible"})

-- this tests that an  infeasible ocp is not solvable
infeasibleOcpIsNotFeasibleIO :: (Dim n, Dim m) => InfeasibleOcp n m -> Property
infeasibleOcpIsNotFeasibleIO (InfeasibleOcp ocp) = monadicIO $ do
  feas <- run $ canSolve ocp
  if feas 
  then stop (failed {reason = "is feasible"})
  else stop (succeeded {reason = "success"})

-- a group of tests
allTests :: [Test]
allTests = [ testGroup "io tests" [ testProperty "feas is feas (IO)" (feasibleOcpIsFeasibleIO :: FeasibleOcp D3 D2 -> Property)
                                  , testProperty "infeas is not feas (IO)" (infeasibleOcpIsNotFeasibleIO :: InfeasibleOcp D3 D2 -> Property)
                                  ]
           ]


-- this runs quickcheck manually
runManually :: IO ()
runManually = do
  --quickCheck (feasibleOcpIsFeasibleIO :: FeasibleOcp D3 D2 -> Property)
  quickCheck (infeasibleOcpIsNotFeasibleIO :: InfeasibleOcp D3 D2 -> Property)
  --quickCheck infeasibleOcpIsNotFeasibleIO
  --quickCheck infeasibleOcpIsNotFeasibleGIO

-- this uses test-framework to run all the tests
main :: IO ()
main = defaultMain allTests
