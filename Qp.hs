{-# OPTIONS_GHC -Wall #-}

module Qp ( main, runManually ) where

import Test.QuickCheck ( quickCheck )
--import Test.QuickCheck ( Args(..), quickCheckWith, stdArgs )
import Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import Test.QuickCheck.Gen
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework ( Test, defaultMain, testGroup )
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property

import Dyno.NlpMonad
import Dyno.Ipopt
import Dyno.NlpSolver
import Dyno.Casadi.SXElement ( SXElement )

import AccessoryFunctions

n :: Int
n = 5

m :: Int
m = 6

quietIpoptSolver :: NlpSolverStuff IpoptSolver
quietIpoptSolver =
  ipoptSolver { defaultOptions =
                   defaultOptions ipoptSolver ++
                   [ ("print_time", Opt False)
                   , ("print_level", Opt (0 :: Int))
                   ]
              }


-- real "QP" 
data Qp = Qp { q :: [Double]                   -- Quadratic term of the objective function
             , c :: [Double]                   -- Linear term of the objective function
             , a :: [Double]                   -- Linear constraints
             , gBounds :: [(Double,Double)]    -- Bounds for the constraints
             , xBounds :: [(Double, Double)]   -- Bounds for the space
             , x0 :: [Double]                  -- Initial guess or starting point
             } deriving (Show, Eq)
data FeasibleQp = FeasibleQp Qp deriving Show
data InfeasibleQp = InfeasibleQp Qp 

instance Show InfeasibleQp where
  show (InfeasibleQp (Qp u1 u2 u3 u4 u5 u6)) = "q = " ++ show u1 ++ "\nc = " ++ show u2 ++ "\na = " ++ show u3 ++ "\ngBounds = " ++ show u4 ++ "\nxBounds = " ++ show u5 ++ "\nx0 = " ++ show u6

-- make it always feasible
instance Arbitrary FeasibleQp where
  arbitrary = do
    k1 <- infiniteListOf (suchThat (arbitrary :: Gen Double) (0 /=))
    let (kq,r1) = splitAt (n*n) k1
        (kc,r2) = splitAt n r1
        (ka,r3) = splitAt (n*m) r2
        kx = take n r3
    gb <- suchThatGF n ka kx
    xb <- suchThatVF kx
    x <- suchThatXF xb
    return (FeasibleQp Qp { q = kq, c = kc, a = ka, gBounds = gb, xBounds = xb, x0 = x})
    
-- make it always infeasible
instance Arbitrary InfeasibleQp where
  arbitrary = do
    k1 <- infiniteListOf (suchThat (arbitrary :: Gen Double) (0 /=))
    let (kq,r1) = splitAt (n*n) k1
        (kc,r2) = splitAt n r1
        (ka,r3) = splitAt (n*m) r2
        kx = take n r3
    j <- elements [1..m]
    xb <- suchThatVI kx
    xa <- createBounds n xb ka
    gb <- suchThatGI xa j
    x <- suchThatXI xb
    return (InfeasibleQp Qp { q = kq, c = kc, a = ka, gBounds = gb, xBounds = xb, x0 = x})
    
-- this is like "isSolveable"
canSolve :: Qp -> IO (Bool)
canSolve qp = do
  (status, _, _) <- solveStaticNlp quietIpoptSolver (qpToNlp qp) guess Nothing
  if status == Right "Solve_Succeeded"
  then return True
  else return False
  where
    guess = zip (zipWith (\v w -> [v,w]) (replicate n 'x') ['1'..]) (x0 qp)

-- this is to create a NlpMonad from a Qp
qpToNlp :: Qp -> NlpMonad ()
qpToNlp qp = do
  x <- return (map designVar (zipWith (\v w -> [v,w]) (replicate n 'x') ['1'..]))
  xUnwrap <- unwrap x
  createBds xUnwrap (xBounds qp)
  createCst xUnwrap (a qp) (gBounds qp)
  minimize $ (createQuad xUnwrap (q qp) xUnwrap) + (createLin xUnwrap (c qp))
  where
    createBds :: [SXElement] -> [(Double, Double)] -> NlpMonad()
    createBds xx bds
      | bds == [] = return ()
      | otherwise = do
        createBds (tail xx) (tail bds)
        bound (head xx) (head bds)

    unwrap :: [NlpMonad SXElement] -> NlpMonad [SXElement]
    unwrap [] = return []
    unwrap xx = do
      yy <- head xx
      end <- unwrap (tail xx)
      return (yy:end)

    createCst :: [SXElement] -> [Double] -> [(Double, Double)] -> NlpMonad()
    createCst xx aa lu
      | aa == [] = return ()
      | otherwise = do
        bound mid (head lu)
        createCst xx tl (tail lu)
        where
          (hd, tl) = splitAt n aa
          now = map realToFrac hd :: [SXElement]
          temp = zipWith (*) now xx
          mid = foldl (+) 0 temp
         
    createLin :: [SXElement] -> [Double] -> SXElement
    createLin xx qq = foldl (+) 0 (zipWith (*) qS xx)
      where
        qS = map realToFrac qq :: [SXElement]

    createQuad :: [SXElement] -> [Double] -> [SXElement] -> SXElement
    createQuad [] _ _ = 0
    createQuad xx qq yy = res
      where
        (hd, tl) = splitAt n qq
        now = map realToFrac hd :: [SXElement]
        temp = map ((head xx) *) (zipWith (*) now yy)
        res = foldl (+) 0 temp + createQuad (tail xx) tl yy

-- this tests that a feasible qp is solvable
feasibleQpIsFeasibleIO :: FeasibleQp -> Property
feasibleQpIsFeasibleIO (FeasibleQp qp) = monadicIO $ do
  feas <- run $ canSolve qp
  if feas 
  then stop (succeeded {reason = "success"})
  else stop (failed {reason = "is not feasible"})

-- this tests that an infeasible qp is not solvable
infeasibleQpIsNotFeasibleIO :: InfeasibleQp -> Property
infeasibleQpIsNotFeasibleIO (InfeasibleQp qp) = monadicIO $ do
  feas <- run $ canSolve qp
  if feas
    then stop (failed {reason = "is feasible"})
    else stop (succeeded {reason = "success"})

-- a group of tests
allTests :: [Test]
allTests = [ testGroup "io tests" [ testProperty "feas is feas (IO)" feasibleQpIsFeasibleIO
                                  , testProperty "infeas is not feas (IO)" infeasibleQpIsNotFeasibleIO
                                  ]
           ]


-- this runs quickcheck manually
runManually :: IO ()
runManually = do
  --quickCheck feasibleQpIsFeasible
  --quickCheck infeasibleQpIsNotFeasible
  quickCheck feasibleQpIsFeasibleIO
  quickCheck infeasibleQpIsNotFeasibleIO

-- this uses test-framework to run all the tests
main :: IO ()
main = defaultMain allTests
