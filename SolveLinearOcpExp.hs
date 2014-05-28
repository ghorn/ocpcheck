{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}

module SolveLinearOcp
       ( feasibleOcpIsFeasible, infeasibleOcpIsInfeasible
       ) where

import Test.QuickCheck.Monadic
import Test.QuickCheck.Property
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.Vector ( Vector )
import qualified Data.Foldable as F
import Linear
import System.IO

import Dyno.Vectorize
import Dyno.View
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Ocp
import Dyno.DirectCollocation
import Dyno.Cov
import Dyno.Nats
import Dyno.TypeVecs
import Dyno.Ipopt

import LinearOcpExp ( IsLinearOcp(..), LinearOcp(..)
                      , FeasibleLinearOcp(..)
                      , InfeasibleLinearOcp(..)
                      , roughPrint, fromListsToOcp
                      --, runGenWithSeed
                      )

data Bcs n a = Bcs (Vec n a) (Vec n a) deriving (Functor, Generic1, Show)
data X n a = X (Vec n a) deriving (Functor, Generic1, Show)
data U m a = U (Vec m a) deriving (Functor, Generic1, Show)

instance Dim n => Vectorize (X n)
instance Dim m => Vectorize (U m)
instance Dim n => Vectorize (Bcs n)

mayer :: Floating a => a -> x -> x -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer _ _ _ _ _ = 0

lagrange :: Floating a => X n a -> None a -> U m a -> None a -> None a -> a -> a
--lagrange _ _ _ _ _ _ = 1
lagrange (X xvec) _ (U vec) _ _ _ = F.sum (fmap (\x -> x*x) xvec) + F.sum (fmap (\x -> x*x) vec)

myDae :: forall n m a . (Dim n, Dim m, Floating a)
         => LinearOcp n m -> Dae (X n) None (U m) None (X n) None a
myDae myOcp (X x') (X x) _ (U u) _ _ = (X (x' ^-^ force), None)
  where
    a' :: Vec n (Vec n a)
    a' = fmap (fmap realToFrac) (loA myOcp)

    b' :: Vec n (Vec m a)
    b' = fmap (fmap realToFrac) (loB myOcp)

    force :: Vec n a
    force = (a' !* x)  ^+^  (b' !* u)

bc :: forall n m a . (Dim n, Floating a) => LinearOcp n m -> X n a -> X n a -> Bcs n a
bc myOcp (X x0) (X xF) = Bcs
                         (x0 ^-^ x0')
                         (xF ^-^ xF')
  where
    x0',xF' :: Vec n a
    x0' = fmap realToFrac $ loX0 myOcp
    xF' = fmap realToFrac $ loXF myOcp

pathc :: x a -> z a -> u a -> p a -> None a -> a -> None a
pathc _ _ _ _ _ _ = None

bds :: Dim n => Vec n (Double, Double) -> Vec n Bounds
bds ocp = fmap (\(x,y) -> (Just x, Just y)) ocp

tBounds :: LinearOcp n m -> Bounds
tBounds ocp = (Just 1e-9, Just (t*10))
--tBounds ocp = (Just maxt, Just maxt)
  where
    --mint = realToFrac (floor (loT ocp) :: Int)
    --maxt = realToFrac (ceiling (loT ocp) :: Int)
    t = loT ocp

toOcpPhase :: (Dim n, Dim m, IsLinearOcp a n m)
              => a -> OcpPhase (X n) None (U m) None (X n) None (Bcs n) None JNone JNone JNone
toOcpPhase myOcp' =
  OcpPhase { ocpMayer = mayer
           , ocpLagrange = lagrange
           , ocpDae = myDae myOcp
           , ocpBc = bc myOcp
           , ocpPathC = pathc
           , ocpPathCBnds = None
           , ocpBcBnds = fill (Just 0, Just 0)
           , ocpXbnd = X $ bds $ loXb myOcp
           , ocpUbnd = U $ bds $ loUb myOcp
           , ocpZbnd = None
           , ocpPbnd = None
           , ocpTbnd = tBounds myOcp 

           , ocpSq = 0
           , ocpSbnd = jfill (Nothing,Nothing)
           , ocpSbc = \_ _ -> cat JNone
           , ocpSbcBnds = cat JNone
           , ocpSh = \_ _ -> cat JNone
           , ocpShBnds = cat JNone
           }
  where
    myOcp = getLinearOcp myOcp'

solveAlone :: (Dim n, Dim m) => LinearOcp n m -> IO()
solveAlone ocp = do
  _ <- solveLinearOcp ipoptSolver ocp
  return ()

solveLinearOcp :: forall n m a nlp . (IsLinearOcp a n m, Dim n, Dim m, NLPSolverClass nlp)
                  => NlpSolverStuff nlp -> a -> IO (Either String String)
solveLinearOcp solver ocp = do
  let guess = jfill 1 :: J (CollTraj (X n) None (U m) None JNone D100 D3) (Vector Double)
  nlp <- makeCollNlp (toOcpPhase (getLinearOcp ocp))
  fmap fst $ solveNlp' solver (nlp { nlpX0' = guess }) Nothing

feasibleOcpIsFeasible :: (Dim n, Dim m, NLPSolverClass nlp)
                         => NlpSolverStuff nlp -> FeasibleLinearOcp n m -> Property
feasibleOcpIsFeasible solver (FeasibleLinearOcp ocp) = monadicIO $ do
  ret <- run $ solveLinearOcp solver ocp
  case ret of
    Right msg -> stop (succeeded {reason = msg})
    Left msg -> stop (failed {reason = msg})

infeasibleOcpIsInfeasible :: (Dim n, Dim m, NLPSolverClass nlp)
                             => NlpSolverStuff nlp -> InfeasibleLinearOcp n m -> Property
infeasibleOcpIsInfeasible solver (InfeasibleLinearOcp ocp) = monadicIO $ do
  ret <- run $ solveLinearOcp solver ocp
  case ret of
    Right msg -> stop (failed {reason = msg})
    Left msg -> stop (succeeded {reason = msg})

quietIpoptSolver :: NlpSolverStuff IpoptSolver
quietIpoptSolver =
  ipoptSolver { options =
                   [ ("max_iter", Opt (3000 :: Int))
                   , ("print_time", Opt False)
                   , ("print_level", Opt (0 :: Int))
                   ]
              }

go :: IO()
go = test 500 0 []
  where
    test :: Int -> Int -> [String] -> IO()
    test nn count text = do
      if nn == 0 
      then do
        putStrLn $ "succeed : " ++ show count ++ " / 500"
        putStrLn $ unlines text
      else do
        ocp <- generate (arbitrary :: Gen (FeasibleLinearOcp D2 D1))
        ret <- solveLinearOcp quietIpoptSolver ocp
        if ret == Right "Solve_Succeeded"
        then do
          test (nn-1) (count+1) text
        else do
          handle <- openFile "NonSolveableOcp.txt" AppendMode
          hPutStrLn handle $ show ret
          hPutStrLn handle $ roughPrint ocp
          hPutStrLn handle $ show ocp
          hClose handle
          --_ <- solveLinearOcp ipoptSolver ocp
          let temp = show ret 
          test (nn-1) count (temp:text)

gogo :: IO()
gogo = test 1000 0
  where
    test :: Int -> Int -> IO()
    test nn count = do
      if nn == 0 
      then putStrLn $ "succeed : " ++ show count ++ " / 2"
      else do
        ocp <- generate (arbitrary :: Gen (FeasibleLinearOcp D2 D1))
        ret <- solveLinearOcp quietIpoptSolver ocp
        if ret == Right "Solve_Succeeded"
        then do
          print count
          --print ocp
          test (nn-1) (count+1)
        else do
          _ <- solveLinearOcp ipoptSolver ocp
          print ocp
          print ret
          putStrLn $ "try #" ++ show (count+1)
           

  
