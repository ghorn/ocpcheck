{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module QP where

import Dyno.NlpMonad
import Dyno.Ipopt
import Dyno.NlpSolver
import Dyno.Casadi.SXElement ( SXElement )

import Test.QuickCheck.Property
import Test.QuickCheck.Monadic


n :: Int
n = 3 
m :: Int
m = 2 


data QP = QP { q :: [Double]
             , c :: [Double]
             , a :: [Double]
             , gBounds :: [(Double,Double)]
             , xBounds :: [(Double, Double)]
             , x0 :: [Double]
             } deriving (Show, Eq)

quietIpoptSolver :: NlpSolverStuff IpoptSolver
quietIpoptSolver =
  ipoptSolver { defaultOptions =
                   defaultOptions ipoptSolver ++
                   [ ("print_time", Opt False)
                   , ("print_level", Opt (0 :: Int))
                   ]
              }


canSolve :: QP -> IO (Maybe Bool)
canSolve qp = do
  feas <- testSolve qp
  if feas == Right "Solve_Succeeded"
  then return (Just True)
  else if feas == Left "Infeasible_Problem_Detected"
       then return (Just False)
       else return( Nothing)

testSolve :: QP -> IO (Either String String)
testSolve qp = do
  (status, _, _) <- solveStaticNlp quietIpoptSolver (qpToNlp qp) guess Nothing
  return status
  where
    guess = zip (zipWith (\v w -> [v,w]) (replicate n 'x') ['1'..]) (x0 qp)   

            
qpToNlp :: QP -> NlpMonad ()
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
