import qualified Numeric.LinearProgramming as GLPK
import System.IO
import System.Directory
import Data.List
import System.Random
import qualified RandGLPK as R

main = do
  gen <- getStdGen
  putStrLn "How many problems to generate ?"
  number <- getLine
  let n = read number
  generateProblem gen n


generateProblem :: StdGen -> Int -> IO ()
generateProblem gen n = do
  let (feasible, gen2) = random gen :: (Bool, StdGen)
      (bd, gen3) = random gen2 :: (Integer, StdGen)
      (g, gen4) = random gen3 :: (Int, StdGen)
      (param, bounds) = R.createParameters feasible g (- 200) (200)
      prob = GLPK.Minimize [R.a param, R.b param]
      constr1 = GLPK.Dense [ [R.c param, R.d param] GLPK.:=>: R.getlbg bounds
                      , [R.c param, R.d param] GLPK.:<=: R.getubg bounds
                      ]
  putStr $ show feasible ++ "   ->   "
  putStrLn.show $ GLPK.simplex prob constr1 [1 GLPK.:&: (R.getlbx1 bounds, R.getubx1 bounds), 2 GLPK.:&: (R.getlbx2 bounds, R.getubx2 bounds)] 
  --putStrLn $ show param ++ show bounds
  if n > 1
  then generateProblem gen4 (n-1)
  else return ()
