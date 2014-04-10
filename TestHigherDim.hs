import qualified Numeric.LinearProgramming as GLPK
import System.IO
import System.Directory
import Data.List
import System.Random
import qualified HigherDim as R

main = do
  gen <- getStdGen
  putStrLn "How many dimensions ? n = "
  inputn <- getLine
  putStrLn "How many constraints ? m = "
  inputm <- getLine
  putStrLn "How many problems to generate ?"
  number <- getLine
  putStrLn "Feasible [True] or Unfeasible [False] ?"
  feas <- getLine
  let num = read number
      n = read inputn
      m = read inputm
      feasible = read feas
  generateProblems gen feasible num n m 

generateProblems :: StdGen -> Bool -> Int -> Int -> Int -> IO ()
generateProblems gen feasible num n m = do
  let (genInt,gen2) = random gen :: (Int, StdGen)
      (opt, pcst, bsp, bcst) = R.generateProblem genInt feasible n m
      prob = GLPK.Minimize opt
      constr = GLPK.Dense $ createConstraints pcst bcst n m
  putStrLn.show $ GLPK.simplex prob constr $ zipWith (GLPK.:&:) [1..n] bsp
  --putStrLn $ show test ++ "    " ++ show bcst ++ "   " ++ show bsp ++ "   " ++ show pcst ++ show opt
  putStrLn ""
  if num > 1 
  then generateProblems gen2 feasible (num-1) n m
  else return ()


createConstraints :: R.ParamCst -> R.BoundsCst -> Int -> Int -> [GLPK.Bound [Double]]
createConstraints pcst bcst n m = 
  if m == 0
  then []
  else constr
  where
    (together, restp) = splitAt n pcst
    ([(l,u)], restb) = splitAt 1 bcst
    h1 = together GLPK.:<=: u
    h2 = together GLPK.:=>: l
    constr = h1:h2:createConstraints restp restb n (m-1)
