{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Main where

import System.IO
import Data.List

printTxt :: String -> Int -> Int -> IO()
printTxt p n m = do
  handle <- openFile "AutoCreate.hs" WriteMode
  hPutStrLn handle textIntro
  hPutStrLn handle (textType ++ textData)
  hPutStrLn handle (textNlp ++ textParam ++ textP ++ textInit ++ textFunct ++ textQPFunct ++ textLPFunct)
  hPutStrLn handle (textMain)
  hClose handle
  where
    textIntro = "{-# OPTIONS_GHC -Wall #-}\n{-# Language DeriveFunctor #-}\n{-# Language DeriveGeneric #-}\n\n"
                ++ "module Main where\n\n"
                ++ "import Dyno.Vectorize\nimport Dyno.Nlp\nimport Dyno.NlpSolver\nimport Dyno.Ipopt\nimport Dyno.Casadi.SXElement\n\n"
    textType = if p == "QP"
               then "import qualified QPGenerate as R\nimport System.Random\n\n"
               else "import qualified FromSolution as R\nimport System.Random\n\n"
    textData = "data X a = X " ++ intercalate " " (replicate n "a") ++ " deriving (Functor, Generic1, Show)\n"
               ++ "data G a = G " ++ intercalate " " (replicate m "a") ++ " deriving (Functor, Generic1, Show)\n\n"
               ++ "instance Vectorize X\ninstance Vectorize G\n\n"
    textNlp = "myNlp :: Integer -> Nlp X None G SXElement\nmyNlp gen = Nlp { nlpFG = fg\n                , nlpBX = bx\n                , nlpBG = bg\n                , nlpX0 = x0\n                , nlpP = None\n                }\n  where\n    "
             ++ if p == "QP"
                then "(start, optq, opt, pcst, bsp, bcst) = R.generateProblem gen True " ++ show n ++ " " ++ show m ++ "\n    "
                else "(start, opt, pcst, bsp, bcst) = R.generateProblem gen True " ++ show n ++ " " ++ show m ++ "\n    "
    textParam = "[" ++ foldr (++) "" (zipWith (\x y -> [x,y,',']) (replicate (n-1) 'l') ['1','2'..]) ++ "l" ++ show n ++ "] = "
                ++ "map fst bsp\n    "
                ++ "[" ++ foldr (++) "" (zipWith (\x y -> [x,y,',']) (replicate (n-1) 'u') ['1','2'..]) ++ "u" ++ show n ++ "] = "
                ++ "map snd bsp\n    "
                ++ "[" ++ foldr (++) "" (zipWith (\x y -> [x,y,',']) (replicate (m-1) 'b') ['1','2'..]) ++ "b" ++ show m ++ "] = "
                ++ "bcst\n    "
                ++ "[" ++ foldr (++) "" (zipWith (\x y -> [x,y,',']) (replicate (n-1) 'a') ['1','2'..]) ++ "a" ++ show n ++ "] "
                ++ "= map realToFrac opt :: [SXElement]\n    "
                ++ "[" ++ foldr (++) "" (zipWith (\x y -> [x,y,',']) (replicate (n-1) 'x') ['1','2'..]) ++ "x" ++ show n ++ "] "
                ++ "= start\n    "
                ++ if p == "QP"
                   then  "["++ intercalate "," ([['q',y,z]| y<-take n ['1','2'..], z<-take n ['1','2'..]]) ++ "] = map realToFrac optq :: [SXElement]\n    "
                   else ""
    textP = "["++ intercalate "," ([['c',y,z]| y<-take m ['1','2'..], z<-take n ['1','2'..]]) ++ "] = map realToFrac pcst :: [SXElement]\n\n    "
    textInit = "x0 :: X Double\n    x0 = X " ++ foldr (++) "" (zipWith (\x y -> [x,y,' ']) (replicate n 'x') ['1','2'..]) ++ "\n\n"
               ++ "    bx :: X Bounds\n    bx = X " ++ foldr (++) "" (zipWith (\x y -> "(Just "++x ++", Just "++ y ++") ") (zipWith (\x y -> [x,y]) (replicate n 'l') ['1','2'..]) (zipWith (\x y -> [x,y]) (replicate n 'u') ['1','2'..])) ++ "\n\n    "
               ++ "bg :: G Bounds\n    bg = G " ++ foldr (++) "" (map (\x -> "(Just (fst "++x ++"), Just (snd "++ x ++")) ") (zipWith (\x y -> [x,y]) (replicate m 'b') ['1','2'..])) ++ "\n\n    " 
    createLista = zipWith (\x y -> [x,y,' ']) ['a'..'z'] ['a'..'z']
    listF1 = zipWith (\x y -> x ++ " * q" ++ [y]) createLista ['1','2'..]
    listF2 = zipWith (\x y -> [y] ++ " * " ++ x) createLista ['1','2'..]
    
    textFunct = "fg :: X SXElement -> None SXElement -> (SXElement, G SXElement)\n    fg (X " ++ foldr (++) "" (take n createLista) ++ ") _ = (f,g)\n      where\n        f = "
    textQPFunct = if p == "QP" 
                  then intercalate " + " [x ++ y | x <- take n listF1, y <- take n listF2] ++ " + "
                  else ""
    textLPFunct = intercalate " + " (zipWith (\x y -> x ++ " * " ++ y) (zipWith (\x y -> [x,y]) (replicate n 'a') ['1','2'..]) createLista) ++ "\n        " ++ "g = G " ++ createG n m ([['c',y,z]| y<-take m ['1','2'..], z<-take n ['1','2'..]])
        
    createG :: Int -> Int -> [String] -> String
    createG nn mm pcst 
      |mm == 0 = ""
      |otherwise = g
      where
        (hd,tl) = splitAt nn pcst
        temp = "(" ++ intercalate " + " (zipWith (\x y -> x ++ " * " ++ y) hd createLista) ++ ") "
        g = temp ++ createG nn (mm-1) tl
        
    textMain = "\n\n\n\nmain :: IO ()\nmain = do\n  gen <- getStdGen\n  printN n gen 0\n    where\n      n = 1000\n      printN :: Int -> StdGen -> Int -> IO ()\n      printN nn gen counter = do\n        if nn == 1\n        then do\n          opt <- solveNlp quietIpoptSolver (myNlp genInt) Nothing\n          if fst opt == Right \"Solve_Succeeded\"\n          then putStrLn $ \"Solve Succeeded = \" ++ show (counter+1) ++ \" times\"\n          else putStrLn $ \"Solve Succeeded = \" ++ show counter ++ \" times\"\n        else do\n          opt <- solveNlp quietIpoptSolver (myNlp genInt) Nothing\n          if fst opt == Right \"Solve_Succeeded\"\n          then printN (nn-1) gen' (counter+1)\n          else printN (nn-1) gen' counter\n        where\n          (genInt, gen') = random gen :: (Integer, StdGen)\n\n          quietIpoptSolver :: NlpSolverStuff IpoptSolver\n          quietIpoptSolver =\n            ipoptSolver { defaultOptions =\n                            defaultOptions ipoptSolver ++\n                            [ (\"print_time\", Opt False)\n                            , (\"print_level\", Opt (0 :: Int))\n                            ]\n                        }"

    
main :: IO()
main = do
  putStrLn "LP or QP ?"
  p <- getLine
  putStrLn "How many dimensions for x ? n = "
  spaceDim <- getLine
  putStrLn "How many constraints on x ? m = "
  cstDim <- getLine
  let n = read spaceDim :: Int
      m = read cstDim :: Int
  printTxt p n m
