{-# OPTIONS_GHC -Wall #-}

module LPGenerate
( ParamOpt, ParamCst, BoundsSpace, BoundsCst
, generateProblem
, generateProblemSF
, mult
, epsilon
, great
, generateProblemU
, createLowerBounds, createUpperBounds
) where 

import System.Random

type ParamOpt = [Double]
type ParamCst = [Double]
type BoundsSpace = [(Double,Double)]
type BoundsCst = [(Double,Double)]

generateProblem :: Integer -> Bool -> Int -> Int -> ([Double], ParamOpt, ParamCst, BoundsSpace, BoundsCst)
generateProblem gen feasible n m =
  if feasible
  then generateProblemSF gen n m minB maxB
  else generateProblemU gen n m minB maxB
  where
    maxB = 3000
    minB = -3000

generateProblemSF :: Integer -> Int -> Int -> Integer -> Integer -> ([Double], ParamOpt, ParamCst, BoundsSpace, BoundsCst)
generateProblemSF gen n m minB maxB = (x0, opt, pcst, bsp, bcst)
  where
  -- Generate parameters of the objective function
    gen':mopt = take (n+1) $ randomRs (minB, maxB) (mkStdGen (fromInteger gen :: Int)) :: [Integer]  
    gen1 = if gen' == gen then gen'+1 else gen'
    opt = zipWith (*) 
                  (map fromInteger mopt) 
                  (take n $ randoms (mkStdGen (fromInteger gen :: Int)) :: [Double])
  -- Generate parameters of the matrix A
    gen2':mpcst = take (n*m+1) $ randomRs (minB, maxB) (mkStdGen (fromInteger gen1 :: Int)) :: [Integer]  
    gen2 = if gen2' == gen1 then gen2'+1 else gen2'
    pcst = zipWith (*) 
                  (map fromInteger mpcst)
                  (take (m*n) $ randoms (mkStdGen (fromInteger gen1 :: Int)) :: [Double])
  -- Generate possible solution
    gen3':msol = take (n+1) $ randomRs (minB, maxB) (mkStdGen (fromInteger gen2 :: Int)) :: [Integer]  
    gen3 = if gen3' == gen2 then gen3'+1 else gen3'
    sol = zipWith (*) 
                  (map fromInteger msol)
                  (take (m*n) $ randoms (mkStdGen (fromInteger gen2 :: Int)) :: [Double])
  -- Generate space bounds containing sol
    generators = take (n+m) $ randoms (mkStdGen (fromInteger gen3 :: Int)) :: [Int]
    (generator1, generator2) = splitAt n generators 
    bsp = zipWith (\v g -> (fst (randomR (v - great v, v - epsilon v) (mkStdGen g)) :: Double, fst (randomR (v + epsilon v,v + great v) (mkStdGen g)) :: Double))
                  sol
                  generator1
  -- Generate constraints for A*sol
    asol = mult n m pcst sol
    bcst = zipWith (\v g -> (fst $ randomR (v - great v, v - epsilon v) (mkStdGen g), fst $ randomR (v + epsilon v,v + great v) (mkStdGen g)))
                   asol
                   generator2
  -- Generate starting point x0
    x0 = zipWith (\(x,y) g -> fst $ randomR (x,y) (mkStdGen g) :: Double)
                 bsp
                 generator1


epsilon :: Double -> Double
epsilon v = if v == 0 then 0.001 else abs $ 0.001 * v

great :: Double -> Double
great v = if v == 0 then 3000 else abs $ 10 * v 

mult :: Int -> Int -> ParamCst -> [Double] -> [Double]
mult n m a v 
  | m == 0 = []
  | otherwise = res
  where
    (now,later) = splitAt n a
    res = sum (zipWith (*) now v) : mult n (m-1) later v 


--Case Unfeasible Problem
generateProblemU :: Integer -> Int -> Int -> Integer -> Integer -> ([Double], ParamOpt, ParamCst, BoundsSpace, BoundsCst)
generateProblemU gen n m minB maxB = (x0, opt, pcst, bsp, bcst)
  where
  -- Generate parameters of the objective function
    gen1:mopt = take (n+1) $ randomRs (minB, maxB) (mkStdGen (fromInteger gen :: Int)) :: [Integer]  
    opt = zipWith (*) 
                  (map fromInteger mopt) 
                  (take n $ randoms (mkStdGen (fromInteger gen :: Int)) :: [Double])
  -- Generate parameters of the matrix A
    gen2:mpcst = take (n*m+1) $ randomRs (minB, maxB) (mkStdGen (fromInteger gen1 :: Int)) :: [Integer]  
    pcst = zipWith (*) 
                  (map fromInteger mpcst)
                  (take (m*n) $ randoms (mkStdGen (fromInteger gen1 :: Int)) :: [Double])
  -- Generate Bounds for x
    gen3:mbsp1 = take (n+1) $ randomRs (minB, maxB) (mkStdGen (fromInteger gen2 :: Int)) :: [Integer]       
    bsp1 = zipWith (*) 
                  (map fromInteger mbsp1) 
                  (take n $ randoms (mkStdGen (fromInteger gen2 :: Int)) :: [Double])
    gen4:mbsp2 = take (n+1) $ randomRs (minB, maxB) (mkStdGen (fromInteger gen3 :: Int)) :: [Integer]      
    bsp2 = zipWith (*) 
                  (map fromInteger mbsp2)
                  (take n $ randoms (mkStdGen (fromInteger gen3 :: Int)) :: [Double])
    bsp = zipWith (\x y -> (min x y, max x y))
                  bsp1
                  bsp2
  -- Generate the constraints bounds
    cstbounds = zip (createLowerBounds n m bsp pcst)
                    (createUpperBounds n m bsp pcst)  
    (constU, gen6) = randomR (1,m) (mkStdGen (fromInteger gen4 :: Int)) :: (Int, StdGen)
    (up, gen7) = random gen6 :: (Bool, StdGen)
    generators = take (2*m+n) $ randoms gen7 :: [Int]
    (genl, genu) = splitAt m generators
    listconst = take m [constU, (constU-1)..]
    genlb = zip listconst genl
    genub = zip listconst genu
    lbcst = if up 
            then zipWith (\(x,y) (j,g) -> if j == 1 
                                          then fst $ randomR (y+1, y+1+ abs (9*y)) (mkStdGen g) :: Double
                                          else fst $ randomR (x - abs (9*x), y+ abs (9*y)) (mkStdGen g) :: Double)
                         cstbounds
                         genlb
            else zipWith (\(x,y) (j,g) -> if j == 1 
                                          then fst $ randomR (x-1 - abs (9*x), x-1) (mkStdGen g) :: Double
                                          else fst $ randomR (x - abs (9*x), y+ abs (9*y)) (mkStdGen g) :: Double)
                         cstbounds
                         genlb
    ubcst = if up 
            then zipWith (\(x,y) (j,g) -> if j == 1 
                                          then fst $ randomR (y+1, y+1 + abs (9*y)) (mkStdGen g) :: Double
                                          else fst $ randomR (x - abs (9*x), y+ abs (9*y)) (mkStdGen g) :: Double)
                         cstbounds
                         genub
            else zipWith (\(x,y) (j,g) -> if j == 1 
                                          then fst $ randomR (x-1 - abs (9*x), x-1) (mkStdGen g) :: Double
                                          else fst $ randomR (x - abs (9*x), y+ abs (9*y)) (mkStdGen g) :: Double)
                         cstbounds
                         genub
    bcst = zipWith (\x y -> (min x y, max x y))
                  lbcst
                  ubcst
    -- Generate starting point x0
    x0 = zipWith (\(x,y) g -> fst $ randomR (x,y) (mkStdGen g) :: Double)
                 bsp
                 generators


createLowerBounds :: Int -> Int -> BoundsSpace -> ParamCst -> [Double]
createLowerBounds n m bsp pcst 
  | m == 0    = []
  | otherwise = cstb
--returns the sum of lower bounds
  where
  (together, restp) = splitAt n pcst
  h = sum $ zipWith min (zipWith (*) (map fst bsp) together) (zipWith (*) (map snd bsp) together)  
  cstb = h:createLowerBounds n (m-1) bsp restp 
                    

createUpperBounds :: Int -> Int -> BoundsSpace -> ParamCst -> [Double]
createUpperBounds n m bsp pcst 
  | m == 0    = []
  | otherwise = cstb
--returns the sum of upper bounds
  where
  (together, restp) = splitAt n pcst
  h = sum $ zipWith max (zipWith (*) (map fst bsp) together) (zipWith (*) (map snd bsp) together) 
  cstb = h:createUpperBounds n (m-1) bsp restp 
