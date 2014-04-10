module HigherDim
( ParamOpt, ParamCst, BoundsSpace, BoundsCst
, generateProblem
, generateProblemF
, createLowerBounds, createUpperBounds
, generateProblemU
) where

import System.Random

type ParamOpt = [Double]
type ParamCst = [Double]
type BoundsSpace = [(Double,Double)]
type BoundsCst = [(Double,Double)]

generateProblem :: Int -> Bool -> Int -> Int -> (ParamOpt, ParamCst, BoundsSpace, BoundsCst)
generateProblem gen feasible n m =
  if feasible
  then generateProblemF gen n m
  else generateProblemU gen n m

--Case Feasible Problem
generateProblemF :: Int -> Int -> Int -> (ParamOpt, ParamCst, BoundsSpace, BoundsCst)
generateProblemF gen n m = (opt, pcst, bsp, bcst)
  where
  -- Generate parameters of the objective function
    maxB = 300
    minB = -300
    gen1:mopt = take (n+1) $ randomRs (minB, maxB) (mkStdGen gen) :: [Integer]  
    opt = zipWith (*) 
                  (map fromInteger mopt) 
                  (take n $ randoms (mkStdGen gen) :: [Double])
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
    gen5:mubcst = take (m+1) $ randomRs (minB, maxB) (mkStdGen (fromInteger gen4 :: Int)) :: [Integer]  
    ubcst = zipWith (*) 
                  (map fromInteger mubcst)
                  (take m $ randoms (mkStdGen (fromInteger gen4 :: Int)) :: [Double])
    generators = take m $ randoms (mkStdGen (fromInteger gen5 :: Int)) :: [Int]
    lbcst = zipWith (\(x,y) g -> fst $ randomR (x,y) (mkStdGen g) :: Double)
                    cstbounds
                    generators
    bcst = zipWith (\x y -> (min x y, max x y))
                  lbcst
                  ubcst
    
  

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
       

--Case Unfeasible Problem
generateProblemU :: Int -> Int -> Int -> (ParamOpt, ParamCst, BoundsSpace, BoundsCst)
generateProblemU gen n m = (opt, pcst, bsp, bcst)
  where
    maxB = 300
    minB = -300
  -- Generate parameters of the objective function
    gen1:mopt = take (n+1) $ randomRs (minB, maxB) (mkStdGen gen) :: [Integer]  
    opt = zipWith (*) 
                  (map fromInteger mopt) 
                  (take n $ randoms (mkStdGen gen) :: [Double])
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
    gen5:mubcst = take (m+1) $ randomRs (minB, maxB) (mkStdGen (fromInteger gen4 :: Int)) :: [Integer]  
    (constU, gen6) = randomR (1,m) (mkStdGen (fromInteger gen5 :: Int)) :: (Int, StdGen)
    (up, gen7) = random gen6 :: (Bool, StdGen)
    generators = take (2*m) $ randoms gen7 :: [Int]
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
