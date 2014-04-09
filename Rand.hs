{-# OPTIONS_GHC -Wall #-}
import System.Random

-- Definition of new types for a better readability
data Bounds = Bounds { getlbx1 :: Float
                     , getubx1 :: Float
                     , getlbx2 :: Float
                     , getubx2 :: Float
                     , getlbg :: Float
                     , getubg :: Float
                     } deriving (Show, Eq, Ord)

data Param = Param { a :: Float
                   , b :: Float
                   , c :: Float
                   , d :: Float
                   } deriving (Show, Eq, Ord)

-- Function assembling all the others
createParameters :: Bool -> Int -> Integer -> Integer -> (Param, Bounds)
createParameters feasible gen minB maxB =
  if feasible
  then createBoundsF gen minB maxB
  else createBoundsU gen minB maxB

-- Case Feasible problem
createBoundsF :: Int -> Integer -> Integer -> (Param, Bounds)
createBoundsF gen minB maxB = ((Param aa bb cc dd), (Bounds lbx ubx lby uby lbg ubg))
  where
    [ma,mb,mc,md,mlx,mux,mly,muy,mug,gen'] = take 10 $ randomRs (minB,maxB) (mkStdGen gen) :: [Integer]
    [aa,bb,cc,dd,lx,ux,ly,uy,ug] = zipWith (\x y -> x*y)
                            (map fromInteger [ma,mb,mc,md,mlx,mux,mly,muy,mug])
                            (take 9 $ randoms (mkStdGen gen) :: [Float])
    [lbx,ubx,lby,uby] = [(min ux lx), (max ux lx), (min uy ly), (max uy ly)]
    [llx, uux, lly, uuy] = [ (min (cc*lbx) (cc*ubx))
                           , (max (cc*lbx) (cc*ubx))
                           , (min (dd*lby) (dd*uby))
                           , (max (dd*lby) (dd*uby))
                           ]
    gen2 = mkStdGen (fromInteger gen' :: Int)
    [lg] = take 1 $ randomRs (llx+lly,uux+uuy) gen2 :: [Float]
    [lbg,ubg] = [min lg ug, max lg ug]



genGBoundsF :: [Float] -> Float -> Float -> Float -> Float -> (Float,Float)
genGBoundsF rlist llx uux lly uuy =
  if isPossible llx uux lly uuy lg
  then (min lg ug, max lg ug)
  else if isPossible llx uux lly uuy ug
       then (min lg ug, max lg ug)
       else genGBoundsF rest llx uux lly uuy  -- Step 2
  where
    ([lg,ug], rest) = splitAt 2 rlist  -- Step 1

isPossible :: Float -> Float -> Float -> Float -> Float -> Bool         -- Step 1
isPossible llx uux lly uuy lg = (lg > llx + lly) && (lg < uux + uuy)


-- Case Unfeasible problem
createBoundsU :: Int -> Integer -> Integer -> (Param, Bounds)
createBoundsU gen minB maxB =
  if not $ testBounds lg ug lx ux ly uy
  then ((Param aa bb cc dd), (Bounds lx ux ly uy lg ug))
  else if (uux+uuy < lg) || (llx +lly > ug)
       then ((Param aa bb cc dd), (Bounds lx ux ly uy lg ug))
       else ((Param aa bb cc dd), (Bounds lx ux ly uy lbg ubg))
  where
    [aa,bb,cc,dd,ux,lx,uy,ly,ug,lg] =
      take 10 $ zipWith (*)
      (map fromInteger (randomRs (minB,maxB) (mkStdGen gen) :: [Integer]))
      (randoms (mkStdGen gen) :: [Float])

    [llx, uux, lly, uuy] = [ (min (cc*lx) (cc*ux))
                           , (max (cc*lx) (cc*ux))
                           , (min (dd*ly) (dd*uy))
                           , (max (dd*ly) (dd*uy))
                           ]
    (lbg,ubg) = genGBoundsU2 (llx+lly) (uux+uuy) minB maxB
    
    
testBounds :: Float -> Float -> Float -> Float -> Float -> Float -> Bool -- Step 1
testBounds lbg ubg lbx1 ubx1 lbx2 ubx2
  | lbg > ubg = False
  | lbx1 > ubx1 = False
  | lbx2 > ubx2 = False
  | otherwise = True

genGBoundsU2 :: Float -> Float -> Integer-> Integer-> (Float, Float)
genGBoundsU2 llg uug minB maxB = 
  if up
  then (lbg1,ubg1)
  else (lbg2,ubg2)
  where
    [up] = take 1 $ randoms (mkStdGen (truncate llg :: Int)) :: [Bool]
    [up1,up2] = take 2 $ randomRs (uug,uug+abs (fromInteger maxB :: Float)) (mkStdGen (truncate llg :: Int)) :: [Float]
    [down1,down2] = take 2 $ randomRs (llg- abs (fromInteger minB :: Float),llg) (mkStdGen (truncate llg :: Int)) :: [Float]
    (lbg1,ubg1) = (min up1 up2, max up1 up2)
    (lbg2,ubg2) = (min down1 down2, max down1 down2) 
