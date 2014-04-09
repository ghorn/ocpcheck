module RandGLPK
( Bounds, Param,
 a, b, c, d,
 getlbx1, getubx1, getlbx2, getubx2,getlbg, getubg, 
 createParameters, 
 createBoundsF, genGBoundsF,isPossible,
 createBoundsU, testBounds, genGBoundsU,genGBoundsU2, isNotPossible
 ) where

import System.Random

-- Definition of new types for a better readability

data Bounds = Bounds { getlbx1 :: Double
                     , getubx1 :: Double
                     , getlbx2 :: Double
                     , getubx2 :: Double
                     , getlbg :: Double
                     , getubg :: Double
                     } deriving (Eq, Ord)

instance Show (Bounds) where
	show (Bounds lbx1 ubx1 lbx2 ubx2 lbg ubg) = "param lbx1 := "++show lbx1 ++";\n"++"param ubx1 := "++show ubx1 ++";\n"++"param lbx2 := "++show lbx2 ++";\n"++"param ubx2 := "++show ubx2 ++";\n"++"param lbg := "++show lbg ++";\n"++"param ubg := "++show ubg ++";\n"
	
data Param = Param { a :: Double
                   , b :: Double
                   , c :: Double
                   , d :: Double
                   } deriving (Eq, Ord)

instance Show (Param) where
	show (Param a b c d) = "param a := "++show a ++";\n"++"param b := "++show b ++";\n"++"param c := "++show c ++";\n"++"param d := "++show d ++";\n"

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
                            (take 9 $ randoms (mkStdGen gen) :: [Double])
    [lbx,ubx,lby,uby] = [(min ux lx), (max ux lx), (min uy ly), (max uy ly)]
    [llx, uux, lly, uuy] = [ (min (cc*lbx) (cc*ubx))
                           , (max (cc*lbx) (cc*ubx))
                           , (min (dd*lby) (dd*uby))
                           , (max (dd*lby) (dd*uby))
                           ]
    gen2 = mkStdGen (fromInteger gen' :: Int)
    [lg] = take 1 $ randomRs (llx+lly,uux+uuy) gen2 :: [Double]
    [lbg,ubg] = [min lg ug, max lg ug]


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
      (randoms (mkStdGen gen) :: [Double])

    [llx, uux, lly, uuy] = [ (min (cc*lx) (cc*ux))
                           , (max (cc*lx) (cc*ux))
                           , (min (dd*ly) (dd*uy))
                           , (max (dd*ly) (dd*uy))
                           ]
    (lbg,ubg) = genGBoundsU2 (llx+lly) (uux+uuy) minB maxB


genGBoundsU2 :: Double -> Double -> Integer-> Integer-> (Double, Double)
genGBoundsU2 llg uug minB maxB = 
  if up
  then (lbg1,ubg1)
  else (lbg2,ubg2)
  where
    [up] = take 1 $ randoms (mkStdGen (truncate llg :: Int)) :: [Bool]
    [up1,up2] = take 2 $ randomRs (uug,uug+abs (fromInteger maxB :: Double)) (mkStdGen (truncate llg :: Int)) :: [Double]
    [down1,down2] = take 2 $ randomRs (llg- abs (fromInteger minB :: Double),llg) (mkStdGen (truncate llg :: Int)) :: [Double]
    (lbg1,ubg1) = (min up1 up2, max up1 up2)
    (lbg2,ubg2) = (min down1 down2, max down1 down2) 
  

testBounds :: Double -> Double -> Double -> Double -> Double -> Double -> Bool -- Step 1
testBounds lbg ubg lbx1 ubx1 lbx2 ubx2
  | lbg > ubg = False
  | lbx1 > ubx1 = False
  | lbx2 > ubx2 = False
  | otherwise = True
			
	
	
