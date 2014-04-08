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
  if feasible then createBoundsF gen minB maxB else createBoundsU gen minB maxB

-- Case Feasible problem
createBoundsF :: Int -> Integer -> Integer -> (Param, Bounds)
createBoundsF gen minB maxB =
  let [ma,mb,mc,md,mlx,mux,mly,muy,gen'] = take 9 $ randomRs (minB,maxB) (mkStdGen gen) :: [Integer]
      [a,b,c,d,lx,ux,ly,uy] = zipWith (\x y -> x*y) (map fromInteger [ma,mb,mc,md,mlx,mux,mly,muy]) (take 8 $ randoms (mkStdGen gen) :: [Float])
      [lbx,ubx,lby,uby] = [(min ux lx), (max ux lx), (min uy ly), (max uy ly)]
      [llx, uux, lly, uuy] = [(min (c*lbx) (c*ubx)), (max (c*lbx) (c*ubx)), (min (d*lby) (d*uby)),(max (d*lby) (d*uby))]
      gen2 = mkStdGen (fromInteger gen' :: Int)
      randlist = zipWith (*) (map fromInteger (randomRs (minB,maxB) gen2 :: [Integer])) (randoms gen2 :: [Float])
      (lbg,ubg) = genGBoundsF randlist llx uux lly uuy
  in ((Param a b c d), (Bounds lbx ubx lby uby lbg ubg))


genGBoundsF :: [Float] -> Float -> Float -> Float -> Float -> (Float,Float)
genGBoundsF rlist llx uux lly uuy =
  let ([lg,ug], rest) = splitAt 2 rlist in  -- Step 1
  if isPossible llx uux lly uuy lg
  then (min lg ug, max lg ug)
  else if isPossible llx uux lly uuy ug
       then (min lg ug, max lg ug)
       else genGBoundsF rest llx uux lly uuy  -- Step 2

isPossible :: Float -> Float -> Float -> Float -> Float -> Bool         -- Step 1
isPossible llx uux lly uuy lg = (lg > llx + lly) && (lg < uux + uuy)


-- Case Unfeasible problem

createBoundsU :: Int -> Integer -> Integer -> (Param, Bounds)
createBoundsU gen minB maxB =
  let ([a,b,c,d,ux,lx,uy,ly,ug,lg],rest) = splitAt 10 $ zipWith (*) (map fromInteger (randomRs (minB,maxB) (mkStdGen gen) :: [Integer])) (randoms (mkStdGen gen) :: [Float]) in
  if not $ testBounds lg ug lx ux ly uy then ((Param a b c d), (Bounds lx ux ly uy lg ug))
  else let [llx, uux, lly, uuy] = [(min (c*lx) (c*ux)), (max (c*lx) (c*ux)), (min (d*ly) (d*uy)),(max (d*ly) (d*uy))] in
  if (uux+uuy < lg) || (llx +lly > ug) then ((Param a b c d), (Bounds lx ux ly uy lg ug))
  else let (lbg,ubg) = genGBoundsU rest llx uux lly uuy in
  ((Param a b c d), (Bounds lx ux ly uy lbg ubg))

testBounds :: Float -> Float -> Float -> Float -> Float -> Float -> Bool -- Step 1
testBounds lbg ubg lbx1 ubx1 lbx2 ubx2
  | lbg > ubg = False
  | lbx1 > ubx1 = False
  | lbx2 > ubx2 = False
  | otherwise = True


genGBoundsU :: [Float] -> Float -> Float -> Float -> Float -> (Float,Float)
genGBoundsU rlist llx uux lly uuy =
  let ([lg,ug], rest) = splitAt 2 rlist in
  if lg > ug then (lg,ug) -- Step 1
  else
    if isNotPossible llx uux lly uuy lg ug then (lg,ug) else genGBoundsF rest llx uux lly uuy


isNotPossible :: Float -> Float -> Float -> Float -> Float -> Float -> Bool -- Step 2
isNotPossible llx uux lly uuy lg ug = (ug < llx + lly) || (lg > uux + uuy)
