{-# OPTIONS_GHC -Wall #-}

module IQP where

import Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import Test.QuickCheck.Gen

import QP

data InfeasibleQP = InfeasibleQP QP deriving(Show)

instance Arbitrary InfeasibleQP where
  arbitrary = do
    k1 <- infiniteListOf (suchThat (arbitrary :: Gen Double) (0 /=))
    let (kq,r1) = splitAt (n*n) k1
        (kc,r2) = splitAt n r1
        (ka,r3) = splitAt (n*m) r2
        kx = take n r3
    j <- elements [1..m]
    xb <- suchThatV kx
    xa <- createBounds xb ka
    gb <- suchThatG xa j
    x <- suchThatX xb
    return (InfeasibleQP QP { q = kq, c = kc, a = ka, gBounds = gb, xBounds = xb, x0 = x})
    where
      suchThatV :: [Double] -> Gen [(Double,Double)]
      suchThatV k 
        | k == [] = return []
        | otherwise = do
          l <- suchThat (arbitrary :: Gen Double) ((head k) >)
          u <- suchThat (arbitrary :: Gen Double) ((head k) <)
          tl <- suchThatV (tail k)
          return ((l,u): tl)
      
      createBounds :: [(Double, Double)] -> [Double] -> Gen [(Double, Double)]
      createBounds xx aa 
        | aa == [] = return []
        | otherwise = do
          --l <- choose (lx,ux)
          --u <- suchThat (arbitrary :: Gen Double) (\_ -> True)
          end <- createBounds xx tl
          return ((lx,ux):end)
          where
            (hd, tl) = splitAt n aa
            lx = sum $ zipWith min (zipWith (*) (map fst xx) hd) (zipWith (*) (map snd xx) hd)
            ux = sum $ zipWith max (zipWith (*) (map fst xx) hd) (zipWith (*) (map snd xx) hd)
  
      suchThatG :: [(Double, Double)] -> Int -> Gen [(Double, Double)]
      suchThatG y i
        | y == [] = return []
        | i == 1 = do
          up <- suchThat (arbitrary :: Gen Bool) (\_ -> True)
          l <- suchThat (arbitrary :: Gen Double) (\w -> if up then (w > snd (head y)) else (w < fst (head y)))
          u <- suchThat (arbitrary :: Gen Double) (\w -> if up then (w > snd (head y)) else (w < fst (head y)))
          end <- suchThatG (tail y) (i-1)
          return ((min l u, max l u) : end)
        | otherwise = do
          l <- suchThat (arbitrary :: Gen Double) (\_ -> True)
          u <- suchThat (arbitrary :: Gen Double) (\_ -> True)
          end <- suchThatG (tail y) (i-1)
          return ((min l u, max l u) : end)
  
      suchThatX :: [(Double, Double)] -> Gen [Double]
      suchThatX bd 
        | bd == [] = return []
        | otherwise = do
          x1 <- choose (l,u)
          end <- suchThatX tl
          return (x1 : end)
          where
            (l,u):tl = bd
            
infeasibleQp :: IO()
infeasibleQp = do
  fqp <- (sample' (arbitrary :: Gen InfeasibleQP))
  printN fqp 0
  putStrLn $ "out of " ++ show (length fqp)
  where
    printN :: [InfeasibleQP] -> Int -> IO ()
    printN [] nn = print nn
    printN f nn = do
      --print (head f)
      res <- canSolve (ifqpToQp (head f))
      if res == Just False 
      then printN (tail f) (nn+1)
      else printN (tail f) nn 
      

ifqpToQp :: InfeasibleQP -> QP
ifqpToQp (InfeasibleQP (QP u1 u2 u3 u4 u5 u6)) = QP u1 u2 u3 u4 u5 u6
 
