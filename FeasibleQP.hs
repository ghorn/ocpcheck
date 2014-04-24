{-# OPTIONS_GHC -Wall #-}

module FQP where

import Test.QuickCheck.Arbitrary ( Arbitrary(..))
import Test.QuickCheck.Gen


import QP

data FeasibleQP = FeasibleQP QP deriving(Show)

instance Arbitrary FeasibleQP where
  arbitrary = do
    k1 <- infiniteListOf (suchThat (arbitrary :: Gen Double) (0 /=))
    let (kq,r1) = splitAt (n*n) k1
        (kc,r2) = splitAt n r1
        (ka,r3) = splitAt (n*m) r2
        kx = take n r3
    gb <- suchThatG ka kx
    xb <- suchThatV kx
    x <- suchThatX xb
    return (FeasibleQP QP { q = kq, c = kc, a = ka, gBounds = gb, xBounds = xb, x0 = x})
    where
      suchThatV :: [Double] -> Gen [(Double,Double)]
      suchThatV k 
        | k == [] = return []
        | otherwise = do
          l <- suchThat (arbitrary :: Gen Double) ((head k) >)
          u <- suchThat (arbitrary :: Gen Double) ((head k) <)
          tl <- suchThatV (tail k)
          return ((l,u): tl)
      
      suchThatG :: [Double] -> [Double] -> Gen [(Double, Double)]
      suchThatG aa y 
        | aa == [] = return []
        | otherwise = do
          l <- suchThat (arbitrary :: Gen Double) (<z)
          u <- suchThat (arbitrary :: Gen Double) (>z)
          end <- suchThatG tl y
          return ((l,u) : end)
          where
            (hd, tl) = splitAt n aa
            z = sum (zipWith (*) hd y)
       

      suchThatX :: [(Double, Double)] -> Gen [Double]
      suchThatX bd 
        | bd == [] = return []
        | otherwise = do
          x1 <- choose (l,u)
          end <- suchThatX tl
          return (x1 : end)
          where
            (l,u):tl = bd
  

feasibleQp :: IO()
feasibleQp = do
  fqp <- (sample' (arbitrary :: Gen FeasibleQP))
  printN fqp 0
  putStrLn $ "out of " ++ show (length fqp)
  where
    printN :: [FeasibleQP] -> Int -> IO ()
    printN [] nn = print nn
    printN f nn = do
      res <- canSolve (fqpToQp (head f))
      if res == Just True 
      then printN (tail f) (nn+1)
      else printN (tail f) nn
      


fqpToQp :: FeasibleQP -> QP
fqpToQp (FeasibleQP (QP u1 u2 u3 u4 u5 u6)) = QP u1 u2 u3 u4 u5 u6
