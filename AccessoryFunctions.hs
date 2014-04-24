{-# OPTIONS_GHC -Wall #-}


module AccessoryFunctions 
( suchThatVF
, suchThatGF
, suchThatXF
, suchThatVI
, suchThatGI
, suchThatXI
, createBounds
) where

import Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import Test.QuickCheck.Gen

--For feasible QP

suchThatVF :: [Double] -> Gen [(Double,Double)]
suchThatVF k 
  | k == [] = return []
  | otherwise = do
    l <- suchThat (arbitrary :: Gen Double) ((head k) >)
    u <- suchThat (arbitrary :: Gen Double) ((head k) <)
    tl <- suchThatVF (tail k)
    return ((l,u): tl)
      
suchThatGF :: Int -> [Double] -> [Double] -> Gen [(Double, Double)]
suchThatGF n aa y 
  | aa == [] = return []
  | otherwise = do
    l <- suchThat (arbitrary :: Gen Double) (<z)
    u <- suchThat (arbitrary :: Gen Double) (>z)
    end <- suchThatGF n tl y
    return ((l,u) : end)
    where
      (hd, tl) = splitAt n aa
      z = sum (zipWith (*) hd y)
       

suchThatXF :: [(Double, Double)] -> Gen [Double]
suchThatXF bd 
  | bd == [] = return []
  | otherwise = do
    x1 <- choose (l,u)
    end <- suchThatXF tl
    return (x1 : end)
    where
      (l,u):tl = bd

--for infeasible QP

suchThatVI :: [Double] -> Gen [(Double,Double)]
suchThatVI k 
  | k == [] = return []
  | otherwise = do
    l <- suchThat (arbitrary :: Gen Double) ((head k) >)
    u <- suchThat (arbitrary :: Gen Double) ((head k) <)
    tl <- suchThatVI (tail k)
    return ((l,u): tl)
    
createBounds :: Int -> [(Double, Double)] -> [Double] -> Gen [(Double, Double)]
createBounds n xx aa 
  | aa == [] = return []
  | otherwise = do
    end <- createBounds n xx tl
    return ((lx,ux):end)
    where
      (hd, tl) = splitAt n aa
      lx = sum $ zipWith min (zipWith (*) (map fst xx) hd) (zipWith (*) (map snd xx) hd)
      ux = sum $ zipWith max (zipWith (*) (map fst xx) hd) (zipWith (*) (map snd xx) hd)
  
suchThatGI :: [(Double, Double)] -> Int -> Gen [(Double, Double)]
suchThatGI y i
  | y == [] = return []
  | i == 1 = do
    up <- suchThat (arbitrary :: Gen Bool) (\_ -> True)
    l <- suchThat (arbitrary :: Gen Double) (\w -> if up then (w > snd (head y)) else (w < fst (head y)))
    u <- suchThat (arbitrary :: Gen Double) (\w -> if up then (w > snd (head y)) else (w < fst (head y)))
    end <- suchThatGI (tail y) (i-1)
    return ((min l u, max l u) : end)
  | otherwise = do
    l <- suchThat (arbitrary :: Gen Double) (\_ -> True)
    u <- suchThat (arbitrary :: Gen Double) (\_ -> True)
    end <- suchThatGI (tail y) (i-1)
    return ((min l u, max l u) : end)
  
suchThatXI :: [(Double, Double)] -> Gen [Double]
suchThatXI bd 
  | bd == [] = return []
  | otherwise = do
    x1 <- choose (l,u)
    end <- suchThatXI tl
    return (x1 : end)
    where
      (l,u):tl = bd



