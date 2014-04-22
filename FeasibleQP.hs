{-# OPTIONS_GHC -Wall #-}

module FQP where

import Test.QuickCheck.Arbitrary ( Arbitrary(..), vector )
import Test.QuickCheck.Gen

import Dyno.Vectorize
import Dyno.Nlp
import Dyno.NlpSolver
import Dyno.Ipopt
import Dyno.Casadi.SXElement

n :: Int
n = 3 
m :: Int
m = 2 

data X a = X a a a deriving (Functor, Generic1, Show)
data G a = G a a deriving (Functor, Generic1, Show)

instance Vectorize X
instance Vectorize G

data FeasibleQP = FeasibleQP { q :: [Double]
                             , c :: [Double]
                             , a :: [Double]
                             , gBounds :: [(Double,Double)]
                             , xBounds :: [(Double, Double)]
                             , x0 :: [Double]
                             } deriving (Show, Eq)

instance Arbitrary FeasibleQP where
  arbitrary = do
    kq <- vector (n*n)
    kc <- vector n
    ka <- vector (n*m)
    kx <- vector n
    gb <- suchThatG ka kx
    xb <- suchThatV kx
    x <- suchThatX xb
    return (FeasibleQP { q = kq, c = kc, a = ka, gBounds = gb, xBounds = xb, x0 = x})
    where
      suchThatV :: [Double] -> Gen [(Double,Double)]
      suchThatV k 
        | k == [] = return []
        | otherwise = do
          l <- suchThat (return (head k)) ((head k) >)
          u <- suchThat (return (head k)) ((head k) <)
          tl <- suchThatV (tail k)
          return ((l,u): tl)
      
      suchThatG :: [Double] -> [Double] -> Gen [(Double, Double)]
      suchThatG aa y 
        | aa == [] = return []
        | otherwise = do
          l <- suchThat (return z) (<z)
          u <- suchThat (return z) (>z)
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
            
problem :: FeasibleQP -> Nlp X None G SXElement
problem fqp =  Nlp { nlpFG = fg
                   , nlpBX = bx
                   , nlpBG = bg
                   , nlpX0 = x0
                   , nlpP = None
                   }

  [q11,q12,q13,q21,q22,q23,q31,q32,q33] = map realToFrac (q fpq) :: [SXElement]
  [c1,c2,c3] = map realToFrac (c fpq) :: [SXElement]
  [a11,a12,a13,a21,a22,a23,a31,a32,a33] = map realToFrac (a fpq) :: [SXElement]
  [gb1,gb2] = gBounds fpq
  [xb1,xb2,xb3] = xBounds fpq
  x0 :: X Double
  x0 = X x1 x2 x3 

  bx :: X Bounds
  bx = X (Just (fst xb1), Just (snd xb1)) (Just (fst xb2), Just (snd xb2)) (Just (fst xb3), Just (snd xb3)) 

  bg :: G Bounds
  bg = G (Just (fst gb1), Just (snd gb1)) (Just (fst gb2), Just (snd gb2)) 

  fg :: X SXElement -> None SXElement -> (SXElement, G SXElement)
  fg (X aa bb cc ) _ = (f,g)
    where
      f = aa  * q11 * aa  + aa  * q12 * bb  + aa  * q13 * cc  + bb  * q21 * aa  + bb  * q22 * bb  + bb  * q23 * cc  + cc  * q31 * aa  + cc  * q32 * bb  + cc  * q33 * cc  + c1 * aa  + c2 * bb  + c3 * cc 
      g = G (a11 * aa  + a12 * bb  + a13 * cc ) (a21 * aa  + a22 * bb  + a23 * cc ) 



canSolve :: FeasibleQP -> Bool
canSolve fqp = solved
  where
    do
      opt <- solveNlp quietIpoptSolver (problem fqp) Nothing
      return ()
    solved = fst opt == Right "Solve_Succeeded"
             
  
