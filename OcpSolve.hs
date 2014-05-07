{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language InstanceSigs #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module OcpSolve ( ) where

import Data.Vector ( Vector )
import PureOcp ( createFeas , Ocp(..) , FeasibleOcp(..) , createFeas'' )
import Dyno.Vectorize
import Dyno.View
import Dyno.Ipopt
--import Dyno.Casadi.SXElement
import Dyno.Nlp
import Dyno.NlpSolver

import Dyno.Ocp
import Dyno.DirectCollocation
import Dyno.Cov
--import Dyno.DirectCollocation.Dynamic ( toMeta, ctToDynamic )

import Dyno.Nats

data X a = X a a a deriving (Functor, Generic1, Show)
data U a = U a a deriving (Functor, Generic1, Show)

instance Vectorize X
instance Vectorize U

mayer :: Floating a => a -> X a -> X a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer _ _ _ _ _ = 0

lagrange :: Floating a => X a -> None a -> U a -> None a -> None a -> a -> a
lagrange (X x y z) _ (U u v) _ _ _ = u*u + v*v -- + x**2 + y**2 + z**2

myDae :: Floating a => Ocp D3 D2 -> Dae X None U None X None a
myDae myOcp (X x' y' z') (X x y z) _ (U u v) _ _ = (force, None)
  where
    [q11,q12,q13,q21,q22,q23,q31,q32,q33] = map realToFrac (a myOcp) 
    [c11,c12,c21,c22,c31,c32] = map realToFrac (b myOcp) 
    force = X (x' - q11*x - q12*y - q13*z - c11*u - c12*v)
              (y' - q21*x - q22*y - q23*z - c21*u - c22*v)
              (z' - q31*x - q32*y - q33*z - c31*u - c32*v)

bc :: Floating a => Ocp D3 D2 -> X a -> X a -> X a
bc myOcp (X x0 y0 z0) (X xF yF zF) = X (limits x0 xF x0' xF') (limits y0 yF y0' yF') (limits z0 zF z0' zF')
  where
    [x0',y0',z0'] = map realToFrac $ xInit myOcp
    [xF',yF',zF'] = map realToFrac $ xFinal myOcp
    limits :: Floating a => a -> a -> a -> a -> a
    limits x y x' y' = abs (x-x') + abs (y-y')

pathc :: x a -> z a -> u a -> p a -> None a -> a -> None a
pathc _ _ _ _ _ _ = None

ocp :: Ocp D3 D2 -> OcpPhase X None U None X None X None JNone JNone JNone
ocp myOcp = OcpPhase { ocpMayer = mayer
               , ocpLagrange = lagrange
               , ocpDae = myDae myOcp
               , ocpBc = bc myOcp
               , ocpPathC = pathc
               , ocpPathCBnds = None
               , ocpBcBnds = fill (Just 0, Just 0)
               , ocpXbnd = fill (Nothing, Nothing)
               , ocpUbnd = fill (Nothing, Nothing)
               , ocpZbnd = None
               , ocpPbnd = None
               , ocpTbnd = (Just 4, Just 4)

               , ocpSq = 0
               , ocpSbnd = jfill (Nothing,Nothing)
               , ocpSbc = \_ _ -> cat JNone
               , ocpSbcBnds = cat JNone
               , ocpSh = \_ _ -> cat JNone
               , ocpShBnds = cat JNone
               }

main :: IO()
main = do
  putStrLn "What seed ?"
  seed <- getLine
  let s = read seed
      myocp = createFeas'' s :: Ocp D3 D2
      --ocptest = Ocp [1,1,1,0,1,0,0,0,1] [1,0,2,3,-1,-1] [0,0,0] [1,2,1]
      guess = jfill 0 :: J (CollTraj X None U None JNone D100 D3) (Vector Double)
      --cb' :: J (CollTraj X None U None JNone D100 D2) (Vector Double) -> IO Bool
      --cb' traj = cb (ctToDynamic traj, toMeta traj)
  nlp <- makeCollNlp $ ocp myocp

  (msg,opt') <- solveNlp' ipoptSolver (nlp { nlpX0' = guess }) Nothing
  print myocp
  opt <- case msg of Left msg' -> error msg'
                     Right _ -> return opt'
  return ()
