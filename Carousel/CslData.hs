{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module CslData ( CX(..) , U(..) , diff , modelForces' , Cdata (..), modelCsl ) where

import GHC.Generics
import Linear

import Dyno.Vectorize
import Dyno.Server.Accessors ( Lookup(..) )


data CX a = CX { deltaDot :: a
               , alpha :: a
               , alphaDot :: a
               , beta :: a
               , betaDot :: a
               } deriving (Eq, Functor, Generic, Generic1, Show)
data U a = U { control :: a } deriving (Eq, Functor, Generic, Generic1, Show)

data Cdata a = Cdata { profile :: a
                     , rho :: a
                     , coeff :: a
                     , lengtArm :: a
                     , radius :: a
                     , mass :: a
                     } deriving (Eq, Functor, Generic, Generic1, Show)

instance Vectorize CX
instance Vectorize U
instance Vectorize Cdata

instance (Lookup a, Generic a) => Lookup (CX a)
instance (Lookup a, Generic a) => Lookup (U a)

diff :: forall a . (Floating a) => CX a -> CX a -> CX a
diff (CX d0 alp0 alpd0 b0 bd0) (CX df alpf alpdf bf bdf) =
  CX (d0-df) (alp0-alpf) (alpd0-alpdf) (b0-bf) (bd0-bdf)

modelCsl :: Floating a => Cdata a
modelCsl = Cdata { profile = pi * (0.65/(2*pi))**2
                   , rho = 1.29
                   , coeff = 0.7
                   , lengtArm = 1.8
                   , radius = 1.94
                   , mass = 0.436
                   }

g :: Floating a => a
g = 9.81

modelForces' ::forall a . (Floating a) => CX a -> CX a -> U a -> a -> a -> CX a
modelForces' (CX d' alp' alpd' b' bd') (CX d alp alpd b bd) (U u) kPhi _kTheta =
  CX (d' - u) (alpd - alp') resA (bd - b') resB
  where
    theta = pi/2 - alp
    td = -alpd
    phi = pi/2 - b
    pd = -bd
    f = 0.5 * roh * pA * cw * (norm v)
    V3 f1 f2 f3 = V3 (f*vx) (f*vy) (f*vz)
    Cdata pA roh cw r0 l m = modelCsl

    resA = m*r0*(-alpd') - m*r0*cos theta *sin theta * pd**2 + sin theta *( m*g+f3) - sin phi * cos theta * f2 - m*l*sin phi *cos theta * d**2 - m*cos theta*d**2 *r0*sin theta +2* cos theta *sin theta * m*r0*d*pd - cos phi * cos theta * (f1+m*u*l) - kPhi * pd

    resB = l*m*cos phi * d**2 - l*m*sin phi * u+2*m*d*r0*cos theta*td - 2*m*r0*cos theta * td*pd - m*u*r0*sin theta +cos phi *f2 -sin phi *f1-m*r0*(-bd')*sin theta
    
    vSpin = V3 (d*(l+r0*sin theta *sin phi)) (-d*r0*sin theta*cos phi) 0
    vBall = V3 (r0*cos theta*td*cos phi -r0*sin theta*sin phi *pd)
               (r0*cos theta*td*sin phi + r0*sin theta *cos phi *pd)
               (-r0*sin theta*td)
    v = - (vSpin+vBall)
    V3 vx vy vz = v
