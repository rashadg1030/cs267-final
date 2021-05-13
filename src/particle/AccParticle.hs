module AccParticle where

import Data.Array.Accelerate.Linear.V2


type Time = Float

type Vel = V2 Float

type Accel = V2 Float

type Mass = Float

type Pos = V2 Float

type PosMass = (Pos, Mass)

type Particle = (PosMass, Vel, Accel)
