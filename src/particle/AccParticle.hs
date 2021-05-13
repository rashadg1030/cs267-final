module AccParticle where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Control.Lens
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Vector


type Time = Float

type Vel = V2 Float

type Accel = V2 Float

type Mass = Float

type Pos = V2 Float

type PosMass = (Pos, Mass)

type Particle = (PosMass, Vel, Accel)

data World = World
  { worldParticles :: !(Vector Particle)
  , worldSteps :: {-# UNPACK #-} !Int
  , worldTime :: {-# UNPACK #-} !Time
  }

advanceParticles
  :: (Acc (Vector PosMass) -> Acc (Vector Accel))
  -> Acc (Scalar Time)
  -> Acc (Vector Particle)
  -> Acc (Vector Particle)
advanceParticles calcAccels timeStep ps =
  let
    accels = calcAccels $ A.map pointMassOfBody ps
    advance b a = advanceBody (the timeStep) (setAccelOfBody a b)
  in
    A.zipWith advance ps accels

advanceWorld
    :: (Scalar Time -> Vector Particle -> Vector Particle)
    -> Time
    -> World
    -> World
advanceWorld advance timeStep world =
  let
    -- Update the bodies
    particles' = advance (fromList Z [timeStep]) (worldParticles world)

    -- Update the world
    steps'  = worldSteps world + 1
    time'   = worldTime  world + timeStep

  in
    world { worldParticles = particles'
          , worldSteps = steps'
          , worldTime = time'
          }

putParticle :: Exp Pos -> Exp Particle
putParticle pos = body
  where
    pointmass = lift (pos, constant 1)                    :: Exp PointMass
    body      = lift (pointmass, constant 0, constant 0)  :: Exp Body

particleVel :: Exp Particle -> Exp Vel
particleVel = view _2


-- | Take the Acceleration of a Body
--
particleAccel :: Exp Particle -> Exp Accel
particleAccel = view _3


-- | Take the PointMass of a Body
--
particlePosMass :: Exp Particle -> Exp PosMass
particlePosMass = view _1

posMassPos :: Exp PosMass -> Exp Pos
posMassPos = A.fst

massPosMass :: Exp PointMass -> Exp Mass
massPosMass = A.snd
