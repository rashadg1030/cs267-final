module AccParticle where

import Data.Array.Accelerate                    as A hiding (V2)
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
    accels = calcAccels $ A.map particlePosMass ps
    advance b a = moveParticle (the timeStep) (setParticleAccel a b)
  in
    A.zipWith advance ps accels

advanceWorld
    :: (Scalar Time -> Vector Particle -> Vector Particle)
    -> Time
    -> World
    -> World
advanceWorld advance timeStep world =
  let
    particles' = advance (fromList Z [timeStep]) (worldParticles world)

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
    posmass = lift (pos, constant 1) :: Exp PosMass
    body = lift (posmass, constant 0, constant 0)  :: Exp Particle

particleVel :: Exp Particle -> Exp Vel
particleVel = view _2

particleAccel :: Exp Particle -> Exp Accel
particleAccel = view _3

particlePosMass :: Exp Particle -> Exp PosMass
particlePosMass = view _1

posMassPos :: Exp PosMass -> Exp Pos
posMassPos = A.fst

posMassMass :: Exp PosMass -> Exp Mass
posMassMass = A.snd


setParticleMass :: Exp Mass -> Exp Particle -> Exp Particle
setParticleMass mass p = lift (posMass, vel, acc)
  where
    vel = particleVel p
    acc = particleAccel p
    pos = posMassPos (particlePosMass p)
    posMass = lift (pos, mass) :: Exp PosMass


setParticleAccel :: Exp Accel -> Exp Particle -> Exp Particle
setParticleAccel acc p = lift (pm, vel, acc)
  where
    pm          = particlePosMass p
    vel         = particleVel p


setParticleStartVel :: Exp Float -> Exp Particle -> Exp Particle
setParticleStartVel startVel p = lift (pm, vel'', acc)
  where
    pm          = particlePosMass p
    acc         = particleAccel p
    pos         = posMassPos pm

    pos'        = normalize pos
    vel'        = lift (V2 y' (-x'))

    vel''       = (sqrt (norm pos) * startVel) *^ vel'

    V2 x' y' = unlift pos' :: V2 (Exp Float)


moveParticle :: Exp Time -> Exp Particle -> Exp Particle
moveParticle time p = lift ( pm', vel', acc )
  where
    pm          = particlePosMass p
    pos         = posMassPos pm
    vel         = particleVel p
    acc         = particleAccel p
    mass        = posMassMass pm

    pm'         = lift (pos', mass) :: Exp PosMass
    pos'        = pos + time *^ vel
    vel'        = vel + time *^ acc
