module AccParticle where

import Data.Array.Accelerate                    as A hiding (V2)
import Data.Array.Accelerate.Control.Lens
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Vector

-- Types

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

-- Helper functions

calcAccel
  :: Exp PosMass -- The affected particle
  -> Exp PosMass -- The particle causing the effect
  -> Exp Accel
calcAccel pmi pmj = s *^ r
  where
    eps = 0.1
    mj          = posMassMass pmj

    r           = posMassPos pmj - posMassPos pmi
    rsqr        = dot r r + eps * eps
    invr        = 1 / sqrt rsqr
    invr3       = invr * invr * invr

    s           = mj * invr3

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


calcAccels :: Acc (Vector PosMass) -> Acc (Vector Accel)
calcAccels bodies =
  let
    move body = A.sfoldl
      (\acc next -> acc + calcAccel body next)
      0
      (constant Z)
      bodies
  in
    A.map move bodies

runSimulation :: IO ()
runSimulation = do
    beginMonitoring
    (conf, opts, rest) <- parseArgs options defaults header footer

    n           = get configBodyCount conf
    size        = get configWindowSize conf
    fps         = get configRate conf
    epsilon     = get configEpsilon conf
    mass        = get configBodyMass conf
    radius      = get configStartDiscSize conf
    backend     = get optBackend opts

        -- Generate random particle positions in a disc layout centred at
        -- the origin. Start the system rotating with particle speed
        -- proportional to distance from the origin
        --
        positions      <- randomArray (cloud (size,size) radius) (Z :. n)
        masses         <- randomArray (uniformR (1, mass)) (Z :. n)

        let bodies      = run backend
                        $ A.map (setStartVelOfBody . constant $ get configStartSpeed conf)
                        $ A.zipWith setMassOfBody (A.use masses)
                        $ A.map unitBody (A.use positions)

            -- The initial simulation state
            --
            universe    = initialise world
            world       = World { worldBodies   = bodies
                                , worldSteps    = 0
                                , worldTime     = 0 }

            -- Advancing the simulation
            --
            advance     = advanceWorld step
            step        = P.curry
                        $ run1 backend
                        $ A.uncurry
                         $ constant epsilon)


        -- Forward unto dawn
        --
        runTests opts rest
          $ makeTests step

        runBenchmarks opts rest
          [ bench "n-body" $ whnf (advance 0.1) world ]

        runInteractive opts rest
          $ play
              (InWindow "N-Body" (size, size) (10, 10))         -- window size & position
              black                                             -- background colour
              fps                                               -- number of simulation steps per second
              universe                                          -- initial world
              (draw conf)                                       -- fn to convert a world into a picture
              react                                             -- fn to handle input events
              (simulate advance)                                -- fn to advance the world
