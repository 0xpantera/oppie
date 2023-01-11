module Motion1D where

type R = Double

type Time = R
type TimeInterval = R
type Position = R
type Velocity = R
type Acceleration = R

type PositionFunc = Time -> Position
type VelocityFunc = Time -> Velocity

type Derivative = (R -> R) -> R -> R

derivative :: R -> Derivative
derivative dt pos t = (pos (t + dt/2) - pos (t - dt/2)) / dt

carPosition :: Time -> Position
carPosition t = cos t

carVelocity :: Time -> Velocity
carVelocity = derivative 0.01 carPosition

velFromPos :: R                  -- dt
           -> (Time -> Position) -- position function
           -> (Time -> Velocity) -- velocity function
velFromPos dt pos = derivative dt pos

positionCV :: Position -> Velocity -> Time -> Position
positionCV x0 v0 t = x0 + v0 * t

accFromVel :: R                      -- dt
           -> (Time -> Velocity)     -- velocity function
           -> (Time -> Acceleration) -- acceleration function
accFromVel = derivative

velocityCA :: Velocity -> Acceleration -> Time -> Velocity
velocityCA v0 a0 t = v0 + a0 * t

positionCA :: Position -> Velocity -> Acceleration -> Time -> Position
positionCA x0 v0 a0 t = a0 * t**2 / 2 + v0 * t + x0