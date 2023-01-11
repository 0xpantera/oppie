module Motion1D where

type R = Double

type Time = R
type TimeInterval = R
type Position = R
type Velocity = R

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

