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
derivative dt x t = (x (t + dt/2) - x(t - dt/2)) / dt