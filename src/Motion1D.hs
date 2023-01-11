module Motion1D where

type R = Double

type Time = R
type TimeInterval = R
type Position = R
type Velocity = R

type PositionFunc = Time -> Position
type VelocityFunc = Time -> Velocity