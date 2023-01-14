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

type Integration = (R -> R) -- function
                -> R        -- lower limit
                -> R        -- upper limit
                -> R        -- result

integral :: R -> Integration
integral dt f a b = sum [f t * dt | t <- [a+dt/2, a+3*dt/2 .. b - dt/2]]

type AntiDerivative =  R       -- initial value
                   -> (R -> R) -- function
                   -> (R -> R) -- antiderivative of function

antiDerivative :: R -> AntiDerivative
antiDerivative dt v0 a t = v0 + integral dt a 0 t            

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

velFromAcc :: R                      -- dt
           -> Velocity               -- initial velocity
           -> (Time -> Acceleration) -- acceleration function
           -> (Time -> Velocity)     -- velocity function
velFromAcc dt v0 a t = antiDerivative dt v0 a t

posFromVel :: R                  -- dt
           -> Position           -- initial position
           -> (Time -> Velocity) -- velocity function
           -> (Time -> Position) -- position function
posFromVel = antiDerivative

integralN' :: Int -> Integration
integralN' n f a b =
    integral dt f a b
    where dt = (b - a) / fromIntegral n

oneStep :: R        -- time step
        -> (R -> R) -- function to integrate
        -> (R,R)    -- current (t,y)
        -> (R,R)    -- updated (t,y)
oneStep dt f (t,y) = let t' = t + dt
                         y' = y + dt * f t
                     in (t',y')

integral' :: R -> Integration
integral' dt f a b = 
    snd $ 
    head $ 
    dropWhile ((< b) . fst) $ 
    iterate (oneStep dt f) (a + dt/2, 0)                  