module SimpleVec where

type R = Double
type Time   = R
type PosVec = Vec
type Velocity = Vec
type Acceleration = Vec

type VecDerivative = (R -> Vec) -> R -> Vec

vecDerivative :: R -> VecDerivative
vecDerivative dt v t = (v (t + dt/2) ^-^ v (t - dt/2)) ^/ dt

data Vec = Vec { xComp :: R -- x component
               , yComp :: R -- y component
               , zComp :: R -- z component
               } deriving (Eq)

instance Show Vec where
    show (Vec x y z) = 
        "vec " ++ showDouble x ++ " " ++ showDouble y ++ " " ++ showDouble z

showDouble :: R -> String
showDouble x
    | x < 0 = "(" ++ show x ++ ")"
    | otherwise = show x

-- Form a vector by giving its components
vec :: R -- x component
    -> R -- y component
    -> R -- z component
    -> Vec
vec = Vec

-- names borrowed from Conal Elliott's vector-space package
infixl 6 ^+^
infixl 6 ^-^
infixr 7 *^
infixl 7 ^*
infixr 7 ^/
infixr 7 <.>
infixl 7 ><

-- unit vectors in the x, y, and z directions
iHat :: Vec
iHat = vec 1 0 0

jHat :: Vec
jHat = vec 0 1 0

kHat :: Vec
kHat = vec 0 0 1

-- zero vector
zeroV :: Vec
zeroV = vec 0 0 0

negateV :: Vec -> Vec
negateV (Vec x y z) = vec (-x) (-y) (-z)

-- vector addition
(^+^) :: Vec -> Vec -> Vec
Vec ax ay az ^+^ Vec bx by bz  = 
    vec (ax + bx) (ay + by) (az + bz)

-- vector subtraction
(^-^) :: Vec -> Vec -> Vec
Vec ax ay az ^-^ Vec bx by bz = 
    vec (ax - bx) (ay - by) (az - bz)

sumV :: [Vec] -> Vec
sumV = foldr (^+^) zeroV

-- scalar multiplication
(*^) :: R -> Vec -> Vec
c *^ Vec ax ay az = vec (c * ax) (c * ay) (c * az)

-- scalar multiplication
(^*) :: Vec -> R -> Vec
(^*) = flip (*^)

-- dot product
(<.>) :: Vec -> Vec -> R
Vec ax ay az <.> Vec bx by bz = ax * bx + ay * by + az * bz

-- cross product
(><) :: Vec -> Vec -> Vec
Vec ax ay az >< Vec bx by bz = 
    vec (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

-- scalar division
(^/) :: Vec -> R -> Vec
Vec ax ay az ^/ c = vec (ax / c) (ay / c) (az / c)

-- magnitude of a vector
magnitude :: Vec -> R
magnitude v = sqrt (v <.> v)

velFromPosV :: R                  -- dt
            -> (Time -> PosVec)   -- position function
            -> (Time -> Velocity) -- velocity function
velFromPosV = vecDerivative

accFromVelV :: R                      -- dt
            -> (Time -> Velocity)     -- velocity function
            -> (Time -> Acceleration) -- acceleration function
accFromVelV = vecDerivative

positionCVV :: PosVec -> Velocity -> Time -> PosVec
positionCVV r0 v0 t = r0 ^+^ v0 ^* t

velocityCAV :: Velocity -> Acceleration -> Time -> Velocity
velocityCAV v0 a0 t = v0 ^+^ a0 ^* t

positionCAV :: PosVec -> Velocity -> Acceleration -> Time -> PosVec
positionCAV r0 v0 a0 t = 0.5 *^ t**2 *^ a0 ^+^ v0 ^* t ^+^ r0

aParallel :: Vec -> Vec -> Vec
aParallel v a = let vHat = v ^/ magnitude v
                in (vHat <.> a) *^ vHat

aPerp :: Vec -> Vec -> Vec
aPerp v a = let vHat = v ^/ magnitude v
            in a ^-^ (vHat <.> a) *^ vHat
            
                       