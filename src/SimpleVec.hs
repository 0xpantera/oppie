module SimpleVec where

type R = Double
type Time   = R
type PosVec = Vec
type Velocity = Vec
type Acceleration = Vec

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
    -> Vec x y z
vec = Vec

-- names borrowed from Conal Elliott's vector-space package
infixl 6 ^+^
infixl 6 ^-^
infixl 7 *^
infixl 7 ^*
infixr 7 ^/
infixr 7 <.>
infixr 7 ><

-- TODO: define operators for vectors
