module Main where
import Graphics.Gnuplot.Simple

type R = Double

tRange :: [R]
tRange = [0, 0.01 .. 5]

yPos :: R -- y0
     -> R -- vy0
     -> R -- ay
     -> R -- t
     -> R --y
yPos y0 vy0 ay t = y0 + vy0 * t + ay * t**2 / 2

plot1 :: IO ()
plot1 = plotFunc [ Title "Projectile Motion"
                 , XLabel "Time (s)"
                 , YLabel "Height of projectile (m)"
                 , PNG "projectile.png"
                 , Key Nothing
                 ] tRange (yPos 0 20 (-9.8))

customLabel :: (R, R) -> String -> Attribute
customLabel (x, y) label = 
  Custom "label" ["\"" ++ label ++ "\"" ++ " at "
                  ++ show x ++ "," ++ show y]

plotCustom :: IO ()
plotCustom =
  plotFunc [ Title "Projectile Motion"
           , XLabel "Time (s)"
           , YLabel "Height of projectile (m)"
           , customLabel (1.5, 22) "Peak Height"
           , PNG "projectileLabel.png"
           , Key Nothing
           ] tRange (yPos 0 20 (-9.8))

plotData :: IO ()
plotData = 
  plotPath [ Title "Projectile Motion"
           , XLabel "Time (s)"
           , YLabel "Height of projectile (m)"
           , customLabel (1.5, 22) "Peak Height"
           , PNG "projectileData.png"
           , Key Nothing
           ] [(t, yPos 0 20 (-9.8) t) | t <- tRange]

xRange :: [R]
xRange = [0, 0.02 .. 10]

f3 :: R -> R
f3 x = exp (-x)

usePlotFuncs :: IO ()
usePlotFuncs = plotFuncs [ XRange (-2, 8)
                         , YRange (-0.2, 1)
                         , PNG "multipleFuncs.png"
                         , Key Nothing
                         ] xRange [cos, sin, f3]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  usePlotFuncs