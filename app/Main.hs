module Main where

import GGPlot
import Rendering
import Text.Blaze.Svg.Renderer.Pretty (renderSvg)

main :: IO ()
main = do
  let svg = renderSvg $ toSvg plot
  writeFile "out.svg" svg
  putStrLn svg


signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

plot :: GGPlot
plot =
    let
        field = [0,(0.1)..4]
        xs = map fst $ signal field
        ys = map snd $ signal field
        scaledX = map (* 0.25) xs
        scaledY = map (\x -> x * 0.5 + 0.2) ys
        colors = take (length xs) $ cycle ["tokio", "kioto", "nagoya"]
    in ggplot [ aes ! x scaledX ! y scaledY ! color colors
              , geomPoint
              , xlab "label for x"
              ]

