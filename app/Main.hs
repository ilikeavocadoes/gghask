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
        field = [0,(0.1)..30]
        xs1 = map fst $ signal field
        ys1 = map snd $ signal field
        xs2 = map fst $ signal field
        ys2 = map (+ 2) $ map snd $ signal field
        xs3 = map fst $ signal field
        ys3 = map (+ 0.2) $ map snd $ signal field
        {-colors = take (length xs) $ cycle ["tokio", "kioto", "nagoya"]-}

        xs = xs1 ++ xs2 ++ xs3
        ys = ys1 ++ ys2 ++ ys3
        colors = take (length xs1) (repeat "tokio") ++ take (length xs2) (repeat "kioto") ++ take (length xs3) (repeat "nagoya")
    in ggplot [ aes ! x xs ! y ys ! color colors
              , geomLine
              , xlab "label for x"
              ]

