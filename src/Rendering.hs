{-# LANGUAGE OverloadedStrings #-}

module Rendering
    ( toSvg
    ) where

import Text.Printf
import Data.Maybe
import Data.List
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import qualified GGPlot as P
import Units

data Guide = X | Y | Color

toSvg :: P.GGPlot -> S.Svg
toSvg plot = S.docTypeSvg ! A.version "1.1" ! A.width "800" ! A.height "800" ! A.viewbox "0 0 100 100" $ do
    -- GeomLine specific
    xGuide []
    yGuide []
    plotPoints $ fromJust $ P.getAes plot


{-renderGeom geom svg = case geom of-}
    {-Line -> renderGuide Y . renderGuide X $ svg-}

xGuide :: [ Double ] -> S.Svg
xGuide ticks = spatialGuide ticks XAxis

yGuide :: [ Double ] -> S.Svg
yGuide ticks = spatialGuide ticks YAxis

data Axis = XAxis | YAxis

spatialGuide :: [ Double ] -> Axis -> S.Svg
spatialGuide ticks axis = do
    S.g $
        let (x1, y1, x2, y2) = case axis of
                XAxis -> ("10", "90", "90", "90")
                YAxis -> ("10", "90", "10", "10")
        in line x1 y1 x2 y2

plotPoints :: P.PlotElement -> S.Svg
plotPoints (P.Aes xs ys cs) = do
    S.g $ do
        mapM_ marker $ zip3 (map toImageSpace xs) (map toImageSpace ys) (categorize cs)

marker :: (ImageSpaceValue, ImageSpaceValue, Integer) -> S.Svg
marker (x, y, color) = do
    S.circle ! A.cx (S.stringValue (show $ toDouble x)) ! A.cy (S.stringValue (show $ toDouble y)) ! A.r "0.5" ! A.fill (S.stringValue $ printf "#%06X" color)

plotLines :: P.PlotElement -> S.Svg
plotLines (P.Aes xs ys cs) =
    let pairs = zip (map toImageSpace xs) (map toImageSpace ys)
        attributeParts = map (\(x, y) -> (show $ toDouble x) ++ "," ++ (show $ toDouble y)) pairs
        attributeString = intercalate " " attributeParts
    in S.polyline ! A.points (S.stringValue attributeString) ! A.fill "none" ! A.stroke "black"

line x1 y1 x2 y2 = do
    S.line ! A.x1 x1 ! A.y1 y1 ! A.x2 x2 ! A.y2 y2 ! A.stroke "black"
