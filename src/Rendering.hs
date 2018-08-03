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
    let plotSpaced = toPlotSpace plot
        P.Aes xs ys _= fromJust $ P.getAes plot
        Aes _ f _ g _ = fromJust $ getAes plotSpaced
        xTicks = generateTicks xs f
        yTicks = generateTicks ys g
    xGuide $ map toImageSpace xTicks
    yGuide $ map toImageSpace yTicks
    plotPoints $ fromJust $ getAes plotSpaced

generateTicks :: [ Double ] -> ( Double -> PlotSpaceValue ) -> [ PlotSpaceValue ]
generateTicks xs f =
    let minTicks = 4
        scale = maximum xs - minimum xs
        step = fromIntegral $ floorToOneOrFive $ scale / minTicks
    in takeWhile (< (f $ maximum xs) ) $ dropWhile (< (f $ minimum xs) ) [ f x | x <- iterate (+ step) (fromIntegral $ floorToOneOrFive $ minimum xs) ]

magnitude :: Double -> Integer
magnitude x =
    (10 ^) $ fromIntegral $ length $ takeWhile (>= 10) $ iterate (/ 10) (abs x)

floorToOneOrFive :: Double -> Integer
floorToOneOrFive x =
    let m = magnitude x
        firstDigit = if ( x / fromIntegral m ) < 5 then 1 else 5
    in firstDigit * m


{-renderGeom geom svg = case geom of-}
    {-Line -> renderGuide Y . renderGuide X $ svg-}

toPlotSpace :: P.GGPlot -> PlotSpaceGGPlot
toPlotSpace ( P.GGPlot xs ) = PlotSpaceGGPlot $ map toPlotSpaceElement xs

toPlotSpaceElement :: P.PlotElement -> PlotSpaceElement
toPlotSpaceElement element = case element of
    P.Aes xs ys cs -> let f = scaleToPlotSpace xs
                          g = scaleToPlotSpace ys 
                      in Aes (map f xs) f (map g ys) g cs
    P.Geom geomtype -> Geom geomtype
    P.XLabel s -> XLabel s

scaleToPlotSpace :: [ Double ] -> Double -> PlotSpaceValue
scaleToPlotSpace xs =
    let emptyRatio = 0.05
        xMax = maximum xs
        xMin = minimum xs
        offset = xMin - ( emptyRatio * ( xMax - xMin ) )
        width = ( xMax - xMin ) * ( 1 + 2 * emptyRatio )
    in \x -> mkPlotSpaceValue $ (x - offset) / width

data PlotSpaceGGPlot = PlotSpaceGGPlot [ PlotSpaceElement ]

data PlotSpaceElement = Aes [ PlotSpaceValue ] ( Double -> PlotSpaceValue ) [ PlotSpaceValue ] ( Double -> PlotSpaceValue ) [ String ]
                      | Geom P.GeomType
                      | XLabel String

getAes :: PlotSpaceGGPlot -> Maybe PlotSpaceElement
getAes (PlotSpaceGGPlot elements) = find (\e -> case e of
                                            Aes _ _ _ _ _ -> True
                                            _ -> False
                                        ) elements

xGuide :: [ ImageSpaceValue ] -> S.Svg
xGuide ticks = spatialGuide ticks XAxis

yGuide :: [ ImageSpaceValue ] -> S.Svg
yGuide ticks = spatialGuide ticks YAxis

data Axis = XAxis | YAxis

spatialGuide :: [ ImageSpaceValue ] -> Axis -> S.Svg
spatialGuide ticks axis = do
    S.g $ do
        let (x1, y1, x2, y2) = case axis of
                XAxis -> ("10", "90", "90", "90")
                YAxis -> ("10", "90", "10", "10")
        line x1 y1 x2 y2
        mapM_ (mkTick axis) ticks

mkTick :: Axis -> ImageSpaceValue -> S.Svg
mkTick axis value = do
    let v = S.stringValue $ show $ toDouble value
        (x1, y1, x2, y2) = case axis of
            XAxis -> (v, "90", v, "92")
            YAxis -> ("10", v, "8", v)
    line x1 y1 x2 y2


plotPoints :: PlotSpaceElement -> S.Svg
plotPoints (Aes xs f ys h cs) = do
    S.g $ do
        mapM_ marker $ zip3 (map toImageSpace xs) (map toImageSpace ys) (categorize cs)

marker :: (ImageSpaceValue, ImageSpaceValue, Integer) -> S.Svg
marker (x, y, color) = do
    S.circle ! A.cx (S.stringValue (show $ toDouble x)) ! A.cy (S.stringValue (show $ toDouble y)) ! A.r "0.5" ! A.fill (S.stringValue $ printf "#%06X" color)

plotLines :: PlotSpaceElement -> S.Svg
plotLines (Aes xs f ys h cs) =
    let pairs = zip (map toImageSpace xs) (map toImageSpace ys)
        attributeParts = map (\(x, y) -> (show $ toDouble x) ++ "," ++ (show $ toDouble y)) pairs
        attributeString = intercalate " " attributeParts
    in S.polyline ! A.points (S.stringValue attributeString) ! A.fill "none" ! A.stroke "black"

line x1 y1 x2 y2 = do
    S.line ! A.x1 x1 ! A.y1 y1 ! A.x2 x2 ! A.y2 y2 ! A.stroke "black"
