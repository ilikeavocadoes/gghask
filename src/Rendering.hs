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
import Data.List.Unique as U
import Units


data Guide = X | Y | Color

toSvg :: P.GGPlot -> S.Svg
toSvg plot = S.docTypeSvg ! A.version "1.1" ! A.width "800" ! A.height "800" ! A.viewbox "0 0 100 100" $ do
    S.style "text { font-size: 2px; }\
            \line { stroke-width: 0.25px; }"
    let plotSpaced = toPlotSpace plot
        P.Aes xs ys _= fromJust $ P.getAes plot
        Aes _ f _ g _ = fromJust $ getAes plotSpaced
        xTicks = generateTicks xs f
        yTicks = generateTicks ys g
        geoms = map (\(P.Geom t) -> t) $ P.getGeoms plot
    xGuide $ map (\(v, s) -> (toImageSpace False v, s)) xTicks
    yGuide $ map (\(v, s) -> (toImageSpace True v, s)) yTicks
    mapM_ (plotGeom $ fromJust $ getAes plotSpaced) geoms

plotGeom :: PlotSpaceElement -> P.GeomType -> S.Svg
plotGeom aes geom = case geom of
    P.Line -> plotLines aes
    P.Point -> plotPoints aes

generateTicks :: [ Double ] -> ( Double -> PlotSpaceValue ) -> [ ( PlotSpaceValue, String ) ]
generateTicks xs f =
    let minTicks = 4
        scale = maximum xs - minimum xs
        maxStep = scale / minTicks
        step = floorToOneOrFive maxStep
    in takeWhile (\x -> fst x < (f $ maximum xs) ) $ dropWhile (\x -> fst x < (f $ minimum xs - 0.05 * scale) ) [ (f $ fromRational x, show $ fromRational x) | x <- iterate ((floorToStepSize step) . (+ step)) (toRational $ minimum xs) ]


magnitude :: Double -> Rational
magnitude 0 = 1
magnitude x =
    let (iterator, negativeSign) = if x > 1 then ((/ 10), False ) else ((* 10), True)
        exponent = (10 ^) $ fromIntegral $ length $ takeWhile (\n -> (n >= 10) || (n < 1)) $ take 4 $ iterate iterator (abs x)
    in if negativeSign then 1/exponent else exponent

floorToStepSize :: Rational -> Rational -> Rational
floorToStepSize stepSize x = fromRational $ (* stepSize) $ toRational $ floor $ x / stepSize

floorToZeroOrFive :: Double -> Rational
floorToZeroOrFive x =
    let m = magnitude x
        firstDigit = if ( toRational x / m ) < 5 then 0 else 5
    in firstDigit * m

floorToOneOrFive :: Double -> Rational
floorToOneOrFive x =
    let m = magnitude x
        firstDigit = if ( toRational x / m ) < 5 then 1 else 5
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

xGuide :: [ ( ImageSpaceValue, String ) ] -> S.Svg
xGuide ticks = spatialGuide ticks XAxis

yGuide :: [ ( ImageSpaceValue, String ) ] -> S.Svg
yGuide ticks = spatialGuide ticks YAxis

data Axis = XAxis | YAxis

spatialGuide :: [ (ImageSpaceValue, String) ] -> Axis -> S.Svg
spatialGuide ticks axis = do
    S.g $ do
        let (x1, y1, x2, y2) = case axis of
                XAxis -> ("10", "90", "90", "90")
                YAxis -> ("10", "90", "10", "10")
        line x1 y1 x2 y2
        mapM_ (\t -> mkTick axis (fst t) (snd t)) ticks

mkTick :: Axis -> ImageSpaceValue -> String -> S.Svg
mkTick axis value label = do
    let v = S.stringValue $ show $ toDouble value
        (x1, y1, x2, y2) = case axis of
            XAxis -> (v, "90", v, "91")
            YAxis -> ("10", v, "9", v)
    line x1 y1 x2 y2
    S.text_ ( S.string label ) ! A.y y1 ! case axis of
                                            XAxis -> A.x x1
                                            YAxis -> A.x "4"


plotPoints :: PlotSpaceElement -> S.Svg
plotPoints (Aes xs f ys h cs) = do
    S.g $ do
        mapM_ marker $ zip3 (map (toImageSpace False) xs) (map (toImageSpace True) ys) (categorize cs)

plotLines :: PlotSpaceElement -> S.Svg
plotLines (Aes xs f ys h cs) = do
    S.g $ do
        let categorizedColors = categorize cs
            uniqueCategories = U.sortUniq categorizedColors
            filters = map filterPoint uniqueCategories
            coords = zip3 (map (toImageSpace False) xs) (map (toImageSpace True) ys) categorizedColors
            groupedLines = map (\f -> filter f coords) filters 
        mapM_ polyline groupedLines

filterPoint :: Integer -> (ImageSpaceValue, ImageSpaceValue, Integer) -> Bool
filterPoint category (x, y, c) = category == c

polyline :: [ (ImageSpaceValue, ImageSpaceValue, Integer) ] -> S.Svg
polyline points = do
    let (_, _, color) = head points
    S.polyline ! A.points (S.stringValue $ concatMap (\(x, y, _) -> show x ++ "," ++ show y ++ " ") points)
               ! A.fill "none"
               ! A.stroke (S.stringValue $ asHexString color)
               ! A.strokeWidth "0.25"

asHexString :: Integer -> String
asHexString x = printf "#%06X" x

marker :: (ImageSpaceValue, ImageSpaceValue, Integer) -> S.Svg
marker (x, y, color) = do
    S.circle ! A.cx (S.stringValue (show $ toDouble x)) ! A.cy (S.stringValue (show $ toDouble y)) ! A.r "0.5" ! A.fill (S.stringValue $ asHexString color)

line x1 y1 x2 y2 = do
    S.line ! A.x1 x1 ! A.y1 y1 ! A.x2 x2 ! A.y2 y2 ! A.stroke "black"
