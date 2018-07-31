{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE Rank2Types                 #-}

module GGPlot
    ( ggplot
    , aes
    , geomLine
    , geomPoint
    , xlab
    , x
    , y
    , color
    , (!)
    , getAes
    , GGPlot
    , PlotElement (Aes)
    , GeomType
    , Attribute
    , Attributable
    ) where

import Data.List
import Units

ggplot :: [ PlotElement ] -> GGPlot
ggplot xs = GGPlot xs

aes :: PlotElement
aes = Aes [] [] []

geomLine :: PlotElement
geomLine = Geom Line

geomPoint :: PlotElement
geomPoint = Geom Point

xlab :: String -> PlotElement
xlab s = XLabel s

x :: [ Double ] -> Attribute
x xs = Attribute (\p -> case p of
                     Aes _ ys cs -> Aes (map mkPlotSpaceValue xs) ys cs
                     other -> other
                     )

y :: [ Double ] -> Attribute
y ys = Attribute (\p -> case p of
                     Aes xs _ cs -> Aes xs (map mkPlotSpaceValue ys) cs
                     other -> other
                     )

color :: [ String ] -> Attribute
color cs = Attribute (\p -> case p of
                     Aes xs ys _ -> Aes xs ys cs
                     other -> other
                     )

data GGPlot = GGPlot [ PlotElement ] deriving Show

data PlotElement = Aes [ PlotSpaceValue ] [ PlotSpaceValue ] [ String ]
                 | Geom GeomType
                 | XLabel String
                 deriving Show

getAes :: GGPlot -> Maybe PlotElement
getAes (GGPlot elements) = find (\e -> case e of
                                    Aes _ _ _ -> True
                                    _ -> False
                                ) elements

data GeomType = Line | Point deriving Show

newtype Attribute = Attribute (PlotElement -> PlotElement)

class Attributable h where
    (!) :: h -> Attribute -> h

instance Attributable PlotElement where
    h ! (Attribute f) = f h

instance Attributable (PlotElement -> PlotElement) where
    h ! f = (! f) . h
