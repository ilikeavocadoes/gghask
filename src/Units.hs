module Units
    ( categorize
    , ImageSpaceValue
    , PlotSpaceValue
    , toImageSpace
    , mkPlotSpaceValue
    , toDouble
    ) where

import Data.Maybe (catMaybes)
import Data.List.Unique as U

categorize :: [String] -> [Integer]
categorize xs =
    let values = U.sortUniq xs
        encodeTable = zip values $ getNColorCodes (fromIntegral $ length values)
    in catMaybes $ map ( flip lookup encodeTable ) xs

getNColorCodes :: Integer -> [Integer]
getNColorCodes n =
    let m = fromIntegral n
    in take m $ cycle [0x00AA00, 0x0000AA, 0xAA0000]

data ImageSpaceValue = ImageSpaceValue Double deriving Show

instance Convertable ImageSpaceValue where
    toDouble (ImageSpaceValue v) = v

data PlotSpaceValue = PlotSpaceValue Double deriving (Show, Ord, Eq)

instance Convertable PlotSpaceValue where
    toDouble (PlotSpaceValue v) = v

toImageSpace :: Bool -> PlotSpaceValue -> ImageSpaceValue
toImageSpace isY (PlotSpaceValue v) = 
    let w = v * 80 + 10
        w' = if isY then 100 - w else w
    in ImageSpaceValue w'

mkPlotSpaceValue :: Double -> PlotSpaceValue
mkPlotSpaceValue v = PlotSpaceValue v

class Convertable a where
    toDouble :: a -> Double
