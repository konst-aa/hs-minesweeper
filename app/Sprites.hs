module Sprites where

import SDL hiding (get)
import Foreign.C.Types
import Data.Array


-- coords in assets sheet
covered, pressed, flagged, _qmark, _qmarkpressed, mine, blownup, _xmine :: Rectangle CInt

covered : pressed : flagged : _qmark : _qmarkpressed : mine : blownup : _xmine : _ =
  map (\x -> Rectangle (P (V2 (14 + x) 195)) (V2 16 16)) [0, 17 ..]

smiling, spressed, _surprised, glasses, dead :: Rectangle CInt
smiling : spressed : _surprised : glasses : dead : _ =
  map (\x -> Rectangle (P (V2 (14 + x) 170)) (V2 24 24)) [0, 25 ..]

leftBar :: Rectangle CInt
leftBar = Rectangle (P (V2 475 431)) (V2 12 16)

rightBar :: Rectangle CInt
rightBar = Rectangle (P (V2 743 431)) (V2 8 16)

leftTopBar :: Rectangle CInt
-- 487 430
leftTopBar = Rectangle (P (V2 475 376)) (V2 12 55)

-- 750 430
rightTopBar :: Rectangle CInt
rightTopBar = Rectangle (P (V2 743 376)) (V2 8 55)

midTopBar :: Rectangle CInt
midTopBar = Rectangle (P (V2 535 376)) (V2 16 55)

leftBotBar :: Rectangle CInt
leftBotBar = Rectangle (P (V2 475 687)) (V2 12 8)

rightBotBar :: Rectangle CInt
rightBotBar = Rectangle (P (V2 743 687)) (V2 8 8)

midBotBar :: Rectangle CInt
midBotBar = Rectangle (P (V2 487 687)) (V2 16 8)

segmentFrame :: Rectangle CInt
segmentFrame = Rectangle (P (V2 491 391)) (V2 41 25)

smileFrame :: Rectangle CInt
smileFrame = Rectangle (P (V2 602 391)) (V2 26 26)

pxScale :: CInt
pxScale = 2

leftBarWidth,
  topHeight,
  botHeight,
  rightBarWidth,
  tileDim,
  segmentHeight,
  segmentWidth,
  -- totalWidth,
  smileWidth,
  smileFrameWidth :: CInt
  -- midFrame
  -- lo,
  -- gridRows,
  -- gridCols,
  -- gridWidth,
  -- gridHeight ::
topHeight = 55 * pxScale
botHeight = 8 * pxScale
leftBarWidth = 12 * pxScale
rightBarWidth = 8 * pxScale
segmentHeight = 23 * pxScale
segmentWidth = 13 * pxScale
tileDim = 16 * pxScale
smileWidth = 24 * pxScale
smileFrameWidth = smileWidth + 2 * pxScale


numbers :: Array CInt (Rectangle CInt)
numbers = array (0, 8) $ [(0, pressed)] ++ numbered
  where
    numbered =
      [ (i + 1, Rectangle (P (V2 (14 + (i * 17)) 212)) (V2 16 16))
        | i <- [0 .. 7]
      ]

segments :: Array CInt (Rectangle CInt)
segments =
  array (0, 11) $
    [(0, zero_), (10, negative), (11, blank)]
      ++ (zip [1 .. 9] $ take 10 row)
  where
    row =
      [ Rectangle (P (V2 (14 + (i * 14)) 146)) (V2 13 23)
        | i <- [0 .. 11]
      ]
    zero_ = head $ drop 9 row
    negative = head $ drop 10 row
    blank = head $ drop 11 row
