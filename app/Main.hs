{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Monad.State
import Control.Monad.Trans
import Data.Array
import qualified Data.Set as Set
import Foreign.C.Types
import SDL hiding (get)
import System.Random

type Hrm = StateT Grid IO

data GameFlow = Quit | Continue | Restart
  deriving (Show, Eq)

data TileInfo = Blank CInt | Mine | BlownUp
  deriving (Show, Eq)

data TileVisual = Shown | Flagged | Covered
  deriving (Show, Eq)

data Tile = Tile TileInfo TileVisual
  deriving (Show, Eq)

type Grid = Array (CInt, CInt) Tile

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

mlt :: CInt
mlt = 4

mineCount,
  leftBarWidth,
  topHeight,
  botHeight,
  rightBarWidth,
  tileDim,
  segmentHeight,
  segmentWidth,
  totalWidth,
  smileWidth,
  smileFrameWidth,
  midFrame,
  lo,
  gridRows,
  gridCols,
  gridWidth,
  gridHeight ::
    CInt
mineCount = 25
topHeight = 55 * 4
botHeight = 8 * 4
leftBarWidth = 12 * 4
rightBarWidth = 8 * 4
segmentHeight = 23 * mlt
segmentWidth = 13 * mlt
tileDim = 64
totalWidth = leftBarWidth + gridWidth + rightBarWidth
smileWidth = 24 * mlt
smileFrameWidth = smileWidth + 2 * mlt
midFrame = (totalWidth - smileFrameWidth) `div` 2
lo = 60
gridRows = 10
gridHeight = tileDim * gridRows
gridCols = 13
gridWidth = tileDim * gridCols

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

pickTileRect :: Tile -> Rectangle CInt
pickTileRect (Tile _ Covered) = covered
pickTileRect (Tile Mine Shown) = mine
pickTileRect (Tile BlownUp Shown) = blownup
pickTileRect (Tile (Blank n) Shown) = numbers ! n
pickTileRect (Tile _ Flagged) = flagged

-- pickTileRect _ = covered

drawTile :: Renderer -> Texture -> ((CInt, CInt), Tile) -> IO ()
drawTile renderer texture ((i, j), tile) = do
  copy
    renderer
    texture
    (Just $ pickTileRect tile)
    (Just (Rectangle (P (V2 x y)) (V2 tileDim tileDim)))
  where
    (x, y) = (leftBarWidth + i * tileDim, topHeight + j * tileDim)

showRect :: Renderer -> Texture -> Rectangle CInt -> (CInt, CInt) -> (CInt, CInt) -> IO ()
showRect renderer texture rect (x, y) (w, h) =
  copy
    renderer
    texture
    (Just rect)
    (Just (Rectangle (P (V2 x y)) (V2 w h)))

getDigits :: CInt -> (CInt, CInt, CInt)
getDigits n =
  ( picker (-99, -10) hundreds,
    picker (-9, -1) tens,
    ones
  )
  where
    n' = abs n
    ones = n' `rem` 10
    tens = (n' `rem` 100) `div` 10
    hundreds = n' `div` 100
    picker (s, e) d
      | inRange (s, e) n = 10 -- minus sign goes here
      | n > s && n < 0 = 11
      | otherwise = d

drawAll :: Renderer -> Texture -> GameFlow -> GameRes -> CInt -> Hrm ()
drawAll renderer texture flow gr t = do
  grid <- get
  let showRect' rect xy wh = liftIO $ showRect renderer texture rect xy wh

  -- top of frame
  showRect' leftTopBar (0, 0) (leftBarWidth, topHeight)
  showRect' midTopBar (leftBarWidth, 0) (gridWidth, topHeight)
  showRect' rightTopBar (leftBarWidth + gridWidth, 0) $
    (rightBarWidth, topHeight)

  -- left and right of frame
  showRect' leftBar (0, topHeight) (leftBarWidth, gridHeight)
  showRect' rightBar (leftBarWidth + gridWidth, topHeight) $
    (rightBarWidth, gridHeight)

  -- bottom of frame
  showRect' leftBotBar (0, topHeight + gridHeight) $
    (leftBarWidth, botHeight)
  showRect' rightBotBar (leftBarWidth + gridWidth, topHeight + gridHeight) $
    (rightBarWidth, botHeight)

  showRect' midBotBar (leftBarWidth, topHeight + gridHeight) $
    (gridWidth, botHeight)

  let frameWidth = segmentWidth * 3 + mlt * 2
      frameHeight = segmentHeight + mlt * 2

  -- segment frames
  showRect' segmentFrame (lo, lo) (frameWidth, frameHeight)
  showRect' segmentFrame (totalWidth - frameWidth - lo - mlt, lo) $
    (frameWidth, frameHeight)

  let showSeg (x, y) (h, t, o) = do
        showRect' (segments ! h) (x, y) (segmentWidth, segmentHeight)
        showRect' (segments ! t) (x + segmentWidth, y) $
          (segmentWidth, segmentHeight)
        showRect' (segments ! o) (x + segmentWidth * 2, y) $
          (segmentWidth, segmentHeight)
      isFlagged (Tile _ Flagged) = True
      isFlagged _ = False

  -- mlt for one pixel to the right/under the frame
  showSeg (lo + mlt, lo + mlt) $ getDigits t
  showSeg (totalWidth - frameWidth - lo, lo + mlt) $
    getDigits $
      mineCount - (CInt $ sum $ map (fromIntegral . fromEnum . isFlagged) $ elems grid)

  showRect' smileFrame (midFrame, lo) $
    (smileFrameWidth, smileFrameWidth)

  case (flow, gr) of
    (Restart, _) ->
      showRect' spressed (midFrame + mlt, lo + mlt) $
        (smileWidth, smileWidth)
    (_, Lose) ->
      showRect' dead (midFrame + mlt, lo + mlt) $
        (smileWidth, smileWidth)
    (_, Win) ->
      showRect' glasses (midFrame + mlt, lo + mlt) $
        (smileWidth, smileWidth)
    _ ->
      showRect' smiling (midFrame + mlt, lo + mlt) $
        (smileWidth, smileWidth)

  mapM_ (liftIO . (drawTile renderer texture)) $ assocs grid

dispatch :: [EventPayload] -> Hrm GameFlow
dispatch ((KeyboardEvent ke) : _)
  | keyboardEventKeyMotion ke == Pressed
      && keysymKeycode (keyboardEventKeysym ke) == KeycodeQ = do
      liftIO $ putStrLn "quitting"
      pure Quit
dispatch ((MouseButtonEvent me) : xs)
  | mm == Pressed
      && inRange (0, gridCols - 1) i
      && inRange (0, gridRows - 1) j = do
      grid <- get
      let new = case (grid ! (i, j)) of
            (Tile ti Covered) | mb == ButtonRight -> Tile ti Flagged
            (Tile ti Flagged) -> Tile ti Covered
            (Tile Mine Covered) | mb == ButtonLeft -> Tile BlownUp Shown
            (Tile ti Covered) | mb == ButtonLeft -> Tile ti Shown
            same -> same
      put $ grid // [((i, j), new)]
      dispatch xs
  | mm == Pressed
      && inRange (midFrame, midFrame + smileFrameWidth) x
      && inRange (lo, lo + smileFrameWidth) y =
      pure Restart
  where
    P (V2 _x _y) = mouseButtonEventPos me
    (x, y) = (CInt _x, CInt _y)
    i = (x - leftBarWidth) `div` tileDim
    j = (y - topHeight) `div` tileDim
    mb = mouseButtonEventButton me
    mm = mouseButtonEventMotion me
dispatch (_ : xs) = dispatch xs
dispatch [] = pure Continue

data GameRes = Win | Lose | Keep | Remake (CInt, CInt)
  deriving (Show, Eq)

houseKeeping :: Grid -> (GameRes, Grid)
houseKeeping grid
  | length shown == 1 = (Remake $ head shown, grid)
  | lost =
      ( Lose,
        grid
          // [(ix, Tile Mine Shown) | (ix, (Tile Mine _)) <- assocs grid]
      )
  | won = (Win, grid)
  | otherwise = (Keep, grid // toReveal)
  where
    lost = any (\(Tile ti _) -> ti == BlownUp) $ elems grid
    shown = [ix | (ix, (Tile _ Shown)) <- assocs $ grid // toReveal]
    won =
      all
        ( \case
            (Tile _ Shown) -> True
            (Tile Mine Flagged) -> True
            _ -> False
        )
        $ elems grid
    toReveal =
      [ (ix, Tile ti Shown) | (ix, (Tile ti _)) <- assocs grid, 1 <= zeros ix
      ]
    zeros (i, j) =
      sum
        [ case (grid ! (i + oi, j + oj)) of
            Tile (Blank 0) Shown -> 1
            _ -> 0
          | oi <- [-1 .. 1],
            oj <- [-1 .. 1],
            withinGrid gridCols (i + oi),
            withinGrid gridRows (j + oj)
        ]
    withinGrid b n = inRange (0, b - 1) n

appLoop :: Renderer -> Texture -> CInt -> CInt -> StdGen -> Hrm ()
appLoop renderer texture st pt g = do
  let self = appLoop renderer texture

  events <- pollEvents
  flow <- dispatch $ map eventPayload events
  t <- fromIntegral <$> ticks
  grid <- get
  let (gr, grid') = houseKeeping grid
  clear renderer
  put grid'
  drawAll renderer texture flow gr $ (pt - st) `div` 1000
  present renderer

  case (flow, gr) of
    (_, Remake ix) -> do
      let (newGrid, g') = makeGrid g ix
          (_, newGrid') =
            houseKeeping $
              newGrid
                // [(ix, (Tile (Blank 0) Shown))]
      put newGrid'
      self st pt g'
    (Restart, _) -> do
      let (newGrid, g') = makeGrid g (0, 0)
      put newGrid
      liftIO $ threadDelay 200000
      t' <- fromIntegral <$> ticks -- bc of delay
      self t' t' g'
    (Quit, _) -> pure ()
    (_, Keep) -> self st t g
    (_, _) -> self st pt g -- Win/Lose, keep timer same

genUnique :: (CInt, CInt) -> Int -> StdGen -> Set.Set CInt -> ([CInt], StdGen)
genUnique interval n g exc = helper g exc
  where
    helper g s
      | Set.size s == n = (Set.toList s, g) -- idk how random this is :/
      | otherwise =
          let (i, g') = (uniformR interval g)
           in helper g' $ Set.insert i s

makeGrid :: StdGen -> (CInt, CInt) -> (Grid, StdGen)
makeGrid g (spi, spj) = (arr, g')
  where
    arr =
      array ((0, 0), (gc', gr')) $
        [((i, j), choose (i, j)) | i <- [0 .. gc'], j <- [0 .. gr']]
    choose c
      | Set.member c mines = Tile Mine Covered
      | otherwise = Tile (Blank (neighbors c)) Covered
    mines =
      Set.fromList $
        [(i `mod` gridCols, i `div` gridCols) | i <- mineNumbers']
    neighbors (i, j) =
      sum
        [ 1 | oi <- [-1 .. 1], oj <- [-1 .. 1], Set.member (i + oi, j + oj) mines
        ]
    safeNumbers =
      [(spi + oi) + gridCols * (spj + oj) | oi <- [-1 .. 1], oj <- [-1 .. 1]]
    (mineNumbers, g') =
      genUnique
        (0, gridRows * gridCols - 1)
        (fromIntegral $ mineCount + 9)
        g
        $ Set.fromList safeNumbers
    mineNumbers' =
      Set.toList $
        (Set.fromList mineNumbers) Set.\\ (Set.fromList safeNumbers)
    gr' = gridRows - 1
    gc' = gridCols - 1

main :: IO ()
main = do
  initializeAll
  window <-
    createWindow "My SDL Application" $
      defaultWindow
        { windowInitialSize = V2 totalWidth (topHeight + botHeight + gridHeight)
        }
  renderer <- createRenderer window (-1) defaultRenderer
  t <- fromIntegral <$> ticks
  gen <- initStdGen
  texture <- (loadBMP "assets.bmp") >>= (createTextureFromSurface renderer)
  let (grid, gen') = makeGrid gen (0, 0)
  _ <- runStateT (appLoop renderer texture t t gen') $ grid
  destroyWindow window
  putStrLn "gg"
