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
import Sprites


data GameFlow = Quit | Continue | Restart
  deriving (Show, Eq)

data TileInfo = Blank CInt | Mine | BlownUp
  deriving (Show, Eq)

data TileVisual = Shown | Flagged | Covered
  deriving (Show, Eq)

data Tile = Tile TileInfo TileVisual
  deriving (Show, Eq)

type Grid = Array (CInt, CInt) Tile

data AppState = AppState
  { appGrid    :: Grid
    , appDims :: (CInt, CInt)
    , appMineCount :: CInt
  } deriving (Show)

type App = StateT AppState IO

putGrid :: Grid -> App ()
putGrid x = modify (\s -> s { appGrid = x })


lo = 15 * pxScale
gridRows = 10
gridCols = 13
gridHeight = tileDim * gridRows
gridWidth = tileDim * gridCols
totalWidth = leftBarWidth + gridWidth + rightBarWidth
midFrame = (totalWidth - smileFrameWidth) `div` 2


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

drawAll :: Renderer -> Texture -> GameFlow -> GameRes -> CInt -> App ()
drawAll renderer texture flow gr t = do
  grid <- gets appGrid
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

  let frameWidth = segmentWidth * 3 + pxScale * 2
      frameHeight = segmentHeight + pxScale * 2

  -- segment frames
  showRect' segmentFrame (lo, lo) (frameWidth, frameHeight)
  showRect' segmentFrame (totalWidth - frameWidth - lo - pxScale, lo) $
    (frameWidth, frameHeight)

  let showSeg (x, y) (h, t, o) = do
        showRect' (segments ! h) (x, y) (segmentWidth, segmentHeight)
        showRect' (segments ! t) (x + segmentWidth, y) $
          (segmentWidth, segmentHeight)
        showRect' (segments ! o) (x + segmentWidth * 2, y) $
          (segmentWidth, segmentHeight)
      isFlagged (Tile _ Flagged) = True
      isFlagged _ = False

  -- pxScale for one pixel to the right/under the frame
  showSeg (lo + pxScale, lo + pxScale) $ getDigits t

  mineCount <- gets appMineCount
  showSeg (totalWidth - frameWidth - lo, lo + pxScale) $
    getDigits $
      mineCount - (sum $ map (fromIntegral . fromEnum . isFlagged) $ elems grid)

  showRect' smileFrame (midFrame, lo) (smileFrameWidth, smileFrameWidth)

  case (flow, gr) of
    (Restart, _) ->
      showRect' spressed (midFrame + pxScale, lo + pxScale) $
        (smileWidth, smileWidth)
    (_, Lose) ->
      showRect' dead (midFrame + pxScale, lo + pxScale) $
        (smileWidth, smileWidth)
    (_, Win) ->
      showRect' glasses (midFrame + pxScale, lo + pxScale) $
        (smileWidth, smileWidth)
    _ ->
      showRect' smiling (midFrame + pxScale, lo + pxScale) $
        (smileWidth, smileWidth)

  mapM_ (liftIO . drawTile renderer texture) $ assocs grid

dispatch :: [EventPayload] -> App GameFlow
dispatch (QuitEvent : _) = pure Quit
dispatch ((KeyboardEvent ke) : _)
  | keyboardEventKeyMotion ke == Pressed
      && keysymKeycode (keyboardEventKeysym ke) == KeycodeEscape = pure Quit

dispatch ((MouseButtonEvent me) : xs)
  | mm == Pressed
      && inRange (0, gridCols - 1) i
      && inRange (0, gridRows - 1) j = do
      grid <- gets appGrid
      let new = case grid ! (i, j) of
            (Tile ti Covered) | mb == ButtonRight -> Tile ti Flagged
            (Tile ti Flagged) -> Tile ti Covered
            (Tile Mine Covered) | mb == ButtonLeft -> Tile BlownUp Shown
            (Tile ti Covered) | mb == ButtonLeft -> Tile ti Shown
            same -> same
      putGrid $ grid // [((i, j), new)]
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
          // [(ix, Tile Mine Shown) | (ix, Tile Mine _) <- assocs grid]
      )
  | won = (Win, grid)
  | otherwise = (Keep, grid // toReveal)
  where
    lost = any (\(Tile ti _) -> ti == BlownUp) $ elems grid
    shown = [ix | (ix, Tile _ Shown) <- assocs $ grid // toReveal]
    won =
      all
        ( \case
            (Tile _ Shown) -> True
            (Tile Mine Flagged) -> True
            _ -> False
        )
        $ elems grid
    toReveal =
      [ (ix, Tile ti Shown) | (ix, Tile ti _) <- assocs grid, (1 :: Integer) <= zeros ix
      ]
    zeros (i, j) =
      sum
        [case grid ! (i + oi, j + oj) of
           Tile (Blank 0) Shown -> 1
           _ -> 0 |
           oi <- [- 1 .. 1],
           withinGrid gridCols (i + oi),
           oj <- [- 1 .. 1],
           withinGrid gridRows (j + oj)]
    withinGrid b = inRange (0, b - 1)

appLoop :: Renderer -> Texture -> CInt -> CInt -> StdGen -> App ()
appLoop renderer texture st pt g = do
  let self = appLoop renderer texture

  events <- pollEvents
  flow <- dispatch $ map eventPayload events
  t <- fromIntegral <$> ticks
  grid <- gets appGrid
  let (gr, grid') = houseKeeping grid
  clear renderer

  modify (\s -> s { appGrid = grid' })

  drawAll renderer texture flow gr $ (pt - st) `div` 1000
  present renderer

  mineCount <- gets appMineCount

  case (flow, gr) of
    (_, Remake ix) -> do
      let (newGrid, g') = makeGrid mineCount g ix
          (_, newGrid') =
            houseKeeping $
              newGrid
                // [(ix, Tile (Blank 0) Shown)]
      putGrid newGrid'
      modify (\s -> s {appGrid = grid'})
      self st pt g'
    (Restart, _) -> do
      let (newGrid, g') = makeGrid mineCount g (0, 0)
      putGrid newGrid
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
          let (i, g') = uniformR interval g
           in helper g' $ Set.insert i s

makeGrid :: CInt -> StdGen -> (CInt, CInt) -> (Grid, StdGen)
makeGrid mineCount g (spi, spj) = (arr, g')
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
        Set.fromList mineNumbers Set.\\ Set.fromList safeNumbers
    gr' = gridRows - 1
    gc' = gridCols - 1

defaultMineCount = 10

main :: IO ()
main = do
  initializeAll
  window <-
    createWindow "Konstantins minesweeper game" $
      defaultWindow
        { windowInitialSize = V2 totalWidth (topHeight + botHeight + gridHeight)
        }
  renderer <- createRenderer window (-1) defaultRenderer
  t <- fromIntegral <$> ticks
  gen <- initStdGen
  texture <- loadBMP "assets.bmp" >>= createTextureFromSurface renderer
  let (grid, gen') = makeGrid defaultMineCount gen (0, 0)
  _ <- runStateT (appLoop renderer texture t t gen') $ AppState {
      appGrid=grid,
      appDims=(9, 9),
      appMineCount=defaultMineCount
                                                                }
  destroyWindow window
  putStrLn "gg"

