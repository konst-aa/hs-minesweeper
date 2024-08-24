{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where
import SDL hiding (get)
import Control.Monad.Trans
import Control.Monad.State
import Control.Concurrent
import Data.Array
import qualified Data.Set as Set

import Foreign.C.Types
import System.Random

type Hrm = StateT (Grid, Texture) IO

data GameFlow = Quit | Continue
  deriving (Show, Eq)

data TileInfo = Blank CInt | Mine | BlownUp
  deriving (Show, Eq)
data TileVisual = Shown | Flagged | Covered
  deriving (Show, Eq)
data Tile = Tile TileInfo TileVisual
  deriving (Show, Eq)

type Grid = Array (CInt, CInt) Tile


nt :: (Num a, Integral a) => (V2 a -> V2 a) -> Rectangle a -> Rectangle a
nt f (Rectangle (P v1) v2) = rect'
    where v2' = v2 - v1
          rect' = Rectangle (P ((f (V2 0 0)) + v1)) ((f v2') + v1)

repos :: (V2 a -> V2 a) -> Rectangle a -> Rectangle a
repos f (Rectangle (P v1) v2) = (Rectangle (P (f v1)) v2)

resize :: (V2 a -> V2 a) -> Rectangle a -> Rectangle a
resize f (Rectangle p v2) = (Rectangle p (f v2))

idk :: Num a => (a ->  a) -> V2 a -> V2 a
idk f (V2 x y) = (V2 (f x) (f y))


covered, pressed, flagged, _qmark, _qmarkpressed, mine, blownup, _xmine :: Rectangle CInt
-- coords in assets sheet
covered:pressed:flagged:_qmark:_qmarkpressed:mine:blownup:_xmine:_  = 
    map (\x -> Rectangle (P (V2 (14 + x) 195)) (V2 15 15)) [0,17..]

numbers :: Array CInt (Rectangle CInt)
numbers = array (0, 8) $ [(0, pressed)] ++ numbered
    where numbered = [ (i+1, Rectangle (P (V2 (14 + (i * 17)) 212)) (V2 15 15)) 
            | i <- [0..7] ]


pickTileRect :: Tile -> Rectangle CInt
pickTileRect (Tile _ Covered) = covered
pickTileRect (Tile Mine Shown) = mine
pickTileRect (Tile BlownUp Shown) = blownup
pickTileRect (Tile (Blank n) Shown) = numbers ! n
pickTileRect (Tile _ Flagged) = flagged
-- pickTileRect _ = covered

drawTile :: Renderer -> Texture -> (CInt, CInt, Tile) -> IO ()
drawTile renderer texture (i, j, tile) = do
    copy renderer texture (Just $ pickTileRect tile) 
                          (Just (Rectangle (P (V2 x y)) (V2 dim dim)))
    where dim = 60
          (x, y) = (i * dim, j * dim)

renderGrid :: Renderer -> Hrm ()
renderGrid renderer = do
    (grid, texture) <- get
    mapM_ (liftIO . (drawTile renderer texture))
          [(i, j, grid ! (i, j)) | i <- [0..9], j <- [0..9]]

dispatch :: [EventPayload] -> Hrm GameFlow
dispatch ((KeyboardEvent ke):_)
  | keyboardEventKeyMotion ke == Pressed
  && keysymKeycode (keyboardEventKeysym ke) == KeycodeQ = do
        liftIO $ putStrLn "quitting"
        pure Quit
dispatch ((MouseButtonEvent me):xs)
  | mouseButtonEventMotion me == Pressed
    && i >= 0 && i <= 9 && j >= 0 && j <= 9 = do
      (grid, texture) <- get
      let new = case (grid ! (i, j)) of
                 (Tile ti Covered) | mb == ButtonRight -> Tile ti Flagged
                 (Tile ti Flagged) -> Tile ti Covered
                 (Tile Mine Covered) | mb == ButtonLeft -> Tile BlownUp Shown
                 (Tile ti Covered) | mb == ButtonLeft -> Tile ti Shown
                 same -> same
      put (grid // [((i, j), new)], texture)
      dispatch xs
      where P (V2 x y) = mouseButtonEventPos me
            i = CInt $ x `div` 60
            j = CInt $ y `div` 60
            mb = mouseButtonEventButton me
dispatch (_:xs) = dispatch xs
dispatch [] = pure Continue

houseKeeping :: Grid -> (Bool, Grid)
houseKeeping grid
  | lost = (True, grid // 
      [(ix, Tile Mine Shown) | ix <- indices grid, isMine (grid ! ix)])
  | otherwise = (False, grid // toReveal)
  where lost = any (\(Tile ti _) -> ti == BlownUp) $ elems grid
        toReveal = [(ix, makeShown (grid ! ix)) | ix <- indices grid, 1 <= zeros ix]
        makeShown (Tile ti _) = Tile ti Shown
        zeros (i, j) =
            sum [ case (grid ! (i+oi, j+oj)) of
                        Tile (Blank 0) Shown -> 1
                        _ -> 0
                  | oi <- [-1..1], oj <- [-1..1], 
                  withinGrid (i+oi), withinGrid (j+oj) ]
        isMine (Tile Mine _) = True
        isMine _ = False

appLoop :: Renderer -> Hrm ()
appLoop renderer = do
  events <- pollEvents
  flow <- dispatch $ map eventPayload events
  (grid, texture) <- get
  let (lost, grid') = houseKeeping grid
  clear renderer
  put (grid', texture)
  renderGrid renderer
  present renderer
  if lost then liftIO $ threadDelay 2000000 else pure ()
  unless (lost || (flow == Quit)) $ appLoop renderer


genUnique :: (CInt, CInt) -> Int -> StdGen -> [CInt]
genUnique interval n gen = helper gen Set.empty
    where helper g s
            | Set.size s == n = Set.toList s -- idk how random this is :/
            | otherwise = 
                let (i, g') = (uniformR interval g) in helper g' $ Set.insert i s

clamp :: Ord a => a -> a -> a -> a
clamp minVal maxVal x = max minVal $ min maxVal x

withinGrid :: CInt -> Bool
withinGrid n = n == clamp 0 9 n


makeGrid :: StdGen -> Grid
makeGrid g = array ((0, 0), (9, 9)) [((i, j), choose (i, j)) | i <- [0..9], j <- [0..9]]
    where choose c
            | Set.member c mines = Tile Mine Covered
            | otherwise = Tile (Blank (neighbors c)) Covered
          mines = Set.fromList [(i `div` 10, i `mod` 10) 
                                | i <- genUnique (0, 99) 10 g]
          neighbors (i, j) = 
              sum [ if Set.member (i+oi, j+oj) mines
                       then 1 else 0
                | oi <- [-1..1], oj <- [-1..1] ]

main :: IO ()
main = do
    initializeAll
    window <- createWindow "My SDL Application" 
                            (defaultWindow { windowInitialSize = V2 600 600 })
    gen <- initStdGen
    renderer <- createRenderer window (-1) defaultRenderer
    texture <- (loadBMP "assets.bmp") >>= (createTextureFromSurface renderer)
    _ <- runStateT (appLoop renderer) $ (makeGrid gen, texture)
    destroyWindow window
    putStrLn "gg"
