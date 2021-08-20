module Main where

import Control.Applicative (liftA2)
import Control.Concurrent (threadDelay)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import System.Console.ANSI
import System.Environment (getArgs)

type Point = (Int, Int)
type Cells = Set Point

add :: Point -> Point -> Point
add (x, y) (x', y') = (x + x', y + y')

fromFile :: String -> IO Cells
fromFile filename = do
  content <- readFile filename
  let
    grid = fmap (== '@') <$> lines content
    points = [(x, y) | (y, row) <- zip [0..] grid
      , (x, cell) <- zip [0..] row, cell]
  pure $ S.fromList points

near :: Cells -> Point -> [Point]
near cells point = add point <$> dirs
 where
  dirs = filter (/= (0, 0)) $ liftA2 (,) [-1..1] [-1..1]

shouldLive :: Cells -> Point -> Bool
shouldLive cells point = (alive && (aliveNear == 2 ||
  aliveNear == 3)) || (not alive && aliveNear == 3)
 where
  alive = S.member point cells
  aliveNear = length $ filter (`S.member` cells) $ near cells point

possible :: Cells -> [Point]
possible cells = S.toList cells >>= f
 where
  f cell = cell : near cells cell

render :: Cells -> (Int, Int) -> String
render cells (width, height) = unlines $ concat <$> canvas
 where
  canvas = (renderCell <$>) <$> resized
  renderCell True = setSGRCode [SetColor Background Vivid White] ++ "  " 
    ++ setSGRCode [Reset]
  renderCell False = "  "
  resized = take (height - 2) $ take (width `div` 2 - 1) <$> grid
  grid = [[S.member (x, y) cells | x <- [minX..maxX]] | y <- [minX..maxY]]

  minX = minimum $ fst <$> cellList
  maxX = maximum $ fst <$> cellList
  minY = minimum $ snd <$> cellList
  maxY = maximum $ snd <$> cellList

  cellList = S.toList cells

simulate :: Cells -> Int -> IO ()
simulate cells fps = simulate' cells
 where
  simulate' cells' = do
    clearScreen
    setCursorPosition 0 0
    sizeRes <- getTerminalSize
    let (height, width) = fromMaybe (maxBound, maxBound) sizeRes
    putStrLn $ render cells' (width, height)
    let nextCells = nextGen cells'
    threadDelay tickRate
    simulate' nextCells
  nextGen cells' = S.fromList $ filter (shouldLive cells') (possible cells')
  tickRate = 1000000 `div` fps

main :: IO ()
main = do
  args <- getArgs
  let fileName = getFileName args
  grid <- fromFile fileName
  simulate grid 5
 where
  getFileName [] = error "invalid usage, the path to the first generation must be passed to the program"
  getFileName (fileName:_) = fileName
