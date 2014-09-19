module Coax where

import Generator(..)
import Generator.Standard(..)
import ListUtil as LU
import Math
import Touch

type Cell = Float
type Grid = [[Cell]]
type Dimension = { width: Int, height: Int }
type Game = { generator: Generator Standard, grid: Grid }

port size: { width: Int, height: Int }
port randomSeed: Int -- probably the Unix time

cellSize : Int
cellSize = 5

model : Game
model = init (generator randomSeed) size

--main : Element
--main = lift (fst . render) (foldp step model input)

init : Generator Standard -> Dimension -> Game
init gen {width, height} =
    { generator = gen,
      grid = repeat (height `div` cellSize)
                 (repeat (width `div` cellSize) 0.5) }

-- Logic

input : Signal [Touch.Touch]
input = sampleOn (fps 15) Touch.touches

step : [Touch.Touch] -> Game -> Game
step touches game =
    let grid' = game.grid |> touch touches |> automate
        gen' = snd (float game.generator)
    in { game | grid <- grid'
              , generator <- gen' }

touch : [Touch.Touch] -> Grid -> Grid
touch touches grid = grid

automate : Grid -> Grid
automate grid = LU.indexedMap
    (\rowNumber row -> LU.indexedMap
        (\columnNumber _ -> stepCell rowNumber columnNumber grid)
        row)
    grid

stepCell : Int -> Int -> Grid -> Cell
stepCell row column grid =
    let cell = LU.get row grid |> LU.get column
    in (getNeighbours row column grid)
        |> foldl (+) 0
        |> liveOrDie cell

getNeighbours : Int -> Int -> Grid -> [Cell]
getNeighbours row column grid =
    let rowCount = length grid
        colCount = length (LU.get 0 grid)
    in
        grid
            |> LU.getAll (bound (rowCount - 1) row)
            |> zipWith (<|)
                   [(LU.getAll (bound (colCount - 1) column)),
                    (LU.getAll (outerBound (colCount - 1) column)),
                    (LU.getAll (bound (colCount - 1) column))]
            |> LU.flatten

bound : Int -> Int -> [Int]
bound max index =
    let lower = Math.max 0 (index - 1)
        upper = Math.min max (index + 1)
    in
        LU.range lower (upper + 1)

outerBound : Int -> Int -> [Int]
outerBound max index = bound max index |> LU.remove index

liveOrDie : Cell -> Float -> Cell
liveOrDie alive neighbouringLife = 0.5


-- Display

render : Game -> (Element, Generator Standard)
render game =
    let (randoms, gen') = randomGrid game.generator size -- TODO: smarter size
        rows = zipWith renderRow randoms game.grid
    in (flow down rows, gen')

randomGrid : Generator Standard -> Dimension -> ([[Float]], Generator Standard)
randomGrid gen size = listOf (listOf float size.width) (size.height) gen

renderRow : [Float] -> [Cell] -> Element
renderRow randoms row = zipWith renderCell randoms row |> flow right

renderCell : Float -> Cell -> Element
renderCell random cell =
    color (cellColor random cell) (spacer cellSize cellSize)

cellColor : Float -> Cell -> Color
cellColor random cell =
    if random < cell then black else white