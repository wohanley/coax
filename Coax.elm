module Coax where

import Generator(..)
import Generator.Standard(..)
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

--step : [Touch] -> Game -> Game
--step touches game = 

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