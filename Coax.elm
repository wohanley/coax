module Coax where

import Generator(..)
import Generator.Standard(..)

type Cell = Float
type Grid = [[Cell]]
type Dimension = (Int, Int)

type Game = { generator: Generator Standard, grid: Grid }

port size: (Int, Int) -- aka Dimension
port randomSeed: Int -- probably the Unix time

cellSize : Int
cellSize = 5

model : Game
model = init (generator randomSeed) size

main : Element
main = fst (render model)

--gameState : Signal Game
--gameState = foldp (init size) model (every second)

init : Generator Standard -> Dimension -> Game
init gen (width, height) =
    { generator = gen,
      grid = repeat (height `div` cellSize)
                 (repeat (width `div` cellSize) 0.5) }

render : Game -> (Element, Generator Standard)
render game =
    let rows = map (renderRow game.generator) game.grid
    in (flow down (map fst rows), snd (last rows))

renderRow : Generator Standard -> [Cell] -> (Element, Generator Standard)
renderRow gen row =
    let cells = map (renderCell gen) row
    in (flow right (map fst cells), (snd (last cells)))

renderCell : Generator Standard -> Cell -> (Element, Generator Standard)
renderCell gen cell =
    let (randomColor, gen') = cellColor gen cell
    in ((color randomColor (spacer cellSize cellSize)), gen')

cellColor : Generator Standard -> Cell -> (Color, Generator Standard)
cellColor gen cell =
    let (random, gen') = float gen
    in ((if random < cell then black else white), gen')
