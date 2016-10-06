module Grid exposing (Cell(..), Grid, stepGrid)

import Array
import Array2D exposing (Array2D, columns, rows)
import Rule exposing (Rule)

type Cell = Dead | Alive Int

type alias Grid = Array2D Cell

mkGrid : Int -> Int -> Grid
mkGrid rows columns = Array2D.repeat rows columns Dead

neighborCells : Grid -> Int -> Int -> List Cell
neighborCells grid row col = 
    let inc n = n + 1
        dec n = n - 1
        id n = n
        wrap bounds n = (n + bounds) % bounds

        offsets =
            [ (dec, dec), (dec, id), (dec, inc)
            , (id, dec), {--blank--} (id, inc)
            , (inc, dec), (inc, id), (inc, inc) ]
            |> List.map (\(dr, dc) -> 
                (wrap (rows grid) << dr, wrap (columns grid) << dc))

        getOffsetCell (dr, dc) = Array2D.get (dr row) (dc col) grid
    in offsets |> List.filterMap getOffsetCell

stepCell : Rule -> Cell -> List Cell -> Cell
stepCell rule currentState neighborStates =
    let score =
            List.map2 (,) (Array.toList rule.weights) neighborStates
            |> List.filter (\(_, s) -> s /= Dead)
            |> List.map fst
            |> List.sum
    in 
        if score > rule.lifeMax then Dead
        else case currentState of
            Dead -> if rule.birthMin <= score then (Alive 1) else Dead
            Alive age -> if rule.lifeMin <= score then (Alive (age+1)) else Dead

stepGrid : Rule -> Grid -> Grid
stepGrid rule grid =
    let updateGridCell row col cell = 
            stepCell rule cell (neighborCells grid row col)
    in Array2D.indexedMap updateGridCell grid
