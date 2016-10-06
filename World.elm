module World exposing (World)

import Rule exposing (..)
import Grid exposing (..)

type alias World =
    { rule : Rule
    , grid: Grid
    , generation : Int
    }

