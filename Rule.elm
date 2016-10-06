module Rule exposing (..)

import Array exposing (Array)

type alias Weights = Array Int

type alias Rule = 
    { lifeMin : Int
    , birthMin : Int
    , lifeMax : Int
    , weights : Weights
    }

mkRule : Int -> Int -> Int -> Weights -> Rule
mkRule lifeMin birthMin lifeMax weights =
    { lifeMin = lifeMin
    , birthMin = birthMin
    , lifeMax = lifeMax
    , weights = weights
    }

-- The base rule-set for Conway's Game of Life
golRule = 
    let weights = Array.repeat 8 1
    in mkRule 2 3 3 weights
