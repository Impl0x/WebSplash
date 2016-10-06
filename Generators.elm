module Generators exposing 
    ( randomCell 
    , randomGrid
    , randomRule
    )

import Array
import Array2D
import Random exposing (Generator, int, float, list, map, map2, map3)
import Grid exposing (Cell(..), Grid)
import Rule exposing (Weights, Rule, mkRule)

(>>=) = Random.andThen

-- Constructs a generator which returns true with the given probability [0.0, 1.0]
weightedBool : Float -> Generator Bool
weightedBool p = map (\roll -> roll < p) (float 0 1)

-- Constructs a Cell which has a given probability [0.0, 1.0] of being alive
randomCell : Float -> Generator Cell
randomCell pAlive = 
    weightedBool pAlive 
    |> map (\b -> if b then Alive 1 else Dead)

-- Constructs a random Grid where each cell has a 
-- probability [0.0, 1.0] of being alive
randomGrid : Float -> Int -> Int -> Generator Grid
randomGrid pAlive rows columns =
    list rows (randomCell pAlive)
    |> list columns
    |> map Array2D.fromList 

randomRule : Generator Rule
randomRule =
    (int 0 30) >>= \lifeMin ->
        (int lifeMin 31) >>= \birthMin ->
            (int birthMin (birthMin + 30)) >>= \lifeMax ->
                map (mkRule lifeMin birthMin lifeMax) randomWeights
                
-- Generates a list of initial weights for a rule
randomWeights : Generator Weights
randomWeights =
    let baseMin = -10
        baseMax = 20
        absMaxOffset = 20
        baseWeight = int baseMin baseMax
        randMaxOffset = int 0 absMaxOffset
        randOffset = randMaxOffset >>= (int 0)
        randWeight = map2 (+) baseWeight randOffset
        initialWeights = map Array.fromList (list 8 randWeight)
    in tweakWeights 0.4 initialWeights

tweakWeights : Float -> Generator Weights -> Generator Weights
tweakWeights tweakProb weights =
    let idx = int 0 7
        offset = int -20 20

        tweak ws idx offset = 
            case Array.get idx ws of
                Just v -> Array.set idx (v + 1) ws
                Nothing -> Debug.crash "Impossible"

        tweaked = map3 tweak weights idx offset

    in
        (weightedBool tweakProb) >>= \shouldTweak -> 
            if shouldTweak then tweakWeights tweakProb tweaked
            else weights
