module Day03 exposing (..)

import Array exposing (fromList, get)
import Binary exposing (Bits, toIntegers)



-- https://adventofcode.com/2021/day/3
-- Parse binary diagnostic information


type alias Report =
    List Bits


type alias BitCounts =
    List Int


parseReport : String -> Report
parseReport s =
    s
        |> String.split "\n"
        |> List.map (\x -> x |> String.toList |> List.filterMap (\y -> y |> String.fromChar |> String.toInt) |> Binary.fromIntegers)


addCount : Bits -> BitCounts -> BitCounts
addCount b counts =
    b |> toIntegers |> List.map2 (+) counts


count : Report -> List Int
count r =
    let
        initial =
            0 |> List.repeat 20
    in
    r |> List.foldl addCount initial


gamma : Report -> Bits
gamma r =
    let
        entries =
            r |> List.length
    in
    count r |> List.map (\x -> x >= (entries // 2)) |> Binary.fromBooleans


epsilon : Report -> Bits
epsilon r =
    r |> gamma |> Binary.not



-- Part 2, co2 and oxy ratings


nthBit : Int -> Bits -> Maybe Bool
nthBit n bits =
    bits |> Binary.toBooleans |> fromList |> get (n - 1)


common : Int -> (Int -> Int -> Bool) -> Report -> Report
common n f r =
    let
        pairs =
            r |> List.filterMap (\i -> nthBit n i |> Maybe.map (Tuple.pair i))

        counts =
            pairs |> List.partition Tuple.second
    in
    if (r |> List.length) == 1 then
        r
        -- Down to one, just return that

    else
    -- Apply comparison function and recurse on matching subset
    if
        f (counts |> Tuple.first |> List.length) (counts |> Tuple.second |> List.length)
    then
        counts |> Tuple.first |> List.map Tuple.first |> common (n + 1) f

    else
        counts |> Tuple.second |> List.map Tuple.first |> common (n + 1) f


cO2 : Report -> Maybe Bits
cO2 r =
    r |> common 1 (<) |> List.head


oxy : Report -> Maybe Bits
oxy r =
    r |> common 1 (>=) |> List.head
