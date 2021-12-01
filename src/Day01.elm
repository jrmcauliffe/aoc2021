module Day01 exposing (..)

-- https://adventofcode.com/2021/day/1
-- Find Count of rising depths in List


risingDiffCount : List Int -> Int
risingDiffCount depths =
    let
        offset =
            depths |> List.tail |> Maybe.withDefault []
    in
    List.map2 (-) offset depths |> List.filter (\x -> x > 0) |> List.length


risingDiffWindowCount : List Int -> Int
risingDiffWindowCount depths =
    let
        window1 = depths
        window2 = window1 |> List.tail |> Maybe.withDefault []
        window3 = window2 |> List.tail |> Maybe.withDefault []
    in
    List.map3 (\a -> \b -> \c -> a + b + c) window1 window2 window3 |> risingDiffCount
