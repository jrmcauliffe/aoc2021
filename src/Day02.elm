module Day02 exposing (..)

-- https://adventofcode.com/2021/day/2
-- Parse list of movements to find final location


type Movement
    = Up Int
    | Down Int
    | Forward Int


type alias Position =
    ( Int, Int )


parseMovement : String -> Maybe Movement
parseMovement n =
    let
        dir =
            n |> String.words |> List.head

        i =
            n |> String.words |> List.tail |> Maybe.andThen List.head |> Maybe.andThen String.toInt
    in
    case dir of
        Just "forward" ->
            i |> Maybe.map (\x -> Forward x)

        Just "up" ->
            i |> Maybe.map (\x -> Up x)

        Just "down" ->
            i |> Maybe.map (\x -> Down x)

        _ ->
            Nothing


parseMovements : String -> List Movement
parseMovements s =
    s
        |> String.split "\n"
        |> List.filterMap parseMovement


move : Movement -> Position -> Position
move movement ( x, y ) =
    case movement of
        Up i ->
            ( x, y - i )

        Down i ->
            ( x, y + i )

        Forward i ->
            ( x + i, y )


track : Position -> List Movement -> Position
track initial movements =
    movements |> List.foldl move initial



-- Helper Function for problem solution value


positionProduct : Position -> Int
positionProduct p =
    case p of
        ( x, y ) ->
            x * y



-- Part 2: Refine concept of movement to incorporate 'Aim'


type alias PositionWithAim =
    { x : Int
    , y : Int
    , a : Int
    }


moveWithAim : Movement -> PositionWithAim -> PositionWithAim
moveWithAim movement pos =
    case movement of
        Up i ->
            { pos | a = pos.a - i }

        Down i ->
            { pos | a = pos.a + i }

        Forward i ->
            { pos | x = pos.x + i, y = pos.y + pos.a * i }


trackWithAim : PositionWithAim -> List Movement -> PositionWithAim
trackWithAim initial movements =
    movements |> List.foldl moveWithAim initial



-- Helper Function for problem solution value


positionProductWithAim : PositionWithAim -> Int
positionProductWithAim p =
    case p of
        { x, y, a } ->
            x * y
