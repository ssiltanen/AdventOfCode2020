let input = System.IO.File.ReadAllLines "inputs/day11.txt"

#time

type Position = Floor | Empty | Occupied with
    static member OfChar = function '.' -> Floor | 'L' -> Empty | '#' -> Occupied | _ -> failwith "Unsupported position"

type Layout = 
    | Changed of Map<(int * int), Position>
    | NotChanged of Map<(int * int), Position>

let positions = 
    input 
    |> Array.map (Seq.toArray >> Array.map Position.OfChar)
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x p -> (x,y), p))
    |> Array.collect id
    |> Map

let task1Adjacent (positions: Map<(int * int), Position>) (x, y) =
    List.allPairs [ x - 1 .. x + 1 ] [ y - 1 .. y + 1 ]
    |> List.except [ x,y ]
    |> List.choose positions.TryFind

let task2Adjacent (positions: Map<(int * int), Position>) (x, y) =
    let findFirstSeatInDirection (positions: Map<(int * int), Position>) pos getNextInDir =
        let rec loop pos =
            let newPos = getNextInDir pos
            match positions.TryFind newPos with
            | Some Floor -> loop newPos
            | Some status -> Some status
            | None -> None
        loop pos

    [ fun (x,y) -> x+1,y 
      fun (x,y) -> x-1,y
      fun (x,y) -> x,y+1
      fun (x,y) -> x,y-1
      fun (x,y) -> x+1,y+1
      fun (x,y) -> x+1,y-1
      fun (x,y) -> x-1,y-1
      fun (x,y) -> x-1,y+1 ]
    |> List.choose (findFirstSeatInDirection positions (x,y))

let reserveVacantSeats getAdjacent positions =
    let atLeastOneSeatChanged, updatedPositions =
        positions |> Seq.fold (fun (changed, processed) (KeyValue(coord,pos)) -> 
            match pos with
            | Empty ->
                if getAdjacent positions coord |> List.forall (function Empty | Floor -> true | _ -> false) 
                then true, (coord, Occupied) :: processed
                else changed, (coord, pos) :: processed
            | _ -> 
                changed, (coord, pos) :: processed) (false, [])
    if atLeastOneSeatChanged then Changed (Map updatedPositions) else NotChanged (Map updatedPositions)

let vacateCrowdedSeats getAdjacent adjacentLimit positions =
    let atLeastOneSeatChanged, updatedPositions =
        positions |> Seq.fold (fun (changed, processed) (KeyValue(coord,pos)) ->
            match pos with
            | Occupied ->
                if getAdjacent positions coord |> List.where (function Occupied -> true | _ -> false) |> List.length >= adjacentLimit
                then true, (coord, Empty) :: processed
                else changed, (coord, pos) :: processed
            | _ ->
                changed, (coord, pos) :: processed) (false, [])
    if atLeastOneSeatChanged then Changed (Map updatedPositions) else NotChanged (Map updatedPositions)

let getOccupiedSeatCountAfterStabilizing getAdjacent adjacentLimit positions =
    let stabilizeSeats =
        let rec loop positions =
            match reserveVacantSeats getAdjacent positions with
            | Changed positions ->
                match vacateCrowdedSeats getAdjacent adjacentLimit positions with
                | Changed positions -> loop positions
                | NotChanged positions -> positions
            | NotChanged positions -> positions
        loop

    positions
    |> stabilizeSeats
    |> Seq.where (fun (KeyValue(_,pos)) ->
        match pos with
        | Occupied -> true
        | _ -> false)
    |> Seq.length

positions
|> getOccupiedSeatCountAfterStabilizing task1Adjacent 4
|> printfn "Answer1: %i"

positions
|> getOccupiedSeatCountAfterStabilizing task2Adjacent 5
|> printfn "Answer2: %i"