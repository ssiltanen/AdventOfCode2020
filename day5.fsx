let input = System.IO.File.ReadAllLines "inputs/day5.txt"

let binarySpacePartitioning low up lowerBound upperBound (partitioning: string) =
    (partitioning |> Seq.toList, lowerBound, upperBound)
    |> Array.unfold (fun (partitioning, lower, upper) ->
        let mid = (lower + upper) / 2
        match partitioning with
        | [] -> Some (mid, (['X'], mid, mid))
        | head :: tail when head = low -> Some (mid, (tail, lower, mid))
        | head :: tail when head = up -> Some (mid+1, (tail, mid+1, upper))
        | _ -> None)
    |> Array.last

let findSeat (partitioning: string) =
    let row = binarySpacePartitioning 'F' 'B' 0 127 partitioning.[..6]
    let col = binarySpacePartitioning 'L' 'R' 0 7 partitioning.[7..]
    row, col

let seatId (row,col) = row * 8 + col

let seatIds = input |> Array.map (findSeat >> seatId) |> Array.sort
let answer1 = Array.last seatIds
let answer2 =
    seatIds
    |> Array.pairwise
    |> Array.pick (fun (a,b) -> if b - a = 2 then Some ((b + a) / 2) else None)