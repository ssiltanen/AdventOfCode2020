let input = System.IO.File.ReadAllLines "inputs/day5.txt"

#time "on"

let binarySpacePartitioning lowChar highChar sequence =
    let middle a b = (a + b) / 2
    sequence
    |> Array.fold (fun (lo,hi) c ->
        let mid = middle lo hi
        if c = lowChar then (lo, mid)
        elif c = highChar then (mid + 1, hi)
        else failwith "Invalid sequence value") (0, (pown 2 sequence.Length) - 1)
    |> fst

let findSeat partitioning =
    let rowData, colData = partitioning |> Seq.toArray |> Array.splitAt 7
    let row = binarySpacePartitioning 'F' 'B' rowData
    let col = binarySpacePartitioning 'L' 'R' colData
    row, col

let seatId (row,col) = row * 8 + col

let seatIds = input |> Array.map (findSeat >> seatId) |> Array.sort

let answer1 = Array.last seatIds
let answer2 = seatIds |> Array.pairwise |> Array.pick (fun (a,b) -> if b - a = 2 then Some (a + 1) else None)

#time "off"