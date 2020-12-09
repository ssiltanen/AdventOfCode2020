let input = System.IO.File.ReadAllLines "inputs/day05.txt"

#time

type SpacePartition = Hi | Lo with 
    static member ofChar = function 
        | 'F' | 'L' -> Lo 
        | 'B' | 'R' -> Hi
        | _ -> failwith "Invalid partition char"

let binarySpacePartitioning sequence =
    let middle a b = (a + b) / 2
    let binaryFolder (lo,hi) p =
        let mid = middle lo hi
        match p with
        | Lo -> lo, mid
        | Hi -> mid + 1, hi

    sequence |> Array.fold binaryFolder (0, (pown 2 sequence.Length) - 1) |> fst

let findSeat partitioning =
    let rowData, colData = partitioning |> Seq.toArray |> Array.map SpacePartition.ofChar |> Array.splitAt 7
    let row = binarySpacePartitioning rowData
    let col = binarySpacePartitioning colData
    row, col

let seatId (row,col) = row * 8 + col

let seatIds = input |> Array.map (findSeat >> seatId) |> Array.sort

Array.last seatIds |> printfn "Answer 1: %i"

seatIds |> Array.pairwise |> Array.pick (fun (a,b) -> if b - a = 2 then Some (a + 1) else None) |> printfn "Answer 2: %i"