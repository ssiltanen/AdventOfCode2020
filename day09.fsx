let input = System.IO.File.ReadAllLines "inputs/day09.txt" |> Array.map int64

#time

let invalidNum =
    seq { 26 .. (Array.length input - 1) }
    |> Seq.pick (fun i -> 
        let cur = int64 input.[i]
        let preceding = input |> Array.skip (i - 25) |> Array.take 25
        match Array.allPairs preceding preceding |> Array.forall (fun (a,b) -> a + b <> cur) with
        | true -> Some cur
        | false -> None)

let tryGetWindowWithSum sum skip size =
    input |> Array.skip skip |> Array.windowed size |> Array.tryPick (fun win -> if Array.sum win = sum then Some win else None)

let windowSizes = [| 2 .. Array.length input |]
let window =
    input
    |> Array.indexed
    |> Array.pick (fun (skip,_) -> windowSizes |> Seq.tryPick (tryGetWindowWithSum invalidNum skip))

printfn "Answer1: %i" invalidNum
printfn "Answer2: %i" (Array.min window + Array.max window)