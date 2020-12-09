let input = System.IO.File.ReadAllLines "inputs/day09.txt" |> Array.map int64

#time

let getPreceding25 i = Array.skip (i - 25) >> Array.take 25

let invalidNum =
    seq { 26 .. (Array.length input - 1) }
    |> Seq.pick (fun i -> 
        let cur = int64 input.[i]
        let preceding = getPreceding25 i input
        match Array.allPairs preceding preceding |> Array.forall (fun (a,b) -> a + b <> cur) with
        | true -> Some cur
        | false -> None)

printfn "Answer1: %i" invalidNum

let window =
    input
    |> Array.indexed
    |> Array.pick (fun (i,_) ->
        seq { 2 .. Array.length input } |> Seq.tryPick (fun size -> 
            input |> Array.skip i |> Array.windowed size |> Array.tryPick (fun win -> if Array.sum win = invalidNum then Some win else None)))

printfn "Answer2: %i" (Array.min window + Array.max window)