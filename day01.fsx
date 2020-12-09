let input = 
    System.IO.File.ReadAllLines "inputs/day01.txt"
    |> Array.map int

#time

let allPairs = 
    Array.allPairs input input
    |> Array.groupBy id
    |> Array.choose (fun ((a,b),values) -> if a = b && Array.length values = 1 then None else Some (a,b))

allPairs 
|> Seq.pick (fun (a,b) -> if a + b = 2020 then Some (a * b) else None) 
|> printfn "Answer 1: %i"

input 
|> Seq.pick (fun i -> allPairs |> Seq.tryPick (fun (a,b) -> if a + b + i = 2020 then Some (a * b * i) else None))
|> printfn "Answer 2: %i"