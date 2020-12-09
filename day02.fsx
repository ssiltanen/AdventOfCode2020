let input = System.IO.File.ReadAllLines "inputs/day02.txt"

[<Struct>]
type PasswordRule =
    { Min: int
      Max: int
      Letter: char
      Password: string }

#time

let passwordRules = 
    input |> Array.map (fun row ->
        let values = row.Split [|' '; '-'|]
        { Min = values.[0] |> int
          Max = values.[1] |> int
          Letter = values.[2].[0]
          Password = values.[3] })

let task1 { Min = min; Max = max; Letter = l; Password = pass } =
    let count = pass |> Seq.where ((=) l) |> Seq.length
    count >= min && count <= max

let task2 { Min = min; Max = max; Letter = l; Password = pass } =
    let a = pass |> Seq.item (min - 1) = l
    let b = pass |> Seq.item (max - 1) = l
    a <> b

module Array =
    let countWith f = 
        Array.where f >> Array.length

passwordRules |> Array.countWith task1 |> printfn "Answer 1: %i"
passwordRules |> Array.countWith task2 |> printfn "Answer 2: %i"