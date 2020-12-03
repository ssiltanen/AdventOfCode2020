let input = System.IO.File.ReadAllLines "inputs/day3.txt"

type Right = Right of int
type Down = Down of int

#time "on"

let traverse (Right right) (Down down) =
    let rowLength = input.[0].Length

    let traverseRight r i (row: string) =
        row.[(r * i) % rowLength]

    let skipRowsByDown d (i,row) =
        if i % d = 0 then Some row else None

    input
    |> Array.indexed
    |> Array.choose (skipRowsByDown down)
    |> Array.mapi (traverseRight right)
    |> Array.tail
    |> Array.where ((=) '#')
    |> Array.length

let answer1 = traverse (Right 3) (Down 1)
let answer2 =
    [ traverse (Right 1) (Down 1)
      answer1
      traverse (Right 5) (Down 1)
      traverse (Right 7) (Down 1)
      traverse (Right 1) (Down 2) ]
    |> List.reduce (*)

#time "off"