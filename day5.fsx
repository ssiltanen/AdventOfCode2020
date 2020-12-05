let input = System.IO.File.ReadAllLines "inputs/day5.txt"


let row =
    ("BBFFBBB" |> Seq.toList, 0, 127)
    |> Array.unfold (fun (partitioning, lower, upper) ->
        let mid = (lower + upper) / 2
        match partitioning with
        | [] -> Some (mid, (['X'], mid, mid))
        | head :: tail when head = 'F' -> Some (mid, (tail, lower, mid))
        | head :: tail when head = 'B' -> Some (mid+1, (tail, mid+1, upper))
        | _ -> None)
    |> Array.last

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