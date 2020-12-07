open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines "inputs/day7.txt"

#time "on"

let bags =
    input |> Array.map (fun bag ->
        let m = Regex.Match(bag, @"(.+?)(?= bags) bags contain (.+)")
        let bagColor = m.Groups.[1].Value
        let innerBags =  m.Groups.[2].Value.Split ", " |> Array.choose (fun b -> 
            if b = "no other bags." then None
            else 
                let m = Regex.Match(b, @"(\d+) (.+?)(?= bag)")
                Some (int m.Groups.[1].Value, m.Groups.[2].Value))
        bagColor, innerBags)
    |> Map

let invertedBags =
    bags
    |> Seq.collect (fun (KeyValue(parentBag,innerBags)) -> innerBags |> Array.map (fun (_,bag) -> bag, parentBag))
    |> Seq.groupBy fst
    |> Seq.map (fun (bag,parents) -> bag, parents |> Seq.toList |> List.map snd)
    |> Map

let rec getParents next found =
    match invertedBags.TryFind next with
    | None -> found
    | Some parents -> parents |> List.collect (fun bag -> getParents bag (bag :: found))

let rec getChildCount next multiplier =
    match bags.[next] with
    | [||] -> multiplier
    | children -> multiplier + (children |> Array.sumBy (fun (count,bag) -> getChildCount bag (count * multiplier)))

getParents "shiny gold" [] 
|> List.distinct
|> List.length 
|> printfn "Answer1: %i"

(getChildCount "shiny gold" 1) - 1 // Do not count starting item itself into the sum
|> printfn "Answer2: %i"

#time "off"