let input = System.IO.File.ReadAllText "inputs/day6.txt"

#time "on"

let groupAnswers = input.Split(System.Environment.NewLine + System.Environment.NewLine)

groupAnswers
|> Array.sumBy (Seq.where System.Char.IsLetter >> Seq.distinct >> Seq.length)
|> printfn "Answer 1: %i"

groupAnswers
|> Array.sumBy (
    fun group -> group.Split System.Environment.NewLine
    >> Array.map Set.ofSeq
    >> Set.intersectMany
    >> Set.count)
|> printfn "Answer 2: %i"

#time "off"