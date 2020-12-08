let input = System.IO.File.ReadAllLines "inputs/day8.txt"

#time

type Instruction = Acc of int | Jmp of int | Nop of int with
    static member ofTuple = function
        | "acc", value -> Acc value
        | "jmp", value -> Jmp value
        | "nop", value -> Nop value
        | _ -> failwith "Unsupported instruction"

type Sequence = Infinite of acc:int | Finite of acc:int

let instructions =
    input |> Array.map (fun instruction -> 
        let split = instruction.Split " "
        Instruction.ofTuple (split.[0], int split.[1]))

let runInstructions instructions =
    let inst = instructions |> Array.map (fun i -> i, 0)
    let length = Array.length inst
    let rec runInstructionAt acc i =
        if i >= length then Finite acc
        else
            let instruction, executions = inst.[i]
            inst.[i] <- (instruction, executions + 1)
            match inst.[i] with
            | _, 2 -> Infinite acc
            | Acc value, _ -> runInstructionAt (acc + value) (i + 1)
            | Jmp value, _ -> runInstructionAt acc (i + value)
            | Nop _, _ -> runInstructionAt acc (i + 1)
    runInstructionAt 0 0

let accOfFixedSequence () =
    let copy (arr: Instruction []) = 
        let output = Array.create (Array.length arr) (Nop 0)
        arr.CopyTo(output, 0)
        output

    let inject i value (arr: Instruction []) =
        arr.[i] <- value
        arr

    instructions
    |> (Seq.mapi (fun i inst -> 
            match inst with
            | Acc _ -> None
            | Jmp value -> Some (runInstructions (instructions |> copy |> inject i (Nop value)))
            | Nop value -> Some (runInstructions (instructions |> copy |> inject i (Jmp value))))
        >> Seq.pick (function Some (Finite acc) -> Some acc | _ -> None))

runInstructions instructions 
|> function 
| Infinite acc -> printfn "Answer1: %i" acc 
| _ -> failwith "Error"

accOfFixedSequence() |> printfn "Answer2: %i"