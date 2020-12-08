let input = System.IO.File.ReadAllLines "inputs/day8.txt"

#time

type Instruction = Acc of int | Jmp of int | Nop of int with
    static member ofTuple = function
        | "acc", value -> Acc value
        | "jmp", value -> Jmp value
        | "nop", value -> Nop value
        | _ -> failwith "Unsupported instruction"

let instructions =
    input |> Array.map (fun instruction -> 
        let split = instruction.Split ([| " "; " +" |], System.StringSplitOptions.None)
        Instruction.ofTuple (split.[0], int split.[1]))

let accAfterRunningInstructionsOnce () =
    let inst = instructions |> Array.map (fun i -> i, 0)
    let rec runInstructionAt acc i =
        let instruction, runs = inst.[i]
        inst.[i] <- (instruction, runs + 1)
        match inst.[i] with
        | _, 2 -> acc
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
        
    let tryGetFiniteSequenceAcc instructions =
        let inst = instructions |> Array.map (fun i -> i, 0)
        let length = Array.length inst
        let rec runInstructionAt acc i =
            if i >= length then Some acc
            else
                let instruction, runs = inst.[i]
                inst.[i] <- (instruction, runs + 1)
                match inst.[i] with
                | _, 2 -> None
                | Acc value, _ -> runInstructionAt (acc + value) (i + 1)
                | Jmp value, _ -> runInstructionAt acc (i + value)
                | Nop _, _ -> runInstructionAt acc (i + 1)
        runInstructionAt 0 0

    instructions
    |> (Seq.mapi (fun i inst -> 
            match inst with
            | Acc _ -> None
            | Jmp value -> tryGetFiniteSequenceAcc (instructions |> copy |> inject i (Nop value))
            | Nop value -> tryGetFiniteSequenceAcc (copy instructions |> inject i (Jmp value)))
        >> Seq.pick id)

accAfterRunningInstructionsOnce() |> printfn "Answer1: %i"
accOfFixedSequence() |> printfn "Answer2: %i"