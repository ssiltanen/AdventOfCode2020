let input = System.IO.File.ReadAllLines "inputs/day8.txt"

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

let runInstructionsOnce () =
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

runInstructionsOnce() |> printfn "Answer1: %i"