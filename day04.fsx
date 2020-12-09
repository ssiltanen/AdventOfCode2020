open System
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllText "inputs/day04.txt"

#time

let regexSuccess pattern (value: string) = if Regex.Match(value, pattern).Success then Some value else None
let between min max i = if i >= min && i <= max then Some i else None

type Passport =
    { Byr: string option
      Iyr: string option
      Eyr: string option
      Hgt: string option
      Hcl: string option
      Ecl: string option
      Pid: string option } with

    member this.HasMandatoryValues =
        [ this.Byr; this.Iyr; this.Eyr; this.Hgt; this.Hcl; this.Ecl; this.Pid ]
        |> List.forall Option.isSome

    member this.HasValidByr = 
        this.Byr |> Option.bind (regexSuccess @"^\d{4}$" >> Option.bind (int >> between 1920 2002)) |> Option.isSome
    member this.HasValidIyr =  
        this.Iyr |> Option.bind (regexSuccess @"^\d{4}$" >> Option.bind (int >> between 2010 2020)) |> Option.isSome
    member this.HasValidEyr = 
        this.Eyr |> Option.bind (regexSuccess @"^\d{4}$" >> Option.bind (int >> between 2020 2030)) |> Option.isSome 
    member this.HasValidHcl = 
        this.Hcl |> Option.bind (regexSuccess @"^#[0-9a-f]{6}$") |> Option.isSome
    member this.HasValidEcl = 
        this.Ecl |> Option.bind (regexSuccess @"^(amb|blu|brn|gry|grn|hzl|oth)$") |> Option.isSome
    member this.HasValidPid = 
        this.Pid |> Option.bind (regexSuccess @"^\d{9}$") |> Option.isSome
    member this.HasValidHgt =
        this.Hgt 
        |> Option.bind (regexSuccess @"^(\d{3}cm|\d{2}in)$" >> Option.bind (fun value -> 
            if value.EndsWith "cm" then value.[ .. 2] |> int |> between 150 193
            elif value.EndsWith "in" then value.[ .. 1] |> int |> between 59 76
            else None))
        |> Option.isSome
        
    member this.HasValidValues =
        [ this.HasValidByr; this.HasValidIyr; this.HasValidEyr; this.HasValidHcl; this.HasValidEcl; this.HasValidPid; this.HasValidHgt ]
        |> List.forall id

let passports =
    let nl = Environment.NewLine
    input.Split (nl + nl)
    |> Array.map (fun passport ->
        let fields = passport.Split([| nl; " "; ":" |], StringSplitOptions.RemoveEmptyEntries) |> Array.pairwise
        let tryGetField name = fields |> Array.tryFind (fst >> (=) name) |> Option.map snd
        { Byr = tryGetField "byr"
          Iyr = tryGetField "iyr"
          Eyr = tryGetField "eyr"
          Hgt = tryGetField "hgt"
          Hcl = tryGetField "hcl"
          Ecl = tryGetField "ecl"
          Pid = tryGetField "pid" })

passports |> Array.where (fun pass -> pass.HasMandatoryValues) |> Array.length |> printfn "Answer 1: %i"
passports |> Array.where (fun pass -> pass.HasValidValues) |> Array.length |> printfn "Answer 2: %i"