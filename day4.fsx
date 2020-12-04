open System.Text.RegularExpressions

let input = System.IO.File.ReadAllText "inputs/day4.txt"

#time "on"

let passports =
    let nl = System.Environment.NewLine
    input.Split (nl + nl)
    |> Array.map (
        fun passport -> passport.Replace(nl, " ").Split ' ' 
        >> Array.map (fun field -> 
            let split = field.Split ':'
            split.[0], split.[1]))

let tryFindField (fields: (string * string) []) (name: string) = fields |> Array.tryFind (fst >> (=) name)
let regexSuccess pattern (value: string) = if Regex.Match(value, pattern).Success then Some value else None
let between min max i = if i >= min && i <= max then Some i else None

let validateBirthYear = regexSuccess @"^\d{4}$" >> Option.bind (int >> between 1920 2002)
let validateIssueYear = regexSuccess @"^\d{4}$" >> Option.bind (int >> between 2010 2020)
let validateExpirationYear = regexSuccess @"^\d{4}$" >> Option.bind (int >> between 2020 2030)
let validateHairColor = regexSuccess @"^#[0-9a-f]{6}$"
let validateEyeColor = regexSuccess @"^(amb|blu|brn|gry|grn|hzl|oth)$"
let validatePassportId = regexSuccess @"^\d{9}$"
let validateHeight = 
    regexSuccess @"^(\d{3}cm|\d{2}in)$" >> Option.bind (fun value -> 
        if value.EndsWith "cm" then value.[ .. 2] |> int |> between 150 193
        elif value.EndsWith "in" then value.[ .. 1] |> int |> between 59 76
        else None)

let (|*>) f = Option.bind (fun _ -> f)
let (|!>) a f = a |> Option.bind (snd >> f)

let task1 fields =
    let tryFind = tryFindField fields
    tryFind "byr"
    |*> (tryFind "iyr")
    |*> (tryFind "eyr")
    |*> (tryFind "hgt")
    |*> (tryFind "hcl")
    |*> (tryFind "ecl")
    |*> (tryFind "pid")

let task2 fields =
    let tryFind = tryFindField fields
    (tryFind "byr" |!> validateBirthYear)
    |*> (tryFind "iyr" |!> validateIssueYear)
    |*> (tryFind "eyr" |!> validateExpirationYear )
    |*> (tryFind "hgt" |!> validateHeight)
    |*> (tryFind "hcl" |!> validateHairColor)
    |*> (tryFind "ecl" |!> validateEyeColor)
    |*> (tryFind "pid" |!> validatePassportId)

let validPassports validate = passports |> Array.choose validate |> Array.length
let answer1 = validPassports task1
let answer2 = validPassports task2

#time "off"