open System
open System.Text.RegularExpressions

let input =
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"

let requiredFields = [
  "byr"
  "iyr"
  "eyr"
  "hgt"
  "hcl"
  "ecl"
  "pid"
  //"cid"
]

let inputs = input.Split ([|"\n\n"|],  StringSplitOptions.RemoveEmptyEntries)

let ``answer to part 1`` = 
  inputs
  |> Seq.toList
  |> List.filter(fun e -> 
    requiredFields
    |> List.forall(e.Contains)  
  )
  |> List.length

// part 2
type Passport = {
  BirthYear: string option
  IssueYear: string option
  ExpirationYear: string option
  Height: string option
  HairColor: string option
  EyeColor: string option
  PassportId: string option
}

let regex pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success && m.Value <> "" then Some m.Value
  else None

let getPattern prop = sprintf @"(?<=%s:)[\w|#]*" prop

let getProperty prop text = regex (getPattern prop) text

let passports = 
  inputs
  |> Seq.toList
  |> List.map(fun e ->
    { BirthYear = getProperty "byr" e
      IssueYear = getProperty "iyr" e
      ExpirationYear = getProperty "eyr" e
      Height = getProperty "hgt" e
      HairColor = getProperty "hcl" e
      EyeColor = getProperty "ecl" e
      PassportId = getProperty "pid" e })

let validateBirthYear passport =
  match passport.BirthYear with
  | Some s -> 
    let year = Convert.ToInt32 s
    year >= 1920 && year <= 2002
  |_ -> false

let validateIssueYear passport =
  match passport.IssueYear with
  | Some s -> 
    let year = Convert.ToInt32 s
    year >= 2010 && year <= 2020
  |_ -> false

let validateExpirationYear passport =
  match passport.ExpirationYear with
  | Some s -> 
    let year = Convert.ToInt32 s
    year >= 2020 && year <= 2030
  |_ -> false

let validateHeight passport =
  match passport.Height with
  | Some s -> 
    if s.EndsWith "cm" then 
      let height = Convert.ToInt32 (s.Replace("cm", ""))
      height >= 150 && height <= 193
    else if s.EndsWith "in" then
      let height = Convert.ToInt32 (s.Replace("in", ""))
      height >= 59 && height <= 76
    else false
  | None -> false

let validateHairColor passport = 
  match passport.HairColor with
  | Some s -> (regex "^#[0-9a-z]{6}$" passport.HairColor.Value).IsSome
  | None -> false

let validateEyeColor passport =
  let validColors = [
    "amb"
    "blu"
    "brn"
    "gry"
    "grn"
    "hzl"
    "oth"
  ]
  match passport.EyeColor with
  | Some s -> validColors |> List.exists(fun e -> e = s)
  | None -> false

let validatePid passport =
  match passport.PassportId with
  | Some s -> (regex @"^\d{9}$" s).IsSome
  | None -> false

let validators = [
  validateBirthYear
  validateExpirationYear
  validateEyeColor
  validateHairColor
  validateHeight
  validateIssueYear
  validatePid
]

let validate passport =
  validators
  |> List.map (fun f -> f passport)
  |> List.fold (&&) true

let filterValid passports = 
  passports
  |> List.filter validate

let ``number of valid passports`` = 
  passports
  |> filterValid
  |> List.length