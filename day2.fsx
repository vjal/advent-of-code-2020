let input = 
  "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"

type Password = {
  GivenPassword: string
  RequiredCharacter: char
  RequiredMin: int
  RequiredMax: int
}

let passwords = 
  input.Split [|'\n'|]
  |> Array.toList
  |> List.map(fun e ->
    let parts = e.Trim().Split [|' '|] |> Array.toList
    {
      GivenPassword = parts |> List.last
      RequiredCharacter = parts.[1].[0]
      RequiredMin = (parts.[0].Split [|'-'|]).[0] |> int
      RequiredMax = (parts.[0].Split [|'-'|]).[1] |> int
    })

let validate (password: Password) : bool =
  let numberOfRequireds = 
    password.GivenPassword 
    |> Seq.toList
    |> List.sumBy (fun c -> if c = password.RequiredCharacter then 1 else 0) 
  (numberOfRequireds >= password.RequiredMin && numberOfRequireds <= password.RequiredMax)

let countValid (passwords: Password list) =
  passwords
  |> List.map validate
  |> List.filter id
  |> List.length

countValid passwords;;


// part 2

let validate2 (password: Password) = 
  let first = password.GivenPassword.[password.RequiredMin - 1] = password.RequiredCharacter
  let second = password.GivenPassword.[password.RequiredMax - 1] = password.RequiredCharacter
  first <> second

// tests
let t = {
  GivenPassword = "baabbcc"
  RequiredCharacter = 'a'
  RequiredMin = 1
  RequiredMax = 3
}
validate2 t;;

let countValid2 (passwords: Password list) =
  passwords
  |> List.map validate2
  |> List.filter id
  |> List.length

countValid2 passwords;;


