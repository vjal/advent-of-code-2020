let row (input: string) = 
  input.ToCharArray()
  |> Seq.toList
  |> List.take 7
  |> List.fold(fun list el ->
    let half = List.length list / 2
    if el = 'F'
    then List.take half list
    else List.skip half list)
    [0 .. 127]
  |> List.exactlyOne

let column (input: string) =
  input.ToCharArray()
  |> Seq.toList
  |> List.skip 7
  |> List.fold (fun list el ->
    let half = List.length list / 2
    if el = 'L'
    then List.take half list
    else List.skip half list)
    [0 .. 7]
  |> List.exactlyOne

let id input = row input * 8 + column input

let input = System.IO.File.ReadAllText "input-day5.txt"

let reservedSeats =
  input.Split [|'\n'|]
  |> Seq.toList
  |> List.map id
  |> List.sort

let first (list: 'a list) = list.[0]
let lowest = reservedSeats |> first
let highest = List.rev reservedSeats |> first
let allSeats = [lowest .. highest]

let mySeat = 
  allSeats 
  |> List.except reservedSeats
  |> List.exactlyOne

printf "Answer to part 1: %i \n" highest
printf "Answer to part 2: %i \n" mySeat