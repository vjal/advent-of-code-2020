open System
open System.Numerics

let input = System.IO.File.ReadAllText "day9-input.txt"
let numbers = 
  input.Split ([|"\n"|],  StringSplitOptions.RemoveEmptyEntries)
  |> Seq.map (fun e -> BigInteger.Parse(e))
  |> Seq.toList

let rec pairs list =
  match list with
  | head::tail -> List.map (fun e -> (head, e)) tail @ pairs tail
  | [] -> []

let isValid (index: int) (list: bigint list) =
  if index <= 24 
  then true
  else 
    let splitted = 
      list
      |> List.splitAt (index)
      |> fst
    let preample =
      splitted
      |> List.skip ((List.length splitted) - 25)

    preample
    |> pairs
    |> List.map(fun (a, b) -> a + b)
    |> List.contains list.[index]

let part1 = 
  (numbers
  |> List.mapi (fun i e -> isValid i numbers, e)
  |> List.filter(fun (isValid, _) -> not isValid)).[0] |> snd

let takelist list = 
  list
  |> List.mapi(fun i e -> List.take (i+1) list)

let skiplist list = 
  list
  |> List.mapi(fun i e -> List.skip (i) list)

let allSetsSum =
  numbers
  |> takelist 
  |> List.map(fun e -> skiplist e)
  |> List.concat
  |> List.filter (fun l -> List.length l >= 2)
  |> List.map(fun l -> List.sum l, l)

let theSet =
  (allSetsSum
  |> List.filter (fun (sum, e) -> sum = part1)).[0] |> snd

let part2 = List.min theSet + List.max theSet