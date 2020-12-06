open System
let input = System.IO.File.ReadAllText "day6-input.txt"

let part1 = 
  input.Split ([|"\n\n"|],  StringSplitOptions.RemoveEmptyEntries)
  |> Seq.toList
  |> List.map (fun e -> 
    e.ToCharArray()
    |> Seq.toList
    |> List.distinct
    |> List.filter (fun i -> ['a' .. 'z'] |> List.contains i)
    |> List.length)
  |> List.sum

let part2 = 
  input.Split ([|"\n\n"|],  StringSplitOptions.RemoveEmptyEntries)
  |> Seq.toList
  |> List.map (fun group -> 
    let lines = 
      group.Split ([|"\n"|],  StringSplitOptions.RemoveEmptyEntries)
      |> Seq.toList
      |> List.map(fun e -> e.ToCharArray() |> Seq.toList)
    group.ToCharArray()
    |> Seq.toList
    |> List.distinct
    |> List.filter (fun i -> ['a' .. 'z'] |> List.contains i)
    |> List.filter (fun answer -> lines |> List.forall(fun e -> e |> List.contains answer))
    |> List.length)
  |> List.sum