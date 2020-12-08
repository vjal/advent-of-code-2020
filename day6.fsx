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
    group.ToCharArray()
    |> Seq.toList
    |> List.distinct
    |> List.filter (fun e -> ['a' .. 'z'] |> List.contains e)
    |> List.filter (fun answers -> 
      group.Split ([|"\n"|],  StringSplitOptions.RemoveEmptyEntries)
      |> Seq.toList
      |> List.forall(fun e -> 
        e.ToCharArray() 
        |> Seq.toList 
        |> List.contains answers))
    |> List.length)
  |> List.sum