open System
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllText "day7-input.txt"

type Bag = {
  Color: string
  Content: (string * int) list
}

let bags = 
  input.Split ([|"\n"|],  StringSplitOptions.RemoveEmptyEntries)
  |> Seq.toList
  |> List.map(fun e -> 
    let matches = 
      Regex.Matches( e, @"(^([\w]+\s+){2})|(\d ([\w]+\s+){2})")
      |> Seq.cast<Match>
      |> Seq.map (fun e -> e.Value)

    { Color = (matches |> Seq.head).Trim()
      Content = 
        matches 
        |> Seq.tail
        |> Seq.map(fun e -> e.Substring(2).Trim(), int(e.[0]))
        |> Seq.toList })

let getBag color = 
  bags
  |> List.filter (fun bag -> bag.Color = color)
  |> List.exactlyOne

let rec canHoldShinyGold bag =     
  if List.exists(fun (color, _) -> color = "shiny gold") bag.Content
  then true
  else 
    bag.Content
    |> List.exists(fun (color, _) -> 
      color 
      |> getBag 
      |> canHoldShinyGold)

let ``bags that can hold shiny gold`` = 
  bags 
  |> List.filter canHoldShinyGold 
  |> List.length

printf "Answer to part 1: %i \n" ``bags that can hold shiny gold``

// part 2

let rec countContainedBags bag : int =
  bag.Content
  |> List.sumBy(fun (color, count) -> count + countContainedBags (getBag color) * count)
  
let shinyGold = 
  bags
  |> List.filter(fun b -> b.Color = "shiny gold")
  |> List.exactlyOne

printf "Answer to part 2: %i \n" countContainedBags shinyGold