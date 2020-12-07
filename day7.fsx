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
    { Color = Regex.Match( e, @"([\w]+\s+){2}").Value.Trim()
      Content = 
        Regex.Matches(e, @"(\d.+?)(?:bags?\s*|$)")
        |> Seq.cast<Match>
        |> Seq.toList
        |> List.map(fun m -> 
          Regex.Match(m.Value.Trim(), @"([\w]+\s+){3}").Value.Substring(2).Trim(), int (m.Value.Substring(0, 1)))})

let getBag color = 
  bags
  |> List.filter (fun bag -> bag.Color = color)
  |> List.exactlyOne

let rec canHoldShinyGold bag = 
  let canHoldDirectly = 
    bag.Content
    |> List.exists(fun e -> fst e = "shiny gold")
  if canHoldDirectly 
    then true
  else 
    bag.Content
    |> List.map (fun e -> getBag(fst e))
    |> List.exists(fun e -> canHoldShinyGold e)

let ``bags that can hold shiny gold`` = 
  bags 
  |> List.filter canHoldShinyGold 
  |> List.length

let rec countContainedBags bag : int =
  let containedDirectly = 
    bag.Content
    |> List.sumBy (fun e -> snd e)
  let containedByInner = 
    bag.Content
    |> List.map(fun e -> getBag (fst e), snd e)
    |> List.sumBy(fun e -> countContainedBags (fst e) * (snd e))
  containedDirectly + containedByInner
  
let shinyGold = 
  bags
  |> List.filter(fun b -> b.Color = "shiny gold")
  |> List.exactlyOne

let ``bags inside the shiny gold`` = countContainedBags shinyGold

printf "Answer to part 1: %i \n" ``bags that can hold shiny gold``
printf "Answer to part 2: %i \n" ``bags inside the shiny gold``