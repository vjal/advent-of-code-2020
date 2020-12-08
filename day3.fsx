let testInput = 
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"

let map = 
  testInput.Split '\n' 
    |> Seq.toList 
    |> List.map(fun e -> e.ToCharArray() |> Seq.toList)

type State =
  { CurrentPosition: int * int
    Trees: int
    Opens: int }

let start =
  { CurrentPosition = 0, 0
    Trees = 0
    Opens = 0 }

let mapWidth = map.[0] |> List.length
let mapHeight = map |> List.length

let getNextPosition position slope = 
  let slopeX, slopeY = slope
  let x, y = position
  (if x + slopeX > mapWidth - 1 then x + slopeX - mapWidth else x + slopeX), y + slopeY

let checkPosition position = 
  let x, y = position
  map.[y].[x]

let step slope state = 
  let nextPosition = getNextPosition state.CurrentPosition slope
  let nextChar = checkPosition nextPosition
  { 
    CurrentPosition = nextPosition
    Trees = if nextChar = '#' then state.Trees + 1 else state.Trees
    Opens = if nextChar = '.' then state.Opens + 1 else state.Opens
  }

let rec stepThrough slope state =
  if (snd state.CurrentPosition) + snd slope >= mapHeight
  then state
  else 
    state 
    |> step slope
    |> stepThrough slope

// part 1
let ``number of trees encountered`` = (start |> stepThrough (3,1)).Trees

// part 2
let slopes = [
  1,1
  3,1
  5,1
  7,1
  1,2
]
let ``product of trees in all slopes`` = 
  slopes
  |> List.map (fun slope ->  (start |> stepThrough slope).Trees |> bigint)
  |> List.fold (*) (bigint 1)
  