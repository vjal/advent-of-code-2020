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

type Position = int * int

type Map = char list list

let map = testInput.Split '\n' |> Seq.toList |> List.map(fun e -> e.ToCharArray() |> Seq.toList)

type State =
  { CurrentPosition: Position
    Trees: int
    Opens: int }

let start =
  { CurrentPosition = 0, 0
    Trees = 0
    Opens = 0 }

let mapWidth = map.[0] |> List.length
let mapHeight = map |> List.length

let getNextPosition position (slope: int*int) : Position = 
  let slopeX,slopeY = slope
  let x, y = position
  (if x + slopeX > mapWidth - 1 then x + slopeX - mapWidth else x + slopeX), y + slopeY

let checkPosition position: char = 
  let x, y = position
  map.[y].[x]

let step (state: State) slope : State = 
  let nextPosition = getNextPosition state.CurrentPosition slope
  let nextChar = checkPosition nextPosition
  { 
    CurrentPosition = nextPosition
    Trees = if nextChar = '#' then state.Trees + 1 else state.Trees
    Opens = if nextChar = '.' then state.Opens + 1 else state.Opens
  }

let rec stepThrough state slope : State =
  printf "%i, %i \n" <|| state.CurrentPosition
  if (snd state.CurrentPosition) + snd slope >= mapHeight
  then state
  else stepThrough (step state slope) slope

stepThrough start (3,1)

let slopes = [
  1,1
  3,1
  5,1
  7,1
  1,2
]

let ``product of trees in all slopes`` : bigint = 
  slopes
  |> List.map (fun e -> bigint (stepThrough start e).Trees)
  |> List.fold (*) (bigint 1)
  