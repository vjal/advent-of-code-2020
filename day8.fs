open System
type Operation =
  | Acc
  | Jmp
  | Nop

type Instruction = Operation * int

type State =
  { Instructions: Instruction list
    CurrentPosition: int
    Accumulator: int 
    PreviousPositions: int list }

let input = System.IO.File.ReadAllText "day8-input.txt"

let instructions : Instruction list =
  input.Split ([|"\n"|],  StringSplitOptions.RemoveEmptyEntries)
  |> Seq.toList  
  |> List.map(fun e -> 
    let s = e.Split([|" "|], StringSplitOptions.None)
    let argument = int(s.[1])
    match s.[0] with
      | s when s = "acc" -> Acc, argument 
      | s when s = "jmp" -> Jmp, argument
      | _ -> Nop, argument )

let startState = {
  Instructions = instructions
  CurrentPosition = 0
  Accumulator = 0
  PreviousPositions = []
}

let rec run state: State = 
  let previousPositions = state.CurrentPosition :: state.PreviousPositions
  if state.PreviousPositions |> List.contains state.CurrentPosition 
    || state.CurrentPosition = List.length state.Instructions
  then state
  else 
    let next = 
      match state.Instructions.[state.CurrentPosition] with
      | Acc, arg ->  
        { state with 
            Accumulator = state.Accumulator + arg 
            CurrentPosition = state.CurrentPosition + 1 
            PreviousPositions = previousPositions }
      | Jmp, arg -> 
        { state with 
            CurrentPosition = state.CurrentPosition + arg 
            PreviousPositions = previousPositions }
      | Nop, _ -> 
        { state with 
            CurrentPosition = state.CurrentPosition + 1 
            PreviousPositions = previousPositions }
    run next

let part1 = (run startState).Accumulator

let fixInstruction (index: int) (list: Instruction list) =
  list
  |> List.mapi (fun i (op, arg) -> 
      match op with
      | Nop when i = index -> Jmp, arg
      | Jmp when i = index -> Nop, arg
      | _ -> op, arg)

let variations instructions =
  let count = List.length instructions - 1
  [0 .. count]
  |> List.map (fun i -> { startState with Instructions = fixInstruction i instructions })

let part2 = 
  (variations startState.Instructions
  |> List.map run
  |> List.filter (fun e -> e.CurrentPosition = List.length e.Instructions)
  |> List.exactlyOne).Accumulator