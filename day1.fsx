let input = [
  1721
  979
  366
  299
  675
  1456
]

// part 1
let ``product of 2 numbers that add up to 2020`` = 
  input
  |> List.filter(fun first -> List.exists (fun second -> first + second = 2020 && first <> second) input)
  |> List.fold (*) 1

// part 2
let ``product of 3 numbers that add up to 2020`` = 
  input
  |> List.filter(fun first -> 
    input 
    |> List.exists(fun second -> 
      input 
      |> List.exists(fun third -> first + second + third = 2020 && third <> first && third <> second && first <> second)))
  |> List.fold (*) 1