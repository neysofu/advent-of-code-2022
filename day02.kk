module day02

import std/os/readline

pub type move
  Rock
  Paper
  Scissors

pub type round-outcome
  Win
  Lose
  Draw

pub type decryption-mode
  Part1
  Part2

pub fun main()
  val decryption-mode = Part2

  var total-score := 0
  var line := readline()
  while {is-notempty(line)}
    total-score := total-score + line-score(line, decryption-mode)
    line := readline()

  println(total-score)

fun line-score(line : string, decryption-mode : decryption-mode) : exn int
  val columns = line.split(" ")
  val (c1, c2) = match columns
    [c1, c2] -> (c1, c2)
    _ -> throw("Wrong number of columns: " ++ line)
  
  val opponent-move = column-value-to-move(c1)
  val my-move = match decryption-mode
    Part1 -> column-value-to-move(c2)
    Part2 ->
      val wanted-outcome = match c2
        "X" -> Lose
        "Y" -> Draw
        "Z" -> Win
        _ -> throw("Bad column value: " ++ line)
      move-based-on-wanted-outcome(opponent-move, wanted-outcome)

  val score = round-score(opponent-move, my-move)
  score

fun column-value-to-move(column : string) : exn move
  match column
    "A" -> Rock
    "X" -> Rock
    "B" -> Paper
    "Y" -> Paper
    "C" -> Scissors
    "Z" -> Scissors
    _ -> throw("Bad column value: " ++ column)

// ---------------------
//  Game utilities
// ---------------------

pub fun move-based-on-wanted-outcome(opponent-move : move, outcome : round-outcome) : move
  match outcome
    Draw -> opponent-move
    Win -> match opponent-move
      Rock -> Paper
      Paper -> Scissors
      Scissors -> Rock
    Lose -> match opponent-move
      Rock -> Scissors
      Paper -> Rock
      Scissors -> Paper

pub fun round-score(opponent : move, mine : move) : int
  val outcome-score = match round-outcome(opponent, mine)
    Win -> 6
    Draw -> 3
    Lose -> 0
  val shape-score = match mine
    Rock -> 1
    Paper -> 2
    Scissors -> 3
  outcome-score + shape-score

pub fun round-outcome(opponent : move, mine : move) : round-outcome
  fun beats(a : move, b : move) : bool
    match (a, b)
      (Rock, Scissors) -> True
      (Paper, Rock) -> True
      (Scissors, Paper) -> True
      _ -> False

  if beats(opponent, mine) then
    Lose
  else if beats(mine, opponent) then
    Win
  else
    Draw
