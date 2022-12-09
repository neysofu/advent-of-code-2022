module day09

import std/os/readline


type direction
  North
  West
  South
  East

struct move
  direction : direction
  distance : int

struct position
  x : int
  y : int

struct game-state
  knots : list<position>
  positions-visited-by-tail : list<position>

pub fun main()
  val lines = lines-until-empty()
  val moves = lines.map(parse-move)
  val steps = moves.flatmap(atomic)

  calculate-and-print-result(steps, 2)
  calculate-and-print-result(steps, 10)

fun calculate-and-print-result(steps : list<direction>, num-knots : int)
  val game-state = Game-state(Position(0, 0).replicate(num-knots), [])
  val game-states = steps.foldl([game-state], fn (gs, step)
    gs.append([move(gs.last().take(), step)])
  )

  val num-positions = game-states.last().take().positions-visited-by-tail.length()
  println("Number of unique tail positions (" ++ num-knots.show() ++ " knots): " ++ num-positions.show())

fun take(maybe : maybe<a>) : exn a
  match maybe
    Just(a) -> a
    Nothing -> throw("Unexpected Nothing")

// --------------------------
//  Parsing
// --------------------------

fun lines-until-empty() : <console, div, exn> list<string>
  var line := readline()
  var lines := []
  while {line.is-notempty()}
    lines := lines.append([line])
    line := readline()
  lines

fun parse-move(s : string) : exn move
  match s.split(" ")
    [dir, distance] -> Move(parse-direction(dir), parse-int(distance).take())
    _ -> throw("Unexpected move: " ++ s)

fun parse-direction(s : string) : exn direction
  match s
    "U" -> North
    "R" -> East
    "D" -> South
    "L" -> West
    _ -> throw("Unexpected direction: " ++ s)

// --------------------------
//  Game logic
// --------------------------

// Splits a move into atomic steps.
fun atomic(move : move) : div list<direction>
  if move.distance == 0 then
    []
  else
    [move.direction].append(atomic(move(distance = move.distance - 1)))

fun offset(dir : direction) : position
  match dir
    North -> Position(0, 1)
    East -> Position(1, 0)
    South -> Position(0, -1)
    West -> Position(-1, 0)

fun calculate-tail-move(head : position, tail : position) : position
  val offset = head - tail
  val touching = offset.x.abs() <= 1 && offset.y.abs() <= 1

  if touching then
    // They're touching already, tail doesn't move.
    Position(0, 0)
  else if offset.x == 0 then
    // Tail moves up or down.
    Position(0, offset.y / 2)
  else if offset.y == 0 then
    // Tail moves left or right.
    Position(offset.x / 2, 0)
  else
    // Tail moves diagonally.
    Position(
      offset.x.max-with-sign(1),
      offset.y.max-with-sign(1)
    )

fun move(gs : game-state, step : direction) : exn game-state
  val new-knot-positions = gs.knots.foldl([], fn(acc, tail)
    match acc.last()
      Nothing -> [tail + step.offset()]
      Just(head) ->
        val new-tail = tail + calculate-tail-move(head, tail)
        acc.append([new-tail])
  )

  val new-tail = new-knot-positions.last().take()
  Game-state(
    new-knot-positions,
    gs.positions-visited-by-tail.append-dedup(new-tail)
  )

// Append an element to a list, but only if it's not already in the list.
fun append-dedup(xs : list<position>, elem : position) : list<position>
  if xs.any(fn (s) -> s.x == elem.x && s.y == elem.y) then
    xs
  else
    [elem].append(xs)

fun max-with-sign(n : int, m : int) : int
  if n.abs() <= m.abs() then
    n
  else if n.is-neg() then
    0 - m
  else
    m

fun tail(gs : game-state) : exn position
  gs.knots.last().take()

fun (+)(p1 : position, p2 : position) : position
  Position(p1.x + p2.x, p1.y + p2.y)

fun (-)(p1 : position, p2 : position) : position
  Position(p1.x - p2.x, p1.y - p2.y)
