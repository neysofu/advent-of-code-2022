module day08

import std/os/readline

pub fun main()
  val grid = read-square-of-digits()
  println("Visible trees: " ++ grid.count-visible().show())
  println("Scenic score: " ++ grid.scenic-score().show())

fun read-square-of-digits() : <console, exn, div> list<list<int>>
  var line := readline()
  var grid := []
  while {line.is-notempty()}
    val line-of-trees = line.list().map(fn (c)
      c.string().parse-int().take()
    )
    grid := grid.append([line-of-trees])
    line := readline()
  
  grid

// --------------------------
//  Utilities
// --------------------------

// Rotates a square matrix.
fun rotate(mtx : list<list<a>>) : exn list<list<a>>
  val range = list(0, mtx.length() - 1)
  range.map(fn (i) -> mtx.map(fn(row) -> row[i].take()))

fun take(maybe : maybe<a>) : exn a
  match maybe
    Just(a) -> a
    Nothing -> throw("Unexpected Nothing")

// --------------------------
//  Scenic scores
// --------------------------

fun scenic-score(grid : list<list<int>>) : <console, exn, div> int
  val rotated = grid.rotate()

  val horizontal = 
    // West
    grid.map(scenic-scores)
    // East
    * grid.map(reverse).map(scenic-scores).map(reverse)
  val vertical =
    // North
    rotated.map(scenic-scores)
    // South
    * rotated.map(reverse).map(scenic-scores).map(reverse)

  val scenic-scores = horizontal.rotate() * vertical
  scenic-scores
    .map(fn (l) -> l.maximum())
    .maximum()

fun scenic-scores(trees : list<int>) : div list<int>
  fun score(height : int, rest : list<int>) : div int
    match rest
      Cons(next-height, tail) ->
        if height > next-height then
          1 + score(height, tail)
        else
          1
      Nil -> 0

  trees.map-peek(score)

fun (*)(a : list<list<int>>, b : list<list<int>>) : list<list<int>>
  a.zipwith(b, fn (x, y) -> x.zipwith(y, (*)))

// --------------------------
//  Visibility
// --------------------------

fun count-visible(grid : list<list<int>>) : <exn, div> int
  val rotated = grid.rotate()

  val horizontal =
    // West
    grid.map(get-visible)
    // East
    || grid.map(reverse).map(get-visible).map(reverse)
  val vertical =
    // North
    rotated.map(get-visible)
    // South
    || rotated.map(reverse).map(get-visible).map(reverse)

  val visible-trees = horizontal.rotate() || vertical
  visible-trees
    .map(fn (l) -> l.filter(fn (x) -> x).length())
    .sum()

fun (||)(a : list<list<bool>>, b : list<list<bool>>) : list<list<bool>>
  a.zipwith(b, fn (x, y) -> x.zipwith(y, (||)))

fun get-visible(trees : list<int>) : list<bool>
  get-visible(trees, -1)

fun get-visible(trees : list<int>, min-height : int) : list<bool>
  match trees
    Cons(height, rest) ->
      if height > min-height then
        Cons(True, get-visible(rest, height))
      else
        Cons(False, get-visible(rest, min-height))
    [] -> []
