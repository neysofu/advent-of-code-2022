module day10

import std/os/readline


type instruction
  Noop
  AddX( v : int )


pub fun main()
  val lines = lines-until-empty()
  val instructions = lines.map(parse-instruction)

  val x-reg-by-cycle = instructions.execute(1)
  val signal-strengths = x-reg-by-cycle.map-indexed(fn (idx, x)
    // Cycle counting is 1-based!
    x * (idx + 1)
  )

  val part1-idxs = [20, 60, 100, 140, 180, 220]
  val part1-signal-strengths = part1-idxs.map(fn (i) -> signal-strengths[i - 1].take())

  val pixels = x-reg-by-cycle.map-indexed(fn (idx, x)
    val crt-pixel-i = idx % 40
    val pixel-is-visible = (crt-pixel-i - x).abs() <= 1
    if pixel-is-visible then "█" else " "
  )

  println("Part 1: " ++ part1-signal-strengths.sum().show())
  println("")
  // Part 2
  pixels.parts(40).map(join).intersperse("\n").join().println()

fun execute(instructions : list<instruction>, x-reg : int) : <div, exn> list<int>
  match instructions
    Nil -> [x-reg]
    Cons(Noop, rest) -> [x-reg].append(rest.execute(x-reg))
    Cons(AddX(v), rest) -> [x-reg, x-reg].append(rest.execute(x-reg + v))

fun take(maybe : maybe<a>) : exn a
  match maybe
    Just(a) -> a
    Nothing -> throw("Unexpected Nothing")

fun parts(l : list<a>, n : int) : div list<list<a>>
  if l.length() == 0 then
    []
  else
    [l.take(n)].append(l.drop(n).parts(n))

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

fun parse-instruction(s : string) : exn instruction
  match s.split(" ")
    ["noop"] -> Noop
    ["addx", v] -> AddX(v.parse-int().take())
    _ -> throw("Bad instruction syntax: " ++ s)
