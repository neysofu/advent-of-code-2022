module day04

import std/os/readline


effect fstream
  ctl read-line() : string

effect yield<a>
  ctl yield(x : a) : ()

type overlap-calculation-mode
  Fully
  Partial


pub fun main()
  var counter := 0

  with ctl yield(x : bool)
    counter := counter + int(x)
    resume(())
  with ctl read-line()
    val line = readline()
    resume(line)
  
  inner-main(Partial)
  println(counter)

fun inner-main(mode : overlap-calculation-mode) : <fstream, yield<bool>, div, exn> ()
  val input = read-line()
  if input.is-empty() then
    ()
  else
    val (assign1, assign2) = parse-pair-of-assignments(input)
    val overlaps = match mode
      Fully -> assign1.contains(assign2) || assign2.contains(assign1)
      Partial -> assign1.overlaps(assign2)
    yield(overlaps)
    inner-main(mode)


fun parse-pair-of-assignments(line : string) : exn (int-range, int-range)
  match line.split(",").map(parse-assignment)
    [assign1, assign2] -> (assign1, assign2)
    _ -> throw("Invalid number of assignments on line")

fun parse-assignment(ss : string) : exn int-range
  match ss.split("-").map(fn(s) parse-int(s))
    [Just(start), Just(end)] -> Int-range(start, end)
    _ -> throw("invalid assignment syntax: " ++ ss)


// Inclusive integer range.
struct int-range
  start : int
  end : int

fun contains(r1 : int-range, r2 : int-range) : bool
  r1.start <= r2.start && r2.end <= r1.end

fun overlaps(r1 : int-range, r2 : int-range) : bool
  val together = Int-range(r1.start.min(r2.start), r1.end.max(r2.end))
  together.count() < r1.count() + r2.count()

fun count(r : int-range) : int
  r.end - r.start + 1
