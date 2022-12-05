module day05

import std/os/readline

type crane-model
  CrateMover9000
  CrateMover9001

struct supplies
  stacks : list<list<char>>

struct instruction
  num-crates : int
  source : int
  destination : int


pub fun main()
  val crane-model = CrateMover9001
  var supplies := parse-supplies()
  var line := readline()

  while {is-notempty(line)}
    val instruction = parse-instruction(line)
    supplies := crane-model.execute-instruction(supplies, instruction)
    line := readline()

  println(supplies.top-of-the-stacks().string())

fun execute-instruction(crane : crane-model, supplies : supplies, ins : instruction) : supplies
  Supplies(supplies.stacks.map-indexed fn (i, stack)
    if i == ins.source then
      stack.drop(ins.num-crates)
    else if i == ins.destination then
      val crates = supplies.stacks[ins.source].unwrap-or([]).take(ins.num-crates)
      match crane
        CrateMover9000 -> crates.reverse().append(stack)
        CrateMover9001 -> crates.append(stack)
    else
      stack
  )

fun top-of-the-stacks(supplies : supplies) : list<char>
  supplies.stacks.map(head).map(fn(x) -> x.unwrap-or(' '))

fun unwrap-or(a : maybe<a>, b : a) : a
  match a
    Just(x) -> x
    Nothing -> b

// --------------------------
//  Parsing utilities
// --------------------------

fun parse-instruction(line : string) : exn instruction
  val parts = line.split(" ").filter-map(fn(s) -> s.parse-int())

  match parts
    [num-crates, source, destination] ->
      // 0-inxeding so we subtract 1.
      Instruction(num-crates, source - 1, destination - 1)
    _ -> throw("Invalid instruction syntax")

fun parse-line-in-supplies(supplies : supplies, line : string) : div supplies
  var stacks := supplies.stacks
  var stack-i := 0
  var spaces-counter := 0

  line.list().foreach fn(c : char)
    if c.is-alpha() then
      while {stack-i >= stacks.length()}
        stacks := stacks.append([[]])
      stacks := stacks.map-indexed fn(i, stack)
        if i == stack-i then stack.append([c])
        else stack
      stack-i := stack-i + 1
      spaces-counter := 0

    // Stacks are separated by 4 characters (spaces and square brackets). We
    // also ignore digits because they are part of the description line at the
    // base of the stacks.
    if !c.is-alpha() && !c.is-digit() then
      spaces-counter := spaces-counter + 1
      // Each stack takes up 4 spaces.
      if spaces-counter == 4 then
        stack-i := stack-i + 1
        spaces-counter := 0
  
  Supplies(stacks)

fun parse-supplies() : <console, div, exn> supplies
  var supplies := Supplies([])
  var line := readline()

  while {is-notempty(line)}
    supplies := parse-line-in-supplies(supplies, line)
    line := readline()

  supplies
