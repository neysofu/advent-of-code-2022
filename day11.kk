module day11

import std/os/readline


struct monkey-state
  items : list<int>
  worry-formula : worry-formula
  num-inspected-items : int
  test-divisible-by : int
  throw-to-if-true : int
  throw-to-if-false : int

struct worry-formula
  op : string
  arg1 : maybe<int>
  arg2 : maybe<int>


pub fun main()
  val groups-of-lines = groups-of-lines()
  val monkey-states = groups-of-lines.map(parse-monkey-state)
  solve-problem(monkey-states, 20, 3)
  solve-problem(monkey-states, 10000, 1)

pub fun solve-problem(monkey-states : list<monkey-state>, num-rounds : int, worry-divider : int)
  val monkey-states-round-after-round = monkey-states.play-rounds(num-rounds, worry-divider)
  val num-inspected = monkey-states-round-after-round
    .last()
    .take()
    .map(num-inspected-items)
  val monkey-business = num-inspected
    .bubblesort()
    .reverse()
    .take(2)
    .foldl1((*))

  println("Inspected items:")
  println(num-inspected.show())
  println("Monkey business: " ++ monkey-business.show())

fun take(maybe : maybe<a>) : exn a
  match maybe
    Just(a) -> a
    Nothing -> throw("Unexpected Nothing")

fun or(maybe : maybe<a>, default : a) : a
  match maybe
    Just(a) -> a
    Nothing -> default

fun bubblesort(xs : list<int>) : div list<int>
  val sorted = match xs
    [] -> []
    [x] -> [x]
    Cons(a, Cons(b, rest)) ->
      if a > b then
        [b].append(Cons(a, rest).bubblesort())
      else
        [a].append(Cons(b, rest).bubblesort())

  if sorted.zipwith(xs, fn (a, b) -> (a == b, 0)).all(fst) then
    sorted
  else
    sorted.bubblesort()

// --------------------------
//  Parsing
// --------------------------

fun parse-monkey-state(lines : list<string>) : <exn, console> monkey-state
  val items = lines[1]
    .take()
    .split(":")[1]
    .take()
    .list()
    .filter(fn (c) -> c != ',')
    .string()
    .split(" ")
    .filter(is-notempty)
    .map(fn (s) -> s.parse-int().take())

  val worry-formula-args = lines[2]
    .take()
    .split("=")[1]
    .take()
    .split(" ")
  val worry-formula = Worry-formula(
    worry-formula-args[2].take(),
    worry-formula-args[1].take().parse-int(),
    worry-formula-args[3].take().parse-int()
  )

  val test-divisible-by = lines[3].take().parse-last-number()
  val throw-to-if-true = lines[4].take().parse-last-number()
  val throw-to-if-false = lines[5].take().parse-last-number()

  Monkey-state(
    items,
    worry-formula,
    0,
    test-divisible-by,
    throw-to-if-true,
    throw-to-if-false
  )

fun parse-last-number(line : string) : exn int
  line.split(" ").last().take().parse-int().take()

fun lines-until-empty() : <console, div, exn> list<string>
  var line := readline()
  var lines := []
  while {line.is-notempty()}
    lines := lines.append([line])
    line := readline()
  lines

fun groups-of-lines() : <console, div, exn> list<list<string>>
  var groups := []
  var continue := True
  while {continue}
    val group = lines-until-empty()
    if group.length() == 0 then
      continue := False
    else
      groups := groups.append([group])
  groups

// --------------------------
//  Monkey logic
// --------------------------

fun play-rounds(monkey-states : list<monkey-state>, rounds : int, worry-divider : int) : <div, exn> list<list<monkey-state>>
  if rounds == 0 then
    []
  else
    val new-states = monkey-states.play-round(worry-divider)
    [new-states].append(play-rounds(new-states, rounds - 1, worry-divider))

fun inspected-items(monkey-states : list<list<monkey-state>>, n : int) : <div, exn> list<int>
  list(0, n).map(fn (i)
    monkey-states.map(fn (ms) -> ms[i].take().items.length()).sum()
  )

fun eval-worry(wf : worry-formula, old : int) : exn int
  match (wf.arg1, wf.op, wf.arg2)
    (a, "+", b) -> a.or(old) + b.or(old)
    (a, "*", b) -> a.or(old) * b.or(old)
    _ -> throw("Unexpected op: " ++ wf.op)

fun play-turn(monkeys : list<monkey-state>, monkey : monkey-state, i : int, worry-divider : int) : exn list<monkey-state>
  val mod = monkeys.map(test-divisible-by).foldl1((*))
  val thrown-to = monkey.items.foldl([], fn (acc, old-worry) ->
    val new-worry = (monkey.worry-formula.eval-worry(old-worry) / worry-divider)
    val is-divisible = new-worry % monkey.test-divisible-by == 0
    val throw-to = if is-divisible then
      monkey.throw-to-if-true
    else
      monkey.throw-to-if-false

    if worry-divider == 1 then
      acc.append([(throw-to, new-worry % mod)])
    else
      acc.append([(throw-to, new-worry)]) // No modulo
  )

  monkeys.map-indexed(fn (idx, m)
    if idx == i then
      m(items = [], num-inspected-items = m.num-inspected-items + thrown-to.length())
    else
      val new-items = thrown-to.filter(fn (x) -> x.fst == idx).map(snd)
      m(items = m.items.append(new-items))
  )

fun play-round(m : list<monkey-state>, worry-divider : int) : <div, exn> list<monkey-state>
  var monkeys := m
  var i := 0
  while {i < monkeys.length()}
    monkeys := play-turn(monkeys, monkeys[i].take(), i, worry-divider)
    i := i + 1
  
  monkeys
