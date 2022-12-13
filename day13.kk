module day13

import std/os/readline

type packet
  List(l : list<packet>)
  Integer(x : int)


pub fun main()
  val lines = lines-until-n-empty(2)

  // Three lines at a time and drop the last one (it's empty).
  val packets = lines.parts(3).map(fn (l)
    val pair = l.take(2)
    (l[0].take().list().parse-packet(), l[1].take().list().parse-packet())
  )

  val cmps = packets.map(fn (pair) -> pair.fst.cmp(pair.snd))
  val sum-of-idxs = cmps.map-indexed(fn (i, cmp)
    if cmp == 1 then i + 1 else 0
  ).sum()

  println("Part 1: " ++ sum-of-idxs.show())

  val all-packets = lines
    .filter(is-notempty)
    .map(list)
    .map(parse-packet)
    .append([List([List([Integer(2)])])])
    .append([List([List([Integer(6)])])])

  val p1 = all-packets
    .map(fn (p)
      if p.cmp(List([List([Integer(2)])])) > 0 then
        1
      else
        0
    ).sum() + 1
  val p2 = all-packets
    .map(fn (p)
      if p.cmp(List([List([Integer(6)])])) > 0 then
        1
      else
        0
    ).sum() + 1

  println("Decoder key (part 2): " ++ (p1 * p2).show())


fun take(maybe : maybe<a>) : exn a
  match maybe
    Just(a) -> a
    Nothing -> throw("Unexpected Nothing")

fun or(maybe : maybe<a>, default : a) : a
  match maybe
    Just(a) -> a
    Nothing -> default

fun parts(l : list<a>, n : int) : div list<list<a>>
  if l.length() == 0 then
    []
  else
    [l.take(n)].append(l.drop(n).parts(n))

fun bubblesort(xs : list<a>, cmp : (a, a) -> <div|e> int) : <div|e> list<a>
  val sorted = match xs
    [] -> []
    [x] -> [x]
    Cons(a, Cons(b, rest)) ->
      if a.cmp(b) > 0 then
        [a].append(Cons(b, rest).bubblesort(cmp))
      else
        [b].append(Cons(a, rest).bubblesort(cmp))

  if sorted.zipwith(xs, cmp).all(fn (x) -> x == 0) then
    sorted
  else
    sorted.bubblesort(cmp)

// --------------------------
//  Packet logic
// --------------------------

fun show(p : packet) : div string
  match p
    List(l) -> "[" ++ l.map(fn (p) -> p.show()).join(", ") ++ "]"
    Integer(x) -> x.show()

fun cmp(l1 : list<packet>, l2 : list<packet>) : <exn, div> int
  match (l1, l2)
    ([], []) -> 0
    ([], _) -> 1
    (_, []) -> 0 - 1
    (Cons(x1, p1), Cons(x2, p2)) ->
      val res = x1.cmp(x2)
      if res == 0 then
        p1.cmp(p2)
      else
        res

// 1 right order, 0 keep looking, -1 wrong order 
fun cmp(p1 : packet, p2 : packet) : <exn, div> int
  match (p1, p2)
    (Integer(x1), Integer(x2)) ->
      if x1 < x2 then
        1
      else if x1 > x2 then
        0 - 1
      else
        0
    (Integer(x1), List(l2)) -> List([Integer(x1)]).cmp(p2)
    (List(l1), Integer(x2)) -> p1.cmp(List([Integer(x2)]))
    (List(l1), List(l2)) -> l1.cmp(l2)

// --------------------------
//  Parsing
// --------------------------

fun parse-packet(src : list<char>) : <div, exn> packet
  match (src.head().take(), src.last().take())
    ('[', ']') -> parse-packet-list(src.drop(1).take(src.length() - 2))
    (_, _) -> Integer(src.string().parse-int().take())

fun parse-packet-list(src : list<char>) : <div, exn> packet
  if src.is-empty() then
    return List([])

  val i-range = list(0, src.length() - 1).zip(src)
  val commas = i-range.foldl((0, []), fn(a, item)
    val acc: (int, list<int>) = a
    val i = item.fst
    val c = item.snd
    val bracket-count = acc.fst
    val seps = acc.snd
    match c
      '[' -> (bracket-count + 1, seps)
      ']' -> (bracket-count - 1, seps)
      ',' ->
        if bracket-count == 0 then
          (0, seps.append([i]))
        else
          acc
      _ -> acc
  ).snd

  val separators = [0 - 1].append(commas).append([src.length()])
  val elements = separators.map-peek(fn (separator, rest)
    match rest.head()
      Just(next-separator) ->
        val substring = src.drop(separator + 1).take(next-separator - separator - 1)
        [parse-packet(substring)]
      Nothing -> []
  )

  List(elements.concat())

fun lines-until-n-empty(n : int) : <console, div, exn> list<string>
  var line := readline()
  var lines := []
  var count-empty := 0
  while {count-empty < n}
    lines := lines.append([line])
    line := readline()
    if line.is-empty() then
      count-empty := count-empty + 1
    else
      count-empty := 0
  lines
