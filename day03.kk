module day03

import std/os/readline

pub type part
  Part1
  Part2

pub fun main()
  val part = Part2

  var total-score := 0
  var line := readline()

  while {is-notempty(line)}
    val strings = match part
      Part1 ->
        val (a, b) = split-in-half(line)
        [a, b]
      Part2 -> [line, readline(), readline()].map(list)

    val line-score = common-chars(strings).map(char-to-score).sum()
    total-score := total-score + line-score
    line := readline()

  println(total-score)

fun split-in-half(s : string) : (list<char>, list<char>)
  val half = s.slice().count() / 2
  s.list().split(half)

fun char-to-score(c : char) : int
  if is-lower(c) then
    int(c) - int('a') + 1
  else if is-upper(c) then
    int(c) - int('A') + 27
  else
    0

// Finds all characters that are common to all strings.
// The specific algorithm that we're using is O(max(len)^n), which is dumb and
// disgusting. But heh, fast enough for our use case.
fun common-chars(strings : list<list<char>>) : <div, exn> list<char>
  strings.foldl1(fn(s1, s2)
    s1
      .without-duplicates()
      .filter(fn(c) -> s2.any(fn(c2) -> c == c2))
  )

fun without-duplicates(l : list<char>) : div list<char>
  match l
    Nil -> Nil
    Cons(head, tail) ->
      Cons(head, without-duplicates(tail.remove(fn(x) -> x == head)))
