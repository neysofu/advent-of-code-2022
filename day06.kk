module day06

import std/os/readline


pub fun main()
  // readline() is limited to 1023 characters.
  var input := ""
  var line := readline()
  while {line.is-notempty()}
    input := input ++ line
    line := readline()

  // 4 for part 1, 14 for part 2.
  val marker-count = 14 
  val start-of-packet = start-of-packet(input, marker-count).unwrap-or(-1)
  println(start-of-packet)


fun start-of-packet(input : string, marker-count : int) : div maybe<int>
  fun find-packet(acc : list<char>, i : int, chars : list<char>) : div maybe<int>
    if acc.is-valid-packet(marker-count) then
      Just(i)
    else if chars.is-empty() then
      Nothing
    else
      val new-acc =
        if acc.length() < marker-count then acc.append(chars.take(1))
        else acc.drop(1).append(chars.take(1))
      new-acc.find-packet(i + 1, chars.drop(1))

  find-packet([], 0, input.list())

fun is-valid-packet(acc : list<char>, marker-count) : bool
  acc.length() == marker-count && acc.all-unique()

// Dumb O(n^2) algorithm.
fun all-unique(elems : list<char>) : bool
  val without-duplicates: list<char> = elems.foldl([]) fn (acc, c)
    if acc.any(fn (x) -> x == c) then acc
    else acc.append([c])
  elems.length() == without-duplicates.length()

fun unwrap-or(a : maybe<a>, b : a) : a
  match a
    Just(x) -> x
    Nothing -> b
