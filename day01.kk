module day01

import std/os/readline

fun main()
  // Part 1: 1 elf.
  // Part 2: 3 elves.
  val num-elves-to-track = 3

  var max-calories := replicate(0, num-elves-to-track)
  var current-calories := 0

  var line := readline()
  while {is-notempty(line)}

    while {is-notempty(line)}
      // FIXME: use parse-int and deal with errors.
      current-calories := current-calories + parse-int-default(line)
      line := readline()
    
    max-calories := replace-min-with(max-calories, current-calories)
    current-calories := 0
    line := readline()

  println(sum(max-calories))

pub fun replace-min-with( xs : list<int>, new-elem : int ) : list<int>
  match xs
    Nil -> Nil
    Cons(h,tail) ->
      // This has the nice side effect of keeping the list sorted in descending
      // order, if it already is.
      if h < new-elem
        then Cons(new-elem, replace-min-with(tail, h))
        else Cons(h, replace-min-with(tail, new-elem))
