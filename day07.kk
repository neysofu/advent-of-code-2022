// Note: this solution is really overengineered, for the sake of feature
// exploration and fun:
// - Parsing could be greatly reduced by reading the whole file into memory.
// - Using effects for reading lines is overkill.
module day07

import std/os/readline


effect peekable-stdin
  ctl peek-line() : string
  ctl read-line() : string


pub fun main()
  // We store the next line in a variable so that we can peek at it.
  // Note: this peeking thing is only necessary because I want to avoid
  // loading the whole file into memory (fun challenge).
  var next-line := Nothing
  // Simple state machine to keep track of whether the next line has been peeked
  // at or not.
  with handler
    ctl peek-line()
      match next-line
        Just(l) -> resume(l)
        Nothing ->
          val l = readline()
          next-line := Just(l)
          resume(l)
    ctl read-line()
      match next-line
        Just(l) ->
          next-line := Nothing
          resume(l)
        Nothing -> resume(readline())

  inner-main()

fun inner-main() : <exn, peekable-stdin, console, div> ()
  val root = read-terminal-and-build-fs()
  val dirs-with-sizes = root.dirs-with-sizes()

  val output1 = dirs-with-sizes.filter-map(fn (dir)
    if dir.snd <= 100000 then Just(dir.snd)
    else Nothing
  ).sum()

  val total-space-used = dirs-with-sizes.head().take().snd
  val available-space = 70000000 - total-space-used
  val space-to-free = 30000000 - available-space

  val output2 = dirs-with-sizes.foldl(total-space-used, fn (best, dir)
    if dir.snd >= space-to-free then best.min(dir.snd)
    else best
  )

  println(root)
  println("")
  println("Total size of dirs <= 100000: " ++ output1.show())
  println("Size of dir to delete: " ++ output2.show())

fun read-terminal-and-build-fs() : <exn, peekable-stdin, div> dir
  var fs := Fs-state([empty-dir("/")])
  while {peek-line().is-notempty()}
    val (cmd, ls-output) = parse-cmd-with-output()
    fs := fs.run(cmd, ls-output)

  // Return the root directory.
  fs.stack.head().take()

fun take(maybe : maybe<a>) : exn a
  match maybe
    Just(a) -> a
    Nothing -> throw("Unexpected Nothing")

// --------------------------
//  Parsing utilities
// --------------------------

type command
  List
  ChangeDir(cd : change-dir)

type change-dir
  ChangeDirToParent
  ChangeDirToRoot
  ChangeDirInto(dir : string)

fun parse-cmd-with-output() : <exn, peekable-stdin, div> (command, list<fs-object>)
  val cmd = parse-command(read-line())
  var output := []

  while {peek-line().is-notempty() && peek-line().starts-with("$").is-nothing()}
    val fs-obj = parse-fs-object(read-line())
    output := output.append([fs-obj])

  (cmd, output)

fun parse-command(line : string) : exn command
  match line.split(" ")
    ["$", "cd", ".."] -> ChangeDir(ChangeDirToParent)
    ["$", "cd", "/"] -> ChangeDir(ChangeDirToRoot)
    ["$", "cd", dir] -> ChangeDir(ChangeDirInto(dir))
    ["$", "ls"] -> List
    _ -> throw("Invalid command syntax: " ++ line)

fun parse-fs-object(line : string) : exn fs-object
  match line.split(" ")
    ["dir", name] -> Directory(empty-dir(name))
    [size, name] -> match parse-int(size)
      Just(s) -> File(name, s)
      Nothing -> throw("Invalid file size: " ++ size)

// --------------------------
//  File system operations
// --------------------------

struct fs-state
  stack : list<dir>

fun run(fs : fs-state, cmd : command, ls-output : list<fs-object>) : <exn, div> fs-state
  match cmd
    ChangeDir(cd-cdm) -> fs.cd(cd-cdm)
    List -> fs.ls(ls-output)

fun cd(fs : fs-state, cd : change-dir) : exn fs-state
  match cd
    ChangeDirToParent -> fs(stack = fs.stack.init())
    ChangeDirToRoot -> fs(stack = fs.stack.take(1))
    ChangeDirInto(dir-name) ->
      val dir = fs.stack.last().take()
      fs(stack = fs.stack.append([dir.get-child(dir-name)]))

fun ls(fs : fs-state, children : list<fs-object>) : <exn, div> fs-state
  // After updating a directory, we need to update pointers in the stack
  // (immutability).
  fun repair(fs : fs-state) : div fs-state
    match fs.stack
      Nil -> fs
      Cons(head, tail) ->
        val new-tail = Fs-state(tail).repair().stack
        val new-head = match new-tail
          Cons(h, _) -> head.replace-child(h)
          Nil -> head
        Fs-state([new-head].append(new-tail))

  val dir = fs.stack.last().take()
  val updated-dir = dir.insert-children-if-empty(children)
  fs(stack = fs.stack.init().append([updated-dir])).repair()

// --------------------------
//  Files and directories
// --------------------------

type fs-object
  File(name : string, size : int)
  Directory(dir : dir)

struct dir
  name : string
  children : list<fs-object>

fun get-child(dir : dir, child-name : string) : exn dir
  match dir.children.find(fn (child) -> child.name() == child-name)
    Just(Directory(d)) -> d
    _ -> throw("No directory with name " ++ child-name)

fun replace-child(dir : dir, child : dir) : dir
  dir(children = dir.children.map(fn (c)
    if c.name() == child.name then
      Directory(child)
    else
      c
  ))

fun insert-children-if-empty(dir : dir, objs : list<fs-object>) : exn dir
  if dir.children.is-empty() then
    dir(children = objs)
  else
    dir

fun name(fs-obj : fs-object) : string
  match fs-obj
    File(name, _) -> name
    Directory(dir) -> dir.name

fun empty-dir(name : string) : dir
  Dir(name, [])

fun size(dir : dir) : div int
  dir.children.map(fn (obj)
    match obj
      File(_, size) -> size
      Directory(d) -> d.size()
  ).sum()

fun dirs-with-sizes(dir : dir) : div list<(dir, int)>
  val children-dirs = dir.children.flatmap-maybe(fn (obj)
    match obj
      File(_, _) -> Nothing
      Directory(d) -> Just(dirs-with-sizes(d))
  ).concat()

  [(dir, dir.size())].append(children-dirs)

// Must print something like this:
//
// - / (dir)
//  - a (dir)
//    - e (dir)
//      - i (file, size=584)
//    - f (file, size=29116)
//    - g (file, size=2557)
//    - h.lst (file, size=62596)
//  - b.txt (file, size=14848514)
//  - c.dat (file, size=8504156)
//  - d (dir)
//    - j (file, size=4060174)
//    - d.log (file, size=8033020)
//    - d.ext (file, size=5626152)
//    - k (file, size=7214296)
fun println(dir : dir, indent : int = 0) : <div, console> ()
  val indent-str = "  ".repeat(indent)
  println(indent-str ++ "- " ++ dir.name ++ " (dir)")
  dir.children.foreach(fn (child) ->
    match child
      Directory(d) -> println(d, indent + 1)
      File(name, size) -> println(indent-str ++ "  - " ++ name ++ " (file, size=" ++ size.show() ++ ")")
  )
