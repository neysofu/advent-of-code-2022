module day12

import std/os/readline

pub fun main()
  val lines = lines-until-empty()
  val heightmap = parse-heightmap(lines)
  val h-len = heightmap.heights.head().take().length()

  val graph = from-heightmap(heightmap.heights)
  println("The graph has " ++ graph.nodes.length().show() ++ " nodes")

  val nodes-distance = dijkstra(graph, heightmap.end.int(h-len), heightmap.start.int(h-len))
  val distance-to-start = nodes-distance.find(fn (d)
    d.fst == heightmap.start.int(h-len)
  ).take().snd

  println("Part 1 distance: " ++ distance-to-start.show())

  val best-distance = nodes-distance.filter(fn (d)
    // We only care about 'a' squares
    d.thd == 0
  // We select the best distance
  ).map(fn (d) -> d.snd).minimum()
  println("Part 2 distance: " ++ best-distance.show())

fun take(maybe : maybe<a>) : exn a
  match maybe
    Just(a) -> a
    Nothing -> throw("Unexpected Nothing")

fun or(maybe : maybe<a>, default : a) : a
  match maybe
    Just(a) -> a
    Nothing -> default

fun replace(list : list<a>, index : int, value : a) : list<a>
  val (before, rest) = list.split(index)
  before.append([value]).append(rest.drop(1))

fun bubblesort(xs : list<a>, gt : (a, a) -> bool, eq : (a, a) -> bool) : div list<a>
  val sorted = match xs
    [] -> []
    [x] -> [x]
    Cons(a, Cons(b, rest)) ->
      if a.gt(b) then
        [b].append(Cons(a, rest).bubblesort(gt, eq))
      else
        [a].append(Cons(b, rest).bubblesort(gt, eq))

  if sorted.zipwith(xs, eq).all(fn (x) -> x) then
    sorted
  else
    sorted.bubblesort(gt, eq)

// --------------------------
//  Parsing
// --------------------------

fun parse-heightmap(lines : list<string>) : <console, exn, div> parsed-heightmap
  var heights := []
  var start := Coord(0, 0)
  var end := Coord(0, 0)
  var y := 0
  while {y < lines.length()}
    val pixels = lines[y].take().list().map-indexed(fn (x, c) -> match c
      'S' ->
        start := Coord(x, y)
        0
      'E' ->
        end := Coord(x, y)
        'z'.int() - 'a'.int()
      h -> h.int() - 'a'.int()
    )
    heights := heights.append([pixels])
    y := y + 1
  
  Parsed-heightmap(heights, start, end)

fun lines-until-empty() : <console, div, exn> list<string>
  var line := readline()
  var lines := []
  while {line.is-notempty()}
    lines := lines.append([line])
    line := readline()
  lines

// --------------------------
//  Heightmap stuff
// --------------------------

struct coord
  x : int
  y : int

struct parsed-heightmap
  heights : list<list<int>>
  start: coord
  end: coord

fun int(c : coord, h-len : int) : int
  c.x + c.y * h-len

fun from-heightmap(heights : list<list<int>>) : exn graph<int>
  val h-len = heights.head().take().length()
  val h-range = list(0, h-len - 1)
  val v-range = list(0, heights.length() - 1)

  val nodes-only = v-range.foldl(empty(), fn (acc, y)
    h-range.foldl(acc, fn (graph, x)
      graph.add-node(heights[y].take()[x].take())
    )
  )

  val with-horizontal-neighbors = v-range.foldl(nodes-only, fn (acc, y)
    h-range.foldl(acc, fn (graph, x)
      graph.add-edges(Coord(x, y), Coord(x - 1, y), h-len)
    )
  )

  val graph = v-range.foldl(with-horizontal-neighbors, fn (acc, y)
    h-range.foldl(acc, fn (graph, x)
      graph.add-edges(Coord(x, y), Coord(x, y - 1), h-len)
    )
  )

  graph

fun add-edges(graph : graph<int>, n1 : coord, n2 : coord, h-len : int) : exn graph<int>
  val i1 = n1.int(h-len)
  val i2 = n2.int(h-len)
  val h1 = graph.get(i1)
  val h2 = graph.get(i2)

  match (h1, h2)
    (Just(h1), Just(h2)) ->
      if h1 <= h2 + 1 && h2 <= h1 + 1 then
        graph.add-edge(i1, i2).add-edge(i2, i1)
      else if h1 <= h2 + 1 then
        graph.add-edge(i1, i2)
      else if h2 <= h1 + 1 then
        graph.add-edge(i2, i1)
      else
        graph
    _ -> graph

// --------------------------
//  Graph stuff
// --------------------------

struct graph<a>
  nodes : list<node<a>>

struct node<a>
  adj-list : list<int>
  info : a


fun empty() : graph<a>
  Graph([])

fun println(g : graph<int>) : console ()
  g.nodes.foreach(fn (n)
    println(n.info.show() ++ " -> " ++ n.adj-list.show())
  )

fun add-node(graph : graph<a>, info : a) : graph<a>
  val node = Node([], info)
  graph(nodes = graph.nodes.append([node]))

fun get(graph : graph<a>, index : int) : exn maybe<a>
  graph.nodes[index].map(info)

fun add-edge(graph : graph<a>, from : int, to : int) : exn graph<a>
  val from-node = graph.nodes[from].take()
  val to-node = graph.nodes[to].take()

  if from-node.adj-list.any(fn (i) -> i == to) then
    graph
  else
    val new-from-node = from-node(adj-list = from-node.adj-list.append([to]))
    graph(nodes = graph.nodes.replace(from, new-from-node))

fun neighbors(graph : graph<a>, node : int) : exn list<int>
  graph.nodes[node].take().adj-list

// Returns a list of node ID, distance, and node information.
fun dijkstra(graph : graph<a>, start : int, stop-at : int) : <console, div, exn> list<(int, int, a)>
  var unvisited := list(0, graph.nodes.length() - 1)
    .map(fn (i) -> (i, 100000000, graph.get(i).take()))
  var visited := []
  var current := start
  var current-distance := 0
  var continue := True

  while {continue}
    println(unvisited.length().show())
    graph.neighbors(current).foreach(fn (neighbor-id)
      match unvisited.index-of(fn (x) -> x.fst == neighbor-id)
        -1 -> ()
        idx ->
          val distance = current-distance + 1
          val old-distance = unvisited[idx].take().snd
          val height = unvisited[idx].take().thd
          unvisited := unvisited.replace(idx, (neighbor-id, distance.min(old-distance), height))
    )

    val height = graph.get(current).take()
    visited := visited.append([(current, current-distance, height)])
    unvisited := unvisited.filter(fn (x) -> x.fst != current)

    if unvisited.length() == 0 then
      continue := False
    else
      val best = unvisited.foldl1(fn (a, b)
        if a.snd > b.snd then
          b
        else
          a
      )
      current := best.fst
      current-distance := best.snd

  visited
