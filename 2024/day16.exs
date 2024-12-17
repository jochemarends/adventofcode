defmodule Day16 do
  defp det([[a, b], [c, d]]), do: a * d - b * c
  defp sub({x, y}, {z, w}), do: {x - z, y - w}
  defp add({x, y}, {z, w}), do: {x + z, y + w}

  defp colinear?({x1, y1}, {x2, y2}, {x3, y3}) do
    mat = [[x2 - x1, y2 - y1], [x3 - x1, y3 - y1]]
    det(mat) == 0
  end

  defp neighbors(map, {x, y}) do
    [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
    |> Enum.filter(&Map.has_key?(map, &1))
  end

  defp weight(map, from, to) do
    {dx, dy} = sub(to, from)

    turned = 
      (Map.get(map, from) == "S" and sub(from, to) != {0, 1})
      |> then(&(&1 or (dx != 0 and dy != 0)))

    cond do
      turned -> abs(dx) + abs(dy) + 1000
      true -> abs(dx) + abs(dy)
    end
  end

  defp weight(map, nodes) do
    nodes
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.reduce(0, fn [from, to], acc -> acc + weight(map, from, to) end)
  end

  defp find_path(map, from, to) do
    {dx, dy} = sub(to, from)
    if dx != 0 and dy != 0 do
      [{dx, 0}, {0, dy}]
      |> Enum.map(&add(&1, from))
      |> Enum.find(&Map.has_key?(map, &1))
      |> then(&[from, &1, to])
    else
      (for x <- 0..dx, y <- 0..dy, do: add(from, {x, y}))
    end
  end

  defp find_path(map, nodes) do
    nodes
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.flat_map(fn [from, to] -> find_path(map, from, to) end)
  end

  defp edges(map, node) do
    neighbors(map, node)
    |> Enum.flat_map(fn node ->
      neighbors(map, node)
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.all?(fn [a, b] -> colinear?(node, a, b) end)
      |> then(&(if &1 or Map.get(map, node) == "E", do: [node], else: edges(map, node)))
    end)
  end

  defp find_score(map, start, exit) do
    distances = 
      map
      |> Enum.map(fn {node, _} -> {node, (if node == start, do: 0, else: nil)} end)
      |> Map.new()

    find_score(map, exit,distances, MapSet.new(Map.keys(map)))
  end

  defp find_score(map, exit, distances, unvisited) do
    candidates = 
      unvisited
      |> MapSet.reject(&Map.get(distances, &1) == nil)

    if MapSet.size(candidates) > 0 do
      case Enum.min_by(candidates,&Map.get(distances, &1)) do
        node when node == exit -> Map.get(distances, exit)
        node ->
          edges(map, node)
          |> Enum.filter(&(&1 in unvisited))
          |> Enum.map(&{&1, Map.get(distances, node) + weight(map, node, &1)})
          |> Enum.reduce(distances, fn {node, distance}, distances ->
            Map.put(distances, node, min(distance, Map.get(distances, node)))
          end)
          |> then(&find_score(map, exit, &1, MapSet.delete(unvisited, node)))
      end
    else
      distances
    end
  end

  defp fooo(map, start, exit) do
    map
    |> Enum.map(fn {node, _} -> {node, {(if node == start, do: 0, else: nil), []}} end)
    |> Map.new()
    |> then(&fooo(map, exit, &1, MapSet.new(Map.keys(map))))
  end

  defp fooo(map, exit, stats, unvisited) do
    distance = &elem(Map.get(stats, &1), 0)

    candidates = 
      unvisited
      |> MapSet.reject(&(distance.(&1) == nil))

    if MapSet.size(candidates) > 0 do
      case Enum.min_by(candidates, distance) do
        node when node == exit -> Map.get(stats, exit)
        node ->
          edges(map, node)
          |> Enum.filter(&(&1 in unvisited))
          |> Enum.map(&{&1, distance.(node) + weight(map, node, &1)})
          |> Enum.reduce(stats, fn {node, new_distance}, stats ->
            case Map.get(stats, node) do
              {distance, predecessors} when new_distance < distance ->
                {new_distance, [node]}
              {distance, predecessors} when new_distance == distance ->
                {distance, [node | predecessors]}
              tuple -> tuple
            end
            |> then(&Map.put(stats, node, &1))
          end)
          |> then(&fooo(map, exit, &1, MapSet.delete(unvisited, node)))
      end
    end
  end

  defp find_best_paths(map, from, to) do
    score = find_score(map, from, to)
    find_best_paths(score, to, map, [[from]], [from], [])
  end

  defp find_best_paths(_, _, map, [], _, found) do 
    Enum.map(found, &find_path(map, &1))
  end
  # defp find_best_paths(_, _, _, [], _, found), do: found

  defp find_best_paths(max_weight, to, map, [path | queue], visited, found) do
    node = Enum.at(path, -1)
    # w = weight(map, find_path(map, path))
    m = Map.filter(map, fn {pos, _} -> pos in find_path(map, path) end)
    w = find_score(map, Enum.at(path, 0), node)

    cond do
      w > max_weight ->
        IO.inspect(w)
        find_best_paths(max_weight, to, map, queue, visited, found)
      node == to ->
        find_best_paths(max_weight, to, map, queue, visited, [path | found])
      true ->
        node
        |> then(&edges(map, &1))
        |> Enum.filter(&(&1 not in visited))
        |> Enum.reduce(queue, &[path ++ [&1] | &2])
        |> then(&find_best_paths(max_weight, to, map, &1, [node | visited], found))
    end
  end

  def parse(input) do
    grid =
      input
      |> String.split("\n", trim: true)
      |> Enum.map(&String.graphemes/1)

    {rows, cols} = {length(grid), length(hd(grid))}
    (for row <- 0..(rows - 1), col <- 0..(cols - 1), do: {row, col})
    |> Enum.zip(List.flatten(grid))
    |> Enum.reject(fn {_, string} -> string == "#" end)
    |> Map.new()
  end

  def part1(map) do
    ["S", "E"]
    |> Enum.map(&Enum.find(map, fn {_, string} -> string == &1 end))
    |> Enum.map(&elem(&1, 0))
    |> then(fn [start, exit] -> find_score(map, start, exit) end)
  end

  def part2(map) do
    ["S", "E"]
    |> Enum.map(&Enum.find(map, fn {_, string} -> string == &1 end))
    |> Enum.map(&elem(&1, 0))
    |> then(fn [start, exit] -> fooo(map, start, exit) end)
    # |> then(fn [start, exit] -> find_best_paths(map, start, exit) end)
    # |> Enum.map(&Enum.uniq/1)
    # |> List.flatten()
    # |> Enum.uniq()
    # |> Enum.count()
  end
end

File.read!("./input.txt")
|> Day16.parse()
|> Day16.part2()
|> IO.inspect()

