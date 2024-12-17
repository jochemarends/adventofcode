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

  defp path(map, from, to) do
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

  defp path(map, nodes) do
    nodes
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.flat_map(fn [from, to] -> path(map, from, to) end)
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

  defp find_scores(map, start) do
    map
    |> Enum.map(fn {node, _} -> {node, {(if node == start, do: 0, else: nil), []}} end)
    |> Map.new()
    |> then(&find_scores(map, &1, MapSet.new(Map.keys(map))))
  end

  defp find_scores(map, stats, unvisited) do
    distance = &elem(Map.get(stats, &1), 0)

    candidates = 
      unvisited
      |> MapSet.reject(&(distance.(&1) == nil))

    if MapSet.size(candidates) > 0 do
      case Enum.min_by(candidates, distance) do
        node ->
          edges(map, node)
          |> Enum.filter(&(&1 in unvisited))
          |> Enum.map(&{&1, distance.(node) + weight(map, node, &1)})
          |> Enum.reduce(stats, fn {neighbor, new_distance}, stats ->
            case Map.get(stats, neighbor) do
              {distance, _} when new_distance < distance ->
                {new_distance, [node]}
              {distance, predecessors} when new_distance == distance ->
                {distance, [node | predecessors]}
              tuple -> tuple
            end
            |> then(&Map.put(stats, neighbor, &1))
          end)
          |> then(&find_scores(map, &1, MapSet.delete(unvisited, node)))
      end
    else
      stats
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
    |> then(fn [start, exit] -> Map.get(find_scores(map, start), exit) end)
    |> elem(0)
  end
  
  def backtrace(map, stats, from, visited \\ []) do
    nodes =
      stats
      |> Map.get(from)
      |> elem(1)
      |> Enum.reject(&(&1 in [from | visited]))

    case nodes do
      [] -> path(map, [from | visited])
      nodes -> Enum.flat_map(nodes, &backtrace(map, stats, &1, [from | visited]))
    end
  end

  def part2(map) do
    ["S", "E"]
    |> Enum.map(&Enum.find(map, fn {_, string} -> string == &1 end))
    |> Enum.map(&elem(&1, 0))
    |> then(fn [start, exit] -> 
      find_scores(map, start)
      |> then(&backtrace(map, &1, exit))
      |> Enum.uniq()
      |> Enum.count()
    end)
  end
end

File.read!("./input.txt")
|> Day16.parse()
|> tap(&IO.puts("part 1: #{Day16.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day16.part2(&1)}"))

