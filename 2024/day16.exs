defmodule Day16 do
  defp det([[a, b], [c, d]]), do: a * d - b * c
  defp sub({x, y}, {z, w}), do: {x - z, y - w}

  defp colinear?({x1, y1}, {x2, y2}, {x3, y3}) do
    mat = [[x2 - x1, y2 - y1], [x3 - x1, y3 - y1]]
    det(mat) == 0
  end

  defp neighbors(map, {x, y}) do
    [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
    |> Enum.filter(&Map.has_key?(map, &1))
  end

  def weight(from, to) do
    {dx, dy} = sub(to, from)

    if dx != 0 and dy != 0 do
      abs(dx) + abs(dy) + 90
    else
      abs(dx) + abs(dy)
    end
  end

  defp edges(map, node) do
    neighbors(map, node)
    |> Enum.flat_map(fn node ->
      neighbors(map, node)
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.all?(fn [a, b] -> colinear?(node, a, b) end)
      |> then(&(if &1, do: [node], else: edges(map, node)))
    end)
    |> Enum.reject(&(&1 == node))
  end

  def foo(map, start) do
    distances = 
      map
      |> Enum.map(fn {node, _} -> {node, (if node == start, do: 0, else: nil)} end)
      |> Map.new()
    foo(map, distances, MapSet.new(Map.keys(map)))
  end

  def foo(map, distances, %MapSet{}), do: distances

  def foo(map, distances, unvisited) do
    node = Enum.min_by(unvisited, &Map.get(distances, &1))

    edges(map, node)
    |> Enum.filter(&(&1 in unvisited))
    |> Enum.map(&{&1, Map.get(distances, node) || 0 + Map.get(distances, &1)})
    |> Enum.reduce(distances, fn {node, distance}, distances ->
      Map.put(distances, node, min(distance, Map.get(distances, node)))
    end)
    |> then(&foo(map, &1, MapSet.delete(unvisited, node)))
     # |> Enum.reduce(distances, &Map.put(&2, min(&1, Map.get(&2, &1))))
  end

  # defp start(map), do: Enum.find(map, fn {_, string} -> string == "S" end)
  # defp end(map), do: Enum.find(map, fn {_, string} -> string == "E" end)

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
end

File.read!("./input.txt")
|> Day16.parse()
|> Day16.foo({1, 1})
|> IO.inspect()

