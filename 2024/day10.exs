defmodule Day10 do
  def parse(string) do 
    string
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, i} ->
      line
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
      |> Enum.with_index()
      |> Enum.map(fn {height, j} -> {{i, j}, height} end)
    end)
    |> Map.new()
  end

  defp trailheads(map) do 
    map
    |> Map.filter(fn {_, height} -> height == 0 end)
    |> Map.keys()
  end

  defp neighbours({row, col}) do
    [{row - 1, col}, {row + 1, col}, {row, col - 1}, {row, col + 1}]
  end

  defp find_summits(map, index, visited \\ MapSet.new()) do
    case Map.get(map, index) do
      9 -> [index]
      height ->
        index
        |> neighbours()
        |> Enum.reject(&(&1 in visited))
        |> Enum.filter(&(Map.get(map, &1) == height + 1))
        |> Enum.flat_map(fn neighbour -> find_summits(map, neighbour, MapSet.put(visited, index)) end)
    end
  end

  def part1(input) do
    input
    |> trailheads()
    |> Enum.map(fn trailhead -> 
      find_summits(input, trailhead)
      |> Enum.uniq()
      |> Enum.count()
    end)
    |> Enum.sum()
  end

  def part2(input) do
    input
    |> trailheads()
    |> Enum.map(fn trailhead -> 
      find_summits(input, trailhead)
      |> Enum.count()
    end)
    |> Enum.sum()
  end
end

File.read!("./input.txt")
|> Day10.parse()
|> tap(&IO.puts("part 1: #{Day10.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day10.part2(&1)}"))

