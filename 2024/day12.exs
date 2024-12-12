defmodule Day12 do
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, i} ->
      line
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
      |> Enum.with_index()
      |> Enum.map(fn {type, j} -> {{i, j}, type} end)
    end)
    |> Map.new()
  end

  defp adjacents({x, y}), do: [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]

  defp is_interior?(garden, plot) do
    plot
    |> adjacents()
    |> Enum.map(&Map.get(garden, &1))
    |> Enum.all?(&(&1 == Map.get(garden, pos)))
  end

  defp is_boundary?(garden, plot), do: not is_interior?(garden, plot)

  defp borders_any?(region, plot), do: plot |> adjacents() |> Enum.any?(&(&1 in region))

  defp group_type(garden) Enum.group_by(garden, fn plot -> Map.get(garden, plot) end)

  defp walk_plots(plots, plot, visited \\ MapSet.new()) do
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

  defp group_bordering([plot | plots]) do
    # |> Enum.reduce([plot], fn plot, region, group_by(borders_any?
  end

  def part1(_), do: "todo"
  def part2(_), do: "todo"
end

File.read!("./input.txt")
|> Day12.parse()
|> tap(&IO.puts("part 1: #{Day12.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day12.part2(&1)}"))
