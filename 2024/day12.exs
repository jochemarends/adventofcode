defmodule Day12 do
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, i} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map(fn {type, j} -> {{i, j}, type} end)
    end)
    |> Map.new()
  end

  @normals [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]
  defp adjacents({x, y}), do: @normals |> Enum.map(fn {dx, dy} -> {x + dx, y + dy} end)

  defp group_by_type(garden), do: Enum.group_by(Map.keys(garden), fn plot -> Map.get(garden, plot) end)

  defp traverse(plots, start, visited \\ []) do
    start
    |> adjacents()
    |> Enum.filter(&(&1 not in visited and &1 in plots))
    |> Enum.reduce([start | visited], &traverse(plots, &1, &2))
    |> Enum.uniq()
  end

  defp group_by_border([]), do: []
  defp group_by_border([start | rest] = plots) do
    visited = traverse(rest, start)
    [visited | group_by_border(plots -- visited)]
  end

  defp group_by_region(garden) do
    garden
    |> group_by_type()
    |> Enum.flat_map(fn {type, plots} ->
      group_by_border(plots)
      |> Enum.map(&({type, &1}))
    end)
  end

  defp normals(garden, {type, plots}) do
    plots
    |> Enum.flat_map(&(Enum.zip(adjacents(&1), @normals)))
    |> Enum.reject(fn {plot, _} -> Map.get(garden, plot) == type end)
  end

  defp perimeter(garden, region), do: normals(garden, region) |> Enum.count()

  defp sides(garden, region) do
    garden
    |> normals(region)
    |> Enum.group_by(fn {_, normal} -> normal end)
    |> Enum.map(fn {_, normals} -> Enum.map(normals, &elem(&1, 0)) end)
    |> Enum.flat_map(&group_by_border/1)
    |> Enum.count()
  end

  def solve(garden, fun) do
    garden
    |> group_by_region()
    |> Enum.map(fn {_, plots} = region -> Enum.count(plots) * fun.(garden, region) end)
    |> Enum.sum()
  end

  def part1(garden), do: solve(garden, &perimeter/2)
  def part2(garden), do: solve(garden, &sides/2)
end

File.read!("./input.txt")
|> Day12.parse()
|> tap(&IO.puts("part 1: #{Day12.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day12.part2(&1)}"))
