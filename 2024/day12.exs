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

  defp is_interior?(garden, pos) do
    adjacents(pos)
    |> Enum.map(&Map.get(garden, &1))
    |> Enum.all?(&(&1 == Map.get(garden, pos)))
  end

  defp group_regions(garden) do
    Map.keys(garden)
    |> Enum.reduce({[], %{}}, fn pos, {groups, visited} -> 

    end)
    # find adjacent that's already in a list
    # add it else add something else
  end

  def part1(_), do: "todo"
  def part2(_), do: "todo"
end

File.read!("./input.txt")
|> Day12.parse()
|> tap(&IO.puts("part 1: #{Day12.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day12.part2(&1)}"))
