defmodule Day5 do
  def parse(line) when is_bitstring(line) do
    line
    |> String.split(~r/[|,]/, trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  def parse(lines) when is_list(lines), do: Enum.map(lines, &parse/1)

  def parse(enum) do
    enum
    |> Stream.map(&String.trim/1)
    |> Stream.chunk_by(&(&1 == ""))
    |> Enum.filter(&(&1 != [""]))
    |> Enum.map(&parse/1)
    |> List.to_tuple()
  end

  defp is_sorted?(update, [_, _] = rule) do
    case Enum.map(rule, fn n -> Enum.find_index(update, &(&1==n)) end) do
      [nil, _] -> true
      [_, nil] -> true
      [a, b] -> a < b
    end
  end

  defp is_sorted?(update, rules), do: Enum.all?(rules, &is_sorted?(update, &1))

  defp middle(list), do: Enum.at(list, length(list) |> div(2))

  defp sum_midpoints(updates), do: Enum.map(updates, &middle/1) |> Enum.sum()

  def part1({ rules, updates }) do
    updates
    |> Enum.filter(&is_sorted?(&1, rules))
    |> sum_midpoints()
  end

  defp sort(update, rules), do: Enum.sort(update, &is_sorted?([&1, &2], rules))

  def part2({ rules, updates }) do
    updates
    |> Enum.reject(&is_sorted?(&1, rules))
    |> Enum.map(&sort(&1, rules))
    |> sum_midpoints()
  end
end

input = Day5.parse(File.stream!("input.txt"))
IO.puts("part 1: #{Day5.part1(input)}\npart 2: #{Day5.part2(input)}")
