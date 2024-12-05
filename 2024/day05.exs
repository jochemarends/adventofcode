defmodule Day5 do
  defp split(string, :parts), do: String.split(string, "\n\n")
  defp split(string, :lines), do: String.split(string, "\n", trim: true)
  defp split(string, :elems), do: String.split(string, ["|", ","], trim: true) 

  def parse(string), do: split(string, :parts) |> Enum.map(&parse(&1, :lines))
  def parse(part, :lines), do: split(part, :lines) |> Enum.map(&parse(&1, :elems))
  def parse(line, :elems), do: split(line, :elems) |> Enum.map(&String.to_integer/1)

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

  def part1([rules, updates]) do
    updates
    |> Enum.filter(&is_sorted?(&1, rules))
    |> sum_midpoints()
  end

  defp sort(update, rules), do: Enum.sort(update, &is_sorted?([&1, &2], rules))

  def part2([rules, updates]) do
    updates
    |> Enum.reject(&is_sorted?(&1, rules))
    |> Enum.map(&sort(&1, rules))
    |> sum_midpoints()
  end
end

input = Day5.parse(File.read("input.txt") |> elem(1))
IO.puts("part 1: #{Day5.part1(input)}")
IO.puts("part 2: #{Day5.part2(input)}")
