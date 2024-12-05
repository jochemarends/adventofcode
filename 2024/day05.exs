defmodule Day5 do
  defp split(string, :parts), do: String.split(string, "\n\n")
  defp split(string, :lines), do: String.split(string, "\n", trim: true)
  defp split(string, :elems), do: String.split(string, ["|", ","], trim: true) 

  def process(input, stage), do: input |> split(stage) |> Enum.map(&parse(&1, stage))
  def parse(input), do: process(input, :parts)
  def parse(input, :parts), do: process(input, :lines)
  def parse(input, :lines), do: process(input, :elems)
  def parse(input, :elems), do: String.to_integer(input)

  defp is_sorted?(update, [_, _] = rule) do
    case Enum.map(rule, fn n -> Enum.find_index(update, &(&1 == n)) end) do
      [nil, _] -> true
      [_, nil] -> true
      [a, b] -> a < b
    end
  end

  defp is_sorted?(update, rules), do: Enum.all?(rules, &is_sorted?(update, &1))

  defp sort(update, rules), do: Enum.sort(update, &is_sorted?([&1, &2], rules))

  defp middle(list), do: Enum.at(list, length(list) |> div(2))

  def solve([rules, updates], part) do
    case part do
      :part1 -> 
        updates
        |> Enum.filter(&is_sorted?(&1, rules)) 
      :part2 -> 
        updates
        |> Enum.reject(&is_sorted?(&1, rules))
        |> Enum.map(&sort(&1, rules))
    end
    |> Enum.map(&middle/1) 
    |> Enum.sum()
  end
end

input = Day5.parse(File.read("input.txt") |> elem(1))
IO.puts("part 1: #{Day5.solve(input, :part1)}")
IO.puts("part 2: #{Day5.solve(input, :part2)}")
