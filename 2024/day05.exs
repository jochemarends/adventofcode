defmodule Day5 do
  def parse(line) when is_bitstring(line) do
    line
    |> String.split(~r{[|,]}, trim: true)
    |> Enum.map(&Integer.parse/1)
    |> Enum.map(&elem(&1, 0)) 
  end

  def parse(lines) when is_list(lines) do
    Enum.map(lines, &parse/1)
  end

  def parse(enum) do
    enum
    |> Stream.map(&String.trim/1)
    |> Stream.chunk_by(&(&1 == ""))
    |> Enum.filter(&(&1 != [""]))
    |> Enum.map(&parse/1)
    |> List.to_tuple()
  end

  defp is_ordered?([_, _] = rule, update) do
    indices = Enum.map(rule, fn n -> Enum.find_index(update, &(&1==n)) end)
    case indices do
      [nil, _] -> true
      [_, nil] -> true
      [a, b] -> a < b
    end
  end

  defp is_ordered?(rules, update) do
    Enum.all?(rules, &is_ordered?(&1, update))
  end

  defp middle(list), do: Enum.at(list, length(list) |> div(2))

  def sorter(a, b, rules) do
    rule = Enum.find(rules, fn rule -> 
      case rule do
        [^a, ^b] -> true
        [^b, ^a] -> true
        _ -> false
      end
    end)

    case rule do
      [^a, ^b] -> a <= b
      [^b, ^a] -> b <= a
      _ -> true
    end
  end

  def sort(rules, update) do
    Enum.sort(update, &sorter(&1, &2, rules))
  end

  def part1({ rules, updates }) do
    updates
    |> Enum.filter(&is_ordered?(rules, &1))
    |> Enum.map(&middle/1)
    |> Enum.sum()
  end

  def part2({ rules, updates }) do
    updates
    |> Enum.reject(&is_ordered?(rules, &1))
    |> Enum.map(&sort(rules, &1))
    |> Enum.map(&middle/1)
    |> Enum.sum()
  end
end

input = Day5.parse(File.stream!("input.txt"))
IO.puts("part 1: #{Day5.part1(input)}")
IO.puts("part 2: #{Day5.part2(input)}")

