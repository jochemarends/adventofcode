defmodule Day7 do
  def operators(:part1), do: ["+", "*"]
  def operators(:part2), do: ["||" | operators(:part1)]

  def parse(string) do
    string
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      line
      |> String.split([":", " "], trim: true)
      |> Enum.map(&String.to_integer/1)
    end)
  end

  defp eval("+", [a, b]), do: a + b
  defp eval("*", [a, b]), do: a * b
  defp eval("||", [a, b]), do: String.to_integer("#{a}#{b}")

  defp eval([op | operators], [a, b | operands]) do 
    inner = eval(op, [a, b])
    eval(operators, [inner | operands])
  end

  defp power(enum, n) do 
    enum
    |> List.duplicate(n)
    |> product()
  end

  defp product([enum]), do: enum

  defp product([head | tail]) do
    for a <- head, b <- product(tail), do: [a | b]
  end

  defp is_true?([result | operands], part) do
    operators = power(operators(part), length(operands) - 1)
    Enum.any?(operators, &(eval(&1, operands) == result))
  end

  defp process(input, part) do
    input
    |> Enum.filter(&is_true?(&1, part))
    |> Enum.map(&hd/1)
    |> Enum.sum()
  end

  def part1(input), do: process(input, :part1)
  def part2(input), do: process(input, :part2)
end

Day7.parse(File.read!("./input.txt"))
|> tap(&IO.puts("part 1: #{Day7.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day7.part2(&1)}"))

