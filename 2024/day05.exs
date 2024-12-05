defmodule Day5 do
  def parse(line) when is_bitstring(line) do
    line
    String.length
    |> String.split(~r{[|,]}, trim: true)
    |> Enum.map(&Float.parse/1)
    |> Enum.map(&IO.inspect/1)
    |> Enum.map(&elem(&1, 0)) 
    |> IO.inspect()
  end

  def parse(lines) when is_list(lines) do
    Enum.map(lines, &parse/1)
  end

  def parse(enum) do
    enum
    |> Stream.map(&String.trim/1)
    |> Stream.chunk_by(&(&1 == ""))
    |> Enum.filter(&(&1 != [""]))
    |> IO.inspect()
    |> Enum.map(&parse/1)
  end

  defp is_ordered?([[a, b]], update) do
    Enum.find(b, &(&1==a)) >= Enum.find(a, &(&1==b))
  end

  defp is_ordered?([a, b], update) do
    is_ordered?(a, update) and is_ordered?(b, update)
  end

  def part1({ rules, updates }) do
    updates
    |> Enum.count(&is_ordered?(rules, &1))
  end
end

foo = File.stream!("input.txt")

Day5.parse(foo)
|> IO.puts()
