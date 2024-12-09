defmodule Day9 do
  def parse(string) do
    string
    |> String.replace("\n", "")
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
    |> then(&Enum.zip(&1, Enum.intersperse(0..(Enum.count(&1) - 1), nil)))
    |> Enum.flat_map(fn {n, id} -> List.duplicate(id, n) end)
  end

  def part1(input) do
    case Enum.find_index(input, &(&1 == nil)) do
      nil -> 
        input
        |> Enum.with_index()
        |> Enum.reduce(0, fn {id, index}, acc -> acc + id * index end)
      index -> 
        input
        |> then(&List.replace_at(&1, index, List.last(&1)))
        |> then(&List.delete_at(&1, length(&1) - 1))
        |> part1()
    end
  end

  def part2(string) do
    string
  end
end

File.read!("./input.txt")
|> Day9.parse()
|> tap(&IO.puts("part 1: #{Day9.part1(&1)}"))
# |> tap(&IO.puts("part 2: #{Day9.part2(&1)}"))

    # |> Enum.flat_map(fn {
