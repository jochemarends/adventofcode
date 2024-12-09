defmodule Day9 do
  require Integer

  def parse(string) do
    string
    |> String.replace("\n", "")
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
    |> Enum.with_index()
    |> Enum.flat_map(fn {size, index} ->
      case size do
        0 -> []
        _ when Integer.is_even(index) -> List.duplicate(div(index, 2), size)
        _ -> List.duplicate(nil, size)
      end
    end)
  end

  def part1(list) do
    index = Enum.find_index(list, fn {id, _} -> id != nil end)
    case  do
      nil -> enum
      id = elem -> 
        last = List.last(list)
        list
        |> List.replace_at(index, {elem(last, 0, index)
        |> List.replace_at(-1, index)
    end
  end

  def part2(string) do
    string
  end
end

File.read!("./input.txt")
|> Day9.parse()
|> tap(&IO.puts("part 1: #{Day9.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day9.part2(&1)}"))
