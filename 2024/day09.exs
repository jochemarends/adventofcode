defmodule Day9 do
  def parse(string) do
    string
    |> String.replace("\n", "")
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
    |> then(&Enum.zip(&1, Enum.intersperse(0..(Enum.count(&1) - 1), nil)))
  end

  def part1(input), do: part1(input, 0, 0)
  def part1([], _, acc), do: acc
  def part1([head | tail], index, acc) do
    case head do
      {n, id} when id != nil ->
        index..(index + n - 1)//1
        |> Enum.reduce(acc, fn index, acc -> acc + index * id end)
        |> then(&part1(tail, index + n, &1))
      {n1, nil} when n1 != 0 and length(tail) > 0 -> 
        case List.pop_at(tail, -1) do
          {{n2, id}, rest} when n2 != 0 ->
            a = {min(n1, n2), id}
            b = {n1 - min(n1, n2), nil}
            c = {n2 - min(n1, n2), id}
            part1([a, b | rest] ++ [c], index, acc)
          {_, rest} -> part1([head | rest], index, acc)
        end
      _ -> part1(tail, index, acc)
    end
  end

  def part2(input), do: part2(input, 0, 0)
  def part2([], _, acc), do: acc
  def part2([head | tail], index, acc) do
    case head do
      {free_space, nil} when free_space != 0 ->
        {file, file_index} =
          tail
          |> Enum.with_index()
          |> Enum.reverse()
          |> Enum.find({nil, nil}, fn {{file_size, id}, _} -> 
            id != nil and file_size <= free_space 
          end)

        case file do
          {file_size, _} ->
            free = {free_space - file_size, nil}
            tail = List.replace_at(tail, file_index, {file_size, nil})
            part2([file, free | tail], index, acc)
          _ -> part2(tail, index + free_space, acc)
        end
      {file_size, id} when id != nil ->
        index..(index + file_size - 1)//1
        |> Enum.reduce(acc, fn index, acc -> acc + index * id end)
        |> then(&part2(tail, index + file_size, &1))
      _ -> part2(tail, index, acc)
    end
  end
end

File.read!("./input.txt")
|> Day9.parse()
|> tap(&IO.puts("part 1: #{Day9.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day9.part2(&1)}"))
