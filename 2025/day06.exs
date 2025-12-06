File.stream!("./input.txt")
|> Enum.map(&String.split(&1, ["\n", " "], trim: true))
|> Enum.zip_with(&Enum.reverse/1)
|> Enum.reduce(0, fn [op | seq], acc ->
  seq = Enum.map(seq, &String.to_integer/1)
  acc + apply(&Enum.reduce/3, [seq | if(op == "+", do: [0, &+/2], else: [1, &*/2])])
end)
|> then(&IO.puts("part 1: #{&1}"))

File.read!("./input.txt")
|> String.split("\n", trim: true)
|> Enum.map(&String.graphemes/1)
|> then(fn rows ->
  {rows, [ops]} = Enum.split(rows, Enum.count(rows) - 1)

  chunks =
    rows
    |> Enum.zip_with(& &1)
    |> Enum.chunk_while([], fn row, acc ->
      if Enum.all?(row, & &1 == " ") do
        {:cont, acc, []}
      else
        {:cont, [String.to_integer(String.trim(Enum.join(row))) | acc]}
      end
    end, &{:cont, &1, []})

  ops
  |> Enum.reject(& &1 == " ")
  |> Enum.map(fn op -> Map.get(%{"+" => [0, &+/2], "*" => [1, &*/2]}, op) end)
  |> Enum.zip_with(chunks, fn a, b -> apply(&Enum.reduce/3, [b | a]) end)
end)
|> then(&IO.puts("part 2: #{Enum.sum(&1)}"))
