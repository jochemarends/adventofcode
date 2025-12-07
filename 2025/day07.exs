[top | rows] = for line <- File.stream!("./input.txt"), do: String.graphemes(line)

start = Enum.find_index(top, &(&1 == "S"))

for part <- 1..2 do
  Stream.transform(rows, %{start => 1}, fn row, beams ->
    Enum.reduce(beams, {[0], %{}}, fn {x, count}, {[n], beams} ->
      count = if(part == 1, do: 1, else: count)

      {
        if(Enum.at(row, x) == "^", do: n + count, else: n) |> List.wrap(),
        if(Enum.at(row, x) == "^", do: [x - 1, x + 1], else: [x])
        |> Enum.reduce(beams, fn x, acc -> Map.update(acc, x, count, &(&1 + count)) end)
      }
    end)
  end)
  |> then(&IO.puts("part #{part}: #{Enum.sum(&1) + part - 1}"))
end
