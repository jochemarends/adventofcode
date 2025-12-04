rows = File.read!("./input.txt") |> String.split("\n", trim: true)

for(
  x <- 0..(length(rows) - 1),
  y <- 0..(String.length(hd(rows)) - 1),
  String.at(Enum.at(rows, y), x) == "@",
  into: MapSet.new(),
  do: {x, y}
)
|> Stream.iterate(fn rolls ->
  MapSet.reject(rolls, fn {x, y} ->
    for(dx <- -1..1, dy <- -1..1, not (dx == 0 and dy == 0), do: {x + dx, y + dy})
    |> Enum.count(&MapSet.member?(rolls, &1))
    |> Kernel.<(4)
  end)
end)
|> Stream.map(&Enum.count/1)
|> Stream.chunk_every(2, 1)
|> Stream.map(fn [a, b] -> a - b end)
|> tap(fn s -> IO.puts("part 1: #{Enum.at(s, 0)}") end)
|> tap(fn s -> IO.puts("part 2: #{Enum.sum(Stream.take_while(s, &(&1 != 0)))}") end)
