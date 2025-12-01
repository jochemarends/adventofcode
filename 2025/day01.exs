input =
  File.stream!("./input.txt")
  |> Enum.map(&String.trim/1)
  |> Enum.map(fn
    "L" <> rot -> -String.to_integer(rot)
    "R" <> rot -> String.to_integer(rot)
  end)

input
|> Enum.scan(50, fn rot, pos -> Integer.mod(pos + rot, 100) end)
|> Enum.count(&(&1 == 0))
|> then(&IO.puts("part 1: #{&1}"))

input
|> Enum.reduce({50, 0}, fn rot, {pos, zeros} ->
  abs(div(pos + rot, 100))
  |> Kernel.+(if pos > 0 and pos + rot <= 0, do: 1, else: 0)
  |> then(&{Integer.mod(pos + rot, 100), &1 + zeros})
end)
|> then(fn {_pos, zeros} -> IO.puts("part 2: #{zeros}") end)
