input =
  File.read!("./input.txt")
  |> String.split(["\n", ","], trim: true)
  |> Enum.map(fn s -> String.split(s, "-") |> Enum.map(&String.to_integer/1) end)
  |> Enum.map(fn [first, last] -> first..last end)

for part <- [1, 2] do
  input
  |> Enum.flat_map(fn range ->
    range
    |> Enum.filter(fn id ->
      digits = Integer.digits(id)

      length(digits) > 1 and
        if(part == 1, do: rem(length(digits), 2) == 0, else: true) and
        Enum.any?(
          if(part == 1, do: div(length(digits), 2), else: 1)..div(length(digits), 2),
          fn n ->
            rem(length(digits), n) == 0 and
              digits
              |> Stream.take(n)
              |> Stream.cycle()
              |> Enum.take(length(digits))
              |> Kernel.==(digits)
          end
        )
    end)
  end)
  |> Enum.sum()
  |> then(&IO.puts("part #{part}: #{&1}"))
end
