defmodule Day03 do
  def solve(digits, choose: n), do: solve(digits, choose: n, table: :ets.new(:cache, [:set]))
  def solve(_digits, choose: 0, table: _t), do: 0
  def solve([digit | rest] = digits, choose: n, table: t) do
    key = {len, _} = {length(digits), n}
    with [] <- :ets.lookup(t, key) do
        if(len > n, do: solve(rest, choose: n, table: t), else: 0)
        |> max(Integer.pow(10, n - 1) * digit + solve(rest, choose: n - 1, table: t))
        |> tap(&:ets.insert(t, {key, &1}))
    else
      [{^key, result}] -> result
    end
  end
end

for n <- [2, 12] do
  File.read!("./input.txt")
  |> String.split("\n", trim: true)
  |> Enum.reduce(0, &(&2 + Day03.solve(Integer.digits(String.to_integer(&1)), choose: n)))
  |> then(&IO.puts("part #{div(n, 12) + 1}: #{&1}"))
end
