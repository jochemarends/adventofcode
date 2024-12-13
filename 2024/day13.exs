defmodule Day13 do
  def parse(input) do
    Regex.scan(~r/[0-9]+/, input)
    |> List.flatten()
    |> Enum.map(&String.to_integer/1)
    |> Enum.chunk_every(2)
    |> Enum.chunk_every(3)
  end

  defguardp is_mat(mat) when is_list(hd(mat))

  defp dot(a, b), do: Enum.zip_with(a, b, fn a, b -> a * b end) |> Enum.sum()

  defp det([[a, b], [c, d]]), do: a * d - b * c

  defp adj([[a, b], [c, d]]), do: [[d, -b], [-c, a]]

  defp mul(a, b) when is_number(a) and is_mat(b) do
    Enum.map(b, fn row -> Enum.map(row, &(a * &1)) end)
  end

  defp mul(a, b) when is_mat(a) and is_mat(b) do
    cols = Enum.zip_with(b, &Function.identity/1)
    Enum.map(a, &mul(&1, cols))
  end

  defp mul(a, b) when is_list(a) and is_mat(b), do: Enum.map(b, &dot(a, &1))

  def inv(mat) when is_mat(mat), do: mul(1 / det(mat), adj(mat))

  def solve([[x1, y1], [x2, y2], prize]) do
    mat = [[x1, x2], [y1, y2]] 
    presses = mul(prize, inv(mat))
    is_whole? = &(Float.round(&1, 3) == round(&1))
    if Enum.all?(presses, is_whole?), do: dot([3, 1], presses), else: 0
  end

  def part1(input), do: input |> Enum.map(&solve/1) |> Enum.sum()

  @offset 10000000000000
  def part2(input) do
    input
    |> Enum.map(fn [a, b, c] -> [a, b, Enum.map(c, &(&1 + @offset))] end)
    |> part1()
  end
end

File.read!("./input.txt")
|> Day13.parse()
|> tap(&IO.puts("part 1: #{Day13.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day13.part2(&1)}"))

