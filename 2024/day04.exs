defmodule Day4 do
  defp diagonal(mat, :left) do
    0..(length(mat) - 1)
    |> Enum.map(fn i -> Enum.at(Enum.at(mat, i), i) end)
  end

  defp diagonal(mat, :right), do: Enum.map(mat, &Enum.reverse/1) |> diagonal(:left)

  def diagonals(mat), do: [:left, :right] |> Enum.map(&diagonal(mat, &1))

  def windows(mat, n) do
    for row <- 0..(length(mat) - n),
         col <- 0..(length(List.first(mat)) - n) do
      mat
      |> Enum.slice(row, n)
      |> Enum.map(&Enum.slice(&1, col, n))
    end
  end
end

defmodule Day4.Part1 do
  import Day4
  @chunk_size String.length("XMAX")

  defp is_match?(["X", "M", "A", "S"]), do: true
  defp is_match?(["S", "A", "M", "X"]), do: true
  defp is_match?(_), do: false

  defp count_matches(mat, :rows) do
    mat
    |> Enum.map(fn row ->
      row
      |> Enum.chunk_every(@chunk_size, 1,:discard)
      |> Enum.count(&is_match?/1)
    end)
    |> Enum.sum()
  end

  defp count_matches(mat, :cols) do
    mat 
    |> List.zip
    |> Enum.map(&Tuple.to_list/1)
    |> count_matches(:rows)
  end

  defp count_matches(mat, :diag) do
    windows(mat, @chunk_size)
    |> Enum.map(&diagonals/1)
    |> Enum.map(fn win -> Enum.count(win, &is_match?/1) end)
    |> Enum.sum()
  end

  defp count_matches(mat) do
    [:rows, :cols, :diag] 
    |> Enum.map(&count_matches(mat, &1))
    |> Enum.sum()
  end

  def solve(mat), do: count_matches(mat)
end

defmodule Day4.Part2 do
  import Day4
  @chunk_size String.length("MAS")
  defguardp is_square(mat, n) when length(mat) == n and length(hd mat) == n

  defp is_match?(mat) when is_square(mat, @chunk_size) do
    mat 
    |> diagonals()
    |> Enum.all?(&is_match?/1)
  end

  defp is_match?(["M", "A", "S"]), do: true
  defp is_match?(["S", "A", "M"]), do: true
  defp is_match?(_), do: false

  defp count_matches(mat) do
    windows(mat, @chunk_size)
    |> Enum.count(&is_match?/1)
  end

  def solve(mat), do: count_matches(mat)
end

input = File.stream!("./input.txt")
  |> Enum.map(&String.trim/1)
  |> Enum.map(&String.graphemes/1)
 
IO.puts("part 1: #{Day4.Part1.solve(input)}")
IO.puts("part 2: #{Day4.Part2.solve(input)}")

