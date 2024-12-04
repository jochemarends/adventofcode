defmodule Solution do
  @chunk_size String.length("XMAS")

  defguardp is_square(mat, n) when length(mat) == n and length(hd mat) == n

  defp flip(mat), do: Enum.map(mat, &Enum.reverse(&1))

  defp is_match?(["X", "M", "A", "S"]), do: true
  defp is_match?(["S", "A", "M", "X"]), do: true
  defp is_match?(_), do: false

  defp count_matches(grid, :rows) do
    grid
    |> Enum.map(fn row ->
      row
      |> Enum.chunk_every(@chunk_size, 1,:discard)
      |> Enum.count(&is_match?/1)
    end)
    |> Enum.sum()
  end

  defp count_matches(grid, :cols) do
    grid 
    |> List.zip
    |> Enum.map(&Tuple.to_list/1)
    |> count_matches(:rows)
  end

  defp count_matches(grid, :diag) when not is_square(grid, @chunk_size) do
     for row <- 0..(length(grid) - @chunk_size),
         col <- 0..(length(List.first(grid)) - @chunk_size) do
      grid
      |> Enum.drop(row)
      |> Enum.take(@chunk_size)
      |> Enum.map(fn row ->
        row
        |> Enum.drop(col)
        |> Enum.take(@chunk_size)
      end)
      |> count_matches(:diag)
    end
    |> Enum.sum()
  end

  defp count_matches(grid, :diag) do
    [:forward, :reverse]
    |> Enum.map(&count_matches(grid, :diag, &1))
    |> Enum.sum()
  end

  defp count_matches(grid, :diag, :forward) do
    is_match = 0..(@chunk_size- 1)
      |> Enum.map(fn i -> Enum.at(Enum.at(grid, i), i) end)
      |> is_match?()
    if is_match, do: 1, else: 0
  end

  defp count_matches(grid, :diag, :reverse) do
    count_matches(flip(grid), :diag, :forward)
  end

  defp count_matches(grid) do
    [:rows, :cols, :diag] 
    |> Enum.map(&count_matches(grid, &1))
    |> Enum.sum()
  end

  def part1(grid), do: count_matches(grid)
end

grid = File.stream!("./input.txt")
  |> Enum.map(&String.trim/1)
  |> Enum.map(&String.graphemes/1)

IO.puts("part 1: #{Solution.part1(grid)}")

