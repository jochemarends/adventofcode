defmodule Day8 do
  def parse(string) do
    string
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
  end

  def at(_, {row, col}) when row < 0 or col < 0, do: nil
  def at(map, {row, col}), do: Enum.at(Enum.at(map, row, []), col)

  def indices(map) do
    for row <- 0..(length(map) - 1), col <- 0..(length(hd(map)) - 1), do: {row, col}
  end

  def freqs(map) do
    map
    |> indices()
    |> Enum.filter((&at(map, &1) != "."))
    |> Enum.group_by(&(at(map, &1)))
  end

  def det([[a, b], [c, d]]), do: a * d - b * c

  def contains({{x1, y1}, {x2, y2}}, {x3, y3}) do
    mat = [[x2 - x1, y2 - y1], [x3 - x1, y3 - y1]]
    det(mat) == 0
  end

  def part1(map) do
    map
    |> freqs()
    |> Enum.flat_map(fn {freq, points} ->
      for {x1, y1} = p1 <- points, {x2, y2} = p2 <- points, p1 != p2 do
        {x1 - (x2 - x1), y1 - (y2 - y1)}
      end
      |>Enum.reject(&(at(map, &1) in [nil, freq]))
    end)
    |> Enum.uniq()
    |> Enum.count()
  end

  def part2(map) do
    lines = 
      map
      |> freqs()
      |> Enum.flat_map(fn {_, points} -> 
        for p1 <- points, p2 <- points, p1 < p2, do: {p1, p2}
      end)
    
    map
    |> indices()
    |> Enum.filter(fn point -> Enum.any?(lines, &contains(&1, point)) end)
    |> Enum.count()
  end
end

File.read!("./input.txt")
|> Day8.parse()
|> tap(&IO.puts("part 1: #{Day8.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day8.part2(&1)}"))

