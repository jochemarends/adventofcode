defmodule Day8 do
  def parse(string) do
    string
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
  end

  def rows(map), do: length(map)
  def cols(map), do: length(hd(map))
  def at(map, {row, col}) when row < 0 or col < 0, do: nil
  def at(map, {row, col}), do: Enum.at(Enum.at(map, row, []), col)
  def distance({x1, y1}, {x2, y2}), do: {abs(x2 - x1), abs(y2 - y1)}

  def indices(map) do
    for row <- 0..(rows(map) - 1), col <- 0..(cols(map) - 1) do
      {row, col}
    end
  end

  def freqs(map) do
    map
    |> indices()
    |> Enum.filter((&at(map, &1) != "."))
    |> Enum.group_by(&(at(map, &1)))
  end

  def part1(map) do
    map
    |> freqs()
    |> Enum.flat_map(fn {freq, indices} -> 
      for {x1, y1} = p1 <- indices, {x2, y2} = p2 <- indices, p1 != p2 do 
        index = {x1 - (x2 - x1), y1 - (y2 - y1)}
        found = at(map, index)
        if found == freq or found == nil, do: nil, else: index
      end
    end)
    |> Enum.reject(&(&1 == nil))
    |> Enum.uniq()
    |> Enum.count()
  end

  def part2(_), do: "foo"
end

File.read!("./input.txt")
|> Day8.parse()
|> tap(&IO.puts("part 1: #{Day8.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day8.part2(&1)}"))

