defmodule Guard do
  defstruct pos: {0, 0}, dir: {0, 0}
end

defmodule Day6 do
  @guard "^"
  @obstacle "#"

  @top {-1, 0}
  @down {1, 0}
  @left {0, -1}
  @right {0, 1}

  def parse(string) do
    string
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
  end

  defp guard(map) do
    pos = 
      map
      |> Enum.with_index()
      |> Enum.find_value(fn {list, row} ->
        case Enum.find_index(list, &(&1 == @guard)) do
          nil -> nil
          col -> {row, col}
        end
      end)
    dir = case at(map, pos) do
      "^" -> @top
      "v" -> @down
      "<" -> @left
      ">" -> @right
    end
    %Guard{pos: pos, dir: dir}
  end

  defp turn(%Guard{pos: p, dir: @top}), do: %Guard{pos: p, dir: @right}
  defp turn(%Guard{pos: p, dir: @right}), do: %Guard{pos: p, dir: @down}
  defp turn(%Guard{pos: p, dir: @down}), do: %Guard{pos: p, dir: @left}
  defp turn(%Guard{pos: p, dir: @left}), do: %Guard{pos: p, dir: @top}

  def at(map, {row, col}), do: Enum.at(Enum.at(map, row, []), col)

  def move(%Guard{pos: p, dir: d}), do: %Guard{pos: move(p, d), dir: d}

  def move({a, b}, {x, y}), do: {a + x, b + y}

  def move(map, guard) when is_struct(guard, Guard) do 
    next = move(guard.pos, guard.dir)
    case at(map, next) do
      @obstacle -> move(map, turn(guard))
      _ -> %Guard{pos: next, dir: guard.dir}
    end
  end

  def path(map, guard) do
    case at(map, guard.pos) do
      nil -> []
      _ -> [guard.pos | path(map, move(map, guard))]
    end
  end

  def part1(map) do
    path(map, guard(map))
    |> Enum.uniq()
    |> Enum.count()
  end
end

map = Day6.parse(File.read!("./input.txt"))
IO.puts("part 1: #{Day6.part1(map)}")

