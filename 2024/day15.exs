defmodule AABB do
  defstruct min: {0, 0}, max: {0, 0}

  def collide?(%AABB{min: {a, b}, max: {c, d}}, %AABB{min: {e, f}, max: {g, h}}) do
    c > e and g > a and d > f and h > b
  end

  def move(%AABB{min: {a, b}, max: {c, d}}, {dx, dy}) do
    %AABB{min: {a + dx, b + dy}, max: {c + dx, d + dy}}
  end
end

defmodule Day15 do
  defp add({a, b}, {c, d}), do: {a + c, b + d}
  defp mul({a, b}, {c, d}), do: {a * c, b * d}
  defp dot({a, b}, {c, d}), do: a * c + b * d

  def parse(input) do
    [warehouse, movements] = String.split(input, "\n\n")

    warehouse =
      warehouse
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.flat_map(fn {line, row} -> 
        line
        |> String.graphemes() 
        |> Enum.with_index()
        |> Enum.map(fn {type, col} -> {{row, col}, type} end)
      end)
      |> Enum.reject(fn {_, type} -> type == "." end)

    {warehouse, String.graphemes(String.replace(movements, "\n", ""))}
  end

  defp offset("^", _), do: {-1, 0}
  defp offset("v", _), do: {1, 0}
  defp offset("<", :part1), do: {0, -1}
  defp offset(">", :part1), do: {0, 1}
  defp offset(string, :part2), do: mul(offset(string, :part1), {0, 0.5})

  defp move(warehouse, movement, part) do
    warehouse
    |> Enum.find(fn {_, type} -> type == "@" end)
    |> then(&move(warehouse, [&1], movement, part))
  end

  defp aabb({{x, y}, "@"}, :part2), do: %AABB{min: {x, y}, max: {x + 1, y + 0.5}}
  defp aabb({{x, y}, _}, _), do: %AABB{min: {x, y}, max: {x + 1, y + 1}}

  defp move(warehouse, entities, movement, part) do
    if Enum.count(entities) == 0 or Enum.any?(entities, fn {_, type} -> type == "#" end) do
      warehouse
    else
      warehouse = warehouse -- entities

      moved =
        entities
        |> Enum.map(fn {pos, type} -> {add(pos, offset(movement, part)), type} end)

      warehouse =
        warehouse
        |> Enum.filter(fn entity ->
          moved
          |> Enum.any?(&AABB.collide?(aabb(&1, part), aabb(entity, part)))
        end)
        |> then(&move(warehouse, &1, movement, part))

      can_move =
        warehouse
        |> Enum.any?(fn entity ->
          moved
          |> Enum.any?(&AABB.collide?(aabb(&1, part), aabb(entity, part)))
        end)
        |> Kernel.not()

      if can_move do
        Enum.concat(warehouse, moved)
      else
        Enum.concat(warehouse, entities)
      end
    end
  end

  def solve({warehouse, movements}, part) do
    boxes =
      movements
      |> Enum.reduce(warehouse, &move(&2, &1, part))
      |> Enum.filter(fn {_, type} -> type == "O" end)

    case part do
      :part1 -> Enum.reduce(boxes, 0, fn {pos, _}, acc -> acc + dot({100, 1}, pos) end)
      :part2 -> Enum.reduce(boxes, 0, fn {pos, _}, acc -> acc + dot({100, 2}, pos) end)
    end
  end
end

File.read!("./input.txt")
|> Day15.parse()
|> tap(&IO.puts("part 1: #{Day15.solve(&1, :part1)}"))
|> tap(&IO.puts("part 2: #{Day15.solve(&1, :part2)}"))
