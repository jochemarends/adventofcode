defmodule AABB do
  defstruct min: {0, 0}, max: {0, 0}

  def collide?(%AABB{min: {a, b}, max: {c, d}}, %AABB{min: {e, f}, max: {g, h}}) do
  c > e and g > a
  end
end

defmodule Day15 do
  defp add({a, b}, {c, d}), do: {a + c, b + d}
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
      |> Map.new()

    {warehouse, String.graphemes(String.replace(movements, "\n", ""))}
  end

  defp offset("^"), do: {-1, 0}
  defp offset("v"), do: {1, 0}
  defp offset("<"), do: {0, -1}
  defp offset(">"), do: {0, 1}

  defp move(warehouse, movement) do
    warehouse
    |> Enum.find(fn {_, type} -> type == "@" end)
    |> elem(0)
    |> then(&move(warehouse, &1, movement))
  end

  defp move(warehouse, pos, movement) do
    case Map.get(warehouse, pos) do
      "#" -> warehouse
      nil -> warehouse
      _ -> 
        target = add(pos, offset(movement))
        warehouse = move(warehouse, target, movement)
        if not Map.has_key?(warehouse, target) do
          {type, warehouse} = Map.pop!(warehouse, pos)
          Map.put(warehouse, target, type)
        else
          warehouse
        end
    end
  end

  defp dump(warehouse) do
    {cols, _} = Enum.max_by(Map.keys(warehouse), fn {x, _} -> x end)
    {_, rows} = Enum.max_by(Map.keys(warehouse), fn {_, y} -> y end)

    for x <- 0..cols do
      for y <- 0..rows do
        case Map.get(warehouse, {x, y}) do
          nil -> IO.write(".")
          type -> IO.write(type)
        end
      end
      IO.write("\n")
    end
  end

  def part1({warehouse, movements}) do
    movements
    |> Enum.reduce(warehouse, fn movement, warehouse ->
      move(warehouse, movement)
    end)
    |> Enum.filter(fn {pos, type} -> type == "O" end)
    |> Enum.reduce(0, fn {pos, _}, acc -> acc + dot({100, 1}, pos) end)
  end

  defp transform(warehouse) do
    warehouse
    |> Enum.flat_map(fn {{row, col}, type} ->
      col = 2 * col

      case type do
        "." -> [
      end
    end)
  end

  def part2(_), do: "todo"
end

File.read!("./input.txt")
|> Day15.parse()
|> tap(&IO.puts("part 1: #{Day15.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day15.part2(&1)}"))
