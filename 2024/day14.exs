defmodule Day14 do
  @width 101
  @height 103

  defp add(a, b), do: Enum.zip_with(a, b, &(&1 + &2))
  defp sub(a, b), do: Enum.zip_with(a, b, &(&1 - &2))
  defp mod(a, b), do: Enum.zip_with(a, b, &Integer.mod/2)
  defp mag([x, y]), do: :math.sqrt(x * x + y * y)

  def parse(input) do
    Regex.scan(~r/-?\d+/, input)
    |> List.flatten()
    |> Enum.map(&String.to_integer/1)
    |> Enum.chunk_every(2)
    |> Enum.chunk_every(2)
  end

  defp simulate([pos, vel], 0), do: [mod(pos, [@width, @height]), vel]
  defp simulate([pos, vel], seconds), do: simulate([add(pos, vel), vel], seconds - 1)
  
  defp in_middle?([[x, y], _]), do: x == div(@width, 2) or y == div(@height, 2)

  defp quadrant([[x, y], _]) when x == div(@width, 2) or y == div(@height, 2), do: nil
  defp quadrant([[x, y], _]), do: div(x, div(@width, 2) + 1) + div(y, div(@height, 2) + 1) * 2

  def part1(robots) do 
    robots
    |> Enum.map(&simulate(&1, 100)) 
    |> Enum.reject(&in_middle?/1) 
    |> Enum.group_by(&quadrant/1)
    |> Enum.map(fn {_, robots} -> Enum.count(robots) end)
    |> Enum.product()
  end

  defp traverse(robots, [from, _] = robot, visited \\ []) do
    robots
    |> Stream.reject(&(&1 in visited))
    |> Stream.filter(fn [to, _] -> mag(sub(from, to)) == 1 end)
    |> Enum.reduce([robot | visited], &traverse(robots, &1, &2))
    |> Enum.uniq()
  end

  defp shapes([]), do: []

  defp shapes([head | tail] = robots) do
    visited = traverse(tail, head)
    [visited | shapes(robots -- visited)]
  end

  @max_seconds @width * @height
  def part2(robots) do
    robots
    |> Stream.iterate(fn robots -> Enum.map(robots, &simulate(&1, 1)) end)
    |> Stream.map(fn robots ->
      robots
      |> shapes()
      |> Enum.map(fn shape -> length(shape) end)
      |> Enum.max()
    end)
    |> Stream.with_index()
    |> Stream.take(@max_seconds)
    |> Enum.max_by(fn {size, _} -> size end)
    |> elem(1)
  end
end

File.read!("./input.txt")
|> Day14.parse()
|> tap(&IO.puts("part 1: #{Day14.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day14.part2(&1)}"))
