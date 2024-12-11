defmodule Day11 do
  require Integer

  def parse(string) do
    string
    |> String.replace("\n", "")
    |> String.split(" ", trim: true)
    |> Enum.map(&String.to_integer/1)
  end
  
  defp digits(0), do: 1
  defp digits(number), do: floor(:math.log10(number)) + 1
  
  defp process(stones, blinks) do
    cache = fetch(stones, blinks)
    stones
    |> Enum.map(&Map.get(cache, {&1, blinks}))
    |> Enum.sum()
  end

  defp fetch(stones, blinks, cache \\ Map.new())

  defp fetch(stones, blinks, cache) when is_list(stones) do
    stones
    |> Enum.reduce(cache, fn stone, cache ->
      fetch(stone, blinks, cache) 
    end)
  end

  defp fetch(stone, 0, cache), do: Map.put(cache, {stone, 0}, 1)

  defp fetch(stone, blinks, cache) do
    case Map.fetch(cache, {stone, blinks}) do
      {:ok, _} -> cache
      :error -> 
        cache = fetch(blink(stone), blinks - 1, cache)
        blink(stone)
        |> Enum.map(&Map.get(cache, {&1, blinks - 1}))
        |> Enum.sum()
        |> then(&Map.put(cache, {stone, blinks}, &1))
    end
  end

  defp blink(0), do: [1]

  defp blink(stone) do
    case digits(stone) do
      n when Integer.is_odd(n) -> [stone * 2024]
      n ->
        stone
        |> Integer.digits()
        |> Enum.chunk_every(div(n, 2))
        |> Enum.map(fn chunk ->
          chunk
          |> Enum.join()
          |> String.to_integer()
        end)
    end
  end

  def part1(input), do: process(input, 25)
  def part2(input), do: process(input, 75)
end

File.read!("./input.txt")
|> Day11.parse()
|> tap(&IO.puts("part 1: #{Day11.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day11.part2(&1)}"))

