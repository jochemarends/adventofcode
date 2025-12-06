defmodule Day5 do
  def part2(ranges), do: do_part2(Enum.sort(ranges))
  defp do_part2([curr]), do: Range.size(curr)
  defp do_part2([prev, curr | rest]) do
    if not Range.disjoint?(prev, curr) or prev.last + 1 == curr.first do
      merged = prev.first..max(prev.last, curr.last)
      do_part2([merged | rest])
    else
      Range.size(prev) + do_part2([curr | rest])
    end
  end
end

{ranges, ids} =
  for s <- File.read!("./input.txt") |> String.split("\n", trim: true) do
    case Integer.parse(s) do
      {id, ""} -> id
      {first, "-" <> rest} -> first..String.to_integer(rest)
    end
  end
  |> Enum.split_with(&match?(%Range{}, &1))

IO.puts("part 1: #{Enum.count(ids, fn id -> Enum.any?(ranges, & id in &1) end)}")
IO.puts("part 2: #{Day5.part2(ranges)}")
