coords =
  for(
    line <- File.stream!("./input.txt"),
    do: Enum.map(String.split(line, [",", "\n"], trim: true), &String.to_integer/1)
  )

graph = :digraph.new()

pairs =
  for(
    {p1, index} <- Enum.with_index(coords),
    p2 <- Enum.drop(coords, index + 1),
    do: {p1, p2}
  )
  |> Enum.sort_by(fn {p1, p2} -> Enum.zip_with(p1, p2, &((&1 - &2) ** 2)) |> Enum.sum() end)

for {p1, p2} <- pairs |> Enum.take(1000) do
  Enum.each([p1, p2], &:digraph.add_vertex(graph, &1))
  :digraph.add_edge(graph, p1, p2)
end

graph
|> :digraph_utils.components()
|> Enum.map(&Enum.count/1)
|> (&IO.puts("part 1: #{Enum.product(Enum.take(Enum.sort(&1, :desc), 3))}")).()

for {p1, p2} <- pairs |> Enum.drop(1000), do: Enum.each([p1, p2], &:digraph.add_vertex(graph, &1))

Enum.find(pairs |> Enum.drop(1000), fn {p1, p2} -> 
  :digraph.add_edge(graph, p1, p2)
  Enum.count(:digraph_utils.components(graph)) == 1
end)
|> (fn {[x1, _, _], [x2, _, _]} -> IO.puts("part 2: #{x1 * x2}") end).()
