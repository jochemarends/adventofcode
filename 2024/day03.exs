defmodule Day3 do
  defguardp is_digit(c) when c in ?0..?9

  def lex(input, tokens \\ [])

  def lex(<<>>, tokens) do
    [:eof | tokens] |> Enum.reverse()
  end

  def lex(input, tokens) do
    {token, rest} = tokenize(input)
    lex(rest, [token | tokens])
  end

  defp tokenize(<<"mul", rest::binary>>), do: {:mul, rest}
  defp tokenize(<<"(", rest::binary>>), do: {:lparen, rest}
  defp tokenize(<<")", rest::binary>>), do: {:rparen, rest}
  defp tokenize(<<",", rest::binary>>), do: {:comma, rest}
  defp tokenize(<<"don't()", rest::binary>>), do: {:skip, rest}
  defp tokenize(<<"do()", rest::binary>>), do: {:noskip, rest}
  defp tokenize(<<c::utf8, rest::binary>>) when is_digit(c), do: read_number(rest, <<c>>)
  defp tokenize(<<_::utf8, rest::binary>>), do: {:illegal, rest}

  defp read_number(<<c::utf8, rest::binary>>, acc) when is_digit(c) do
    read_number(rest, <<acc::binary, c::utf8>>)
  end

  defp read_number(<<rest::binary>>, acc) do
    if String.length(acc) > 3 do
      {:illegal, rest}
    else
      {{:int, String.to_integer(acc)}, rest}
    end
  end

  def eval(tokens, part, acc \\ 0)
  
  def eval([], _, acc), do: acc

  def eval([:skip | rest], :part2, acc) do
    rest
    |> Enum.drop_while(&(&1 != :noskip))
    |> eval(:part2, acc)
  end

  def eval([:mul, :lparen, {:int, a}, :comma, {:int, b}, :rparen | rest], part, acc) do
    eval(rest, part, acc + a * b)
  end

  def eval([_ | rest], part, acc), do: eval(rest, part, acc)

  def part1(input), do: input |> lex() |> eval(:part1)
  def part2(input), do: input |> lex() |> eval(:part2)
end

File.read!("./input.txt")
|> tap(&IO.puts("part 1: #{Day3.part1(&1)}"))
|> tap(&IO.puts("part 2: #{Day3.part2(&1)}"))
