defmodule Phi.Lexer do
  @moduledoc """
  Lexer for Phi (Hamler/PureScript-like syntax).
  """

  def lex(input) do
    lex(String.to_charlist(input), 1, 1, [])
  end

  defp lex([], _line, _col, acc), do: {:ok, Enum.reverse(acc)}

  defp lex([?\n | rest], line, _col, acc), do: lex(rest, line + 1, 1, acc)
  defp lex([?  | rest], line, col, acc), do: lex(rest, line, col + 1, acc)
  defp lex([?\r | rest], line, col, acc), do: lex(rest, line, col, acc)
  defp lex([?\t | rest], line, col, acc), do: lex(rest, line, col + 4, acc)

  # Comments
  defp lex([?-, ?- | rest], line, col, acc) do
    rest2 = Enum.drop_while(rest, fn c -> c != ?\n end)
    lex(rest2, line, col, acc)
  end

  defp lex([?{, ?- | rest], line, col, acc) do
    lex_block_comment(rest, line, col + 2, acc)
  end

  # Special symbols
  defp lex([?(, ?) | rest], line, col, acc), do: lex(rest, line, col + 2, [{:unit, line, col} | acc])
  defp lex([?( | rest], line, col, acc), do: lex(rest, line, col + 1, [{:left_paren, line, col} | acc])
  defp lex([?) | rest], line, col, acc), do: lex(rest, line, col + 1, [{:right_paren, line, col} | acc])
  defp lex([?[ | rest], line, col, acc), do: lex(rest, line, col + 1, [{:left_square, line, col} | acc])
  defp lex([?] | rest], line, col, acc), do: lex(rest, line, col + 1, [{:right_square, line, col} | acc])
  defp lex([?{ | rest], line, col, acc), do: lex(rest, line, col + 1, [{:left_brace, line, col} | acc])
  defp lex([?} | rest], line, col, acc), do: lex(rest, line, col + 1, [{:right_brace, line, col} | acc])
  defp lex([?, | rest], line, col, acc), do: lex(rest, line, col + 1, [{:comma, line, col} | acc])
  defp lex([?; | rest], line, col, acc), do: lex(rest, line, col + 1, [{:semicolon, line, col} | acc])
  defp lex([?` | rest], line, col, acc), do: lex(rest, line, col + 1, [{:backtick, line, col} | acc])

  # Double colon, Arrow, Double Arrow
  defp lex([?:, ?: | rest], line, col, acc), do: lex(rest, line, col + 2, [{:double_colon, line, col} | acc])
  defp lex([?-, ?> | rest], line, col, acc), do: lex(rest, line, col + 2, [{:arrow, line, col} | acc])
  defp lex([?=, ?> | rest], line, col, acc), do: lex(rest, line, col + 2, [{:double_arrow, line, col} | acc])
  defp lex([?\\ | rest], line, col, acc), do: lex(rest, line, col + 1, [{:backslash, line, col} | acc])

  # Atoms and Operators starting with :
  defp lex([?:, c | rest], line, col, acc) when (c >= ?a and c <= ?z) or (c >= ?A and c <= ?Z) or c == ?_ do
    {ident_chars, rest2} = take_ident([c | rest])
    ident = List.to_string(ident_chars)
    lex(rest2, line, col + 1 + String.length(ident), [{:atom, line, col, ident} | acc])
  end

  defp lex([?: | rest], line, col, acc), do: lex(rest, line, col + 1, [{:operator, line, col, ":"} | acc])

  # Numbers and Floats
  defp lex([c | rest], line, col, acc) when c >= ?0 and c <= ?9 do
    {num_chars, rest2} = take_while([c | rest], fn x -> x >= ?0 and x <= ?9 end)
    case rest2 do
      [?., d | rest3] when d >= ?0 and d <= ?9 ->
        {frac_chars, rest4} = take_while([d | rest3], fn x -> x >= ?0 and x <= ?9 end)
        float_chars = num_chars ++ [?.] ++ frac_chars
        float_str = List.to_string(float_chars)
        lex(rest4, line, col + length(float_chars), [{:float, line, col, float_str} | acc])
      _ ->
        num = List.to_integer(num_chars)
        lex(rest2, line, col + length(num_chars), [{:number, line, col, num} | acc])
    end
  end

  # Identifiers and Keywords
  defp lex([c | rest], line, col, acc) when (c >= ?a and c <= ?z) or c == ?_ do
    {ident_chars, rest2} = take_ident([c | rest])
    ident = List.to_string(ident_chars)
    case ident do
      "module" -> lex(rest2, line, col + String.length(ident), [{:module, line, col} | acc])
      "where" -> lex(rest2, line, col + String.length(ident), [{:where, line, col} | acc])
      "import" -> lex(rest2, line, col + String.length(ident), [{:import, line, col} | acc])
      "as" -> lex(rest2, line, col + String.length(ident), [{:as, line, col} | acc])
      "hiding" -> lex(rest2, line, col + String.length(ident), [{:hiding, line, col} | acc])
      "type" -> lex(rest2, line, col + String.length(ident), [{:type_kw, line, col} | acc])
      "data" -> lex(rest2, line, col + String.length(ident), [{:data, line, col} | acc])
      "newtype" -> lex(rest2, line, col + String.length(ident), [{:newtype, line, col} | acc])
      "class" -> lex(rest2, line, col + String.length(ident), [{:class, line, col} | acc])
      "instance" -> lex(rest2, line, col + String.length(ident), [{:instance, line, col} | acc])
      "foreign" -> lex(rest2, line, col + String.length(ident), [{:foreign, line, col} | acc])
      "infix" -> lex(rest2, line, col + String.length(ident), [{:infix, line, col} | acc])
      "infixl" -> lex(rest2, line, col + String.length(ident), [{:infixl, line, col} | acc])
      "infixr" -> lex(rest2, line, col + String.length(ident), [{:infixr, line, col} | acc])
      "let" -> lex(rest2, line, col + String.length(ident), [{:let, line, col} | acc])
      "in" -> lex(rest2, line, col + String.length(ident), [{:in, line, col} | acc])
      "if" -> lex(rest2, line, col + String.length(ident), [{:if_kw, line, col} | acc])
      "then" -> lex(rest2, line, col + String.length(ident), [{:then_kw, line, col} | acc])
      "else" -> lex(rest2, line, col + String.length(ident), [{:else_kw, line, col} | acc])
      "case" -> lex(rest2, line, col + String.length(ident), [{:case, line, col} | acc])
      "of" -> lex(rest2, line, col + String.length(ident), [{:of, line, col} | acc])
      "do" -> lex(rest2, line, col + String.length(ident), [{:do, line, col} | acc])
      "forall" -> lex(rest2, line, col + String.length(ident), [{:forall, line, col} | acc])
      _ -> lex(rest2, line, col + String.length(ident), [{:var_ident, line, col, ident} | acc])
    end
  end

  defp lex([c | rest], line, col, acc) when c >= ?A and c <= ?Z do
    {ident_chars, rest2} = take_ident([c | rest])
    ident = List.to_string(ident_chars)
    lex(rest2, line, col + String.length(ident), [{:proper_name, line, col, ident} | acc])
  end

  # Operators
  defp lex([c | rest], line, col, acc) when c in [?=, ?|, ?., ?<, ?>, ?+, ?-, ?*, ?/, ?%, ?^, ?&, ?!, ?$, ?#, ?@, ??] do
    {op_chars, rest2} = take_while([c | rest], fn x -> x in [?=, ?|, ?., ?<, ?>, ?+, ?-, ?*, ?/, ?%, ?^, ?&, ?!, ?$, ?#, ?@, ?:, ??] end)
    op = List.to_string(op_chars)
    token = case op do
      "=" -> {:=, line, col}
      "|" -> {:pipe, line, col}
      "." -> {:dot, line, col}
      _ -> {:operator, line, col, op}
    end
    lex(rest2, line, col + String.length(op), [token | acc])
  end

  # Strings
  defp lex([?" | rest], line, col, acc) do
    {str_chars, rest2} = take_string(rest)
    lex(rest2, line, col + length(str_chars) + 2, [{:string, line, col, List.to_string(str_chars)} | acc])
  end

  # Char
  defp lex([?' , ?\\, c, ?' | rest], line, col, acc) do
    lex(rest, line, col + 4, [{:char, line, col, c} | acc])
  end
  defp lex([?' , c, ?' | rest], line, col, acc) do
    lex(rest, line, col + 3, [{:char, line, col, c} | acc])
  end

  defp lex([c | _rest], line, col, _acc) do
    {:error, "Unexpected character: #{<<c::utf8>>} at #{line}:#{col}"}
  end

  defp lex_block_comment([?-, ?} | rest], line, col, acc), do: lex(rest, line, col + 2, acc)
  defp lex_block_comment([?\n | rest], line, _col, acc), do: lex_block_comment(rest, line + 1, 1, acc)
  defp lex_block_comment([_ | rest], line, col, acc), do: lex_block_comment(rest, line, col + 1, acc)
  defp lex_block_comment([], _line, _col, acc), do: {:ok, Enum.reverse(acc)}

  defp take_ident([c | rest]) when (c >= ?a and c <= ?z) or (c >= ?A and c <= ?Z) or (c >= ?0 and c <= ?9) or c == ?_ or c == ?' do
    {rest_ident, rest2} = take_ident(rest)
    {[c | rest_ident], rest2}
  end
  defp take_ident(rest), do: {[], rest}

  defp take_while([], _), do: {[], []}
  defp take_while([c | rest], f) do
    if f.(c) do
      {matched, remaining} = take_while(rest, f)
      {[c | matched], remaining}
    else
      {[], [c | rest]}
    end
  end

  defp take_string([?" | rest]), do: {[], rest}
  defp take_string([?\\, c | rest]) do
    {s, r} = take_string(rest)
    {[?\\, c | s], r}
  end
  defp take_string([c | rest]) do
    {s, r} = take_string(rest)
    {[c | s], r}
  end
  defp take_string([]), do: {[], []}

end
