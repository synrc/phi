defmodule Phi.Layout do
  @moduledoc """
  Implements the layout resolution algorithm for Hamler/PureScript syntax.
  Layout is suspended inside explicit context: (), [], {}.
  Blocks started inside an explicit context are closed by its closing symbol.
  """

  def resolve(tokens) do
    do_resolve(tokens, [], [], 0)
  end

  defp do_resolve([], acc, [_ | ctx_rest], 0) do
    do_resolve([], [{:right_brace, 0, 0} | acc], ctx_rest, 0)
  end
  defp do_resolve([], acc, [], _), do: Enum.reverse(acc)
  defp do_resolve([], acc, [_ | ctx_rest], depth) do
     do_resolve([], acc, ctx_rest, depth)
  end

  defp do_resolve([token | rest] = tokens, acc, ctx, depth) do
    line = elem(token, 1)
    col = elem(token, 2)

    case ctx do
      [] -> do_resolve_token(tokens, acc, ctx, depth)
      [{layout_type, layout_col, layout_depth} | ctx_rest] ->
        cond do
          # 1. Indentation outdent takes priority
          col < layout_col and depth <= layout_depth ->
            IO.inspect({token, layout_type, layout_col, layout_depth}, label: "Layout Outdent")
            do_resolve(tokens, [{:right_brace, line, col} | acc], ctx_rest, depth)

          # 2. Delimiters close layout blocks started at this or greater depth
          is_delimiter?(token) and layout_depth >= depth ->
            if layout_depth == 0 do
              do_resolve_token(tokens, acc, ctx, depth)
            else
              do_resolve(tokens, [{:right_brace, line, col} | acc], ctx_rest, depth)
            end

          # 3. 'in' keyword closes layout blocks >= its own column or if it's a let block
          elem(token, 0) == :in ->
            if layout_type == :let do
               do_resolve(tokens, [{:right_brace, line, col} | acc], ctx_rest, depth)
            else
               if layout_col >= col and layout_depth >= depth do
                 do_resolve(tokens, [{:right_brace, line, col} | acc], ctx_rest, depth)
               else
                 do_resolve_token(tokens, acc, ctx, depth)
               end
            end

          # 4. Semicolon injection
          col == layout_col and depth == layout_depth ->
            case acc do
              [{:left_brace, _, _} | _] -> do_resolve_token(tokens, acc, ctx, depth)
              _ when elem(token, 0) in [:in, :else_kw, :then_kw, :of] -> do_resolve_token(tokens, acc, ctx, depth)
              _ -> do_resolve_token(tokens, [{:semicolon, line, col} | acc], ctx, depth)
            end

          true ->
            do_resolve_token(tokens, acc, ctx, depth)
        end
    end
  end

  defp is_delimiter?({:right_paren, _, _}), do: true
  defp is_delimiter?({:right_square, _, _}), do: true
  defp is_delimiter?({:right_brace, _, _}), do: true
  defp is_delimiter?(_), do: false

  defp do_resolve_token([{:left_paren, line, col} | rest], acc, ctx, depth) do
    do_resolve(rest, [{:left_paren, line, col} | acc], ctx, depth + 1)
  end
  defp do_resolve_token([{:right_paren, line, col} | rest], acc, ctx, depth) do
    do_resolve(rest, [{:right_paren, line, col} | acc], ctx, max(0, depth - 1))
  end
  defp do_resolve_token([{:left_square, line, col} | rest], acc, ctx, depth) do
    do_resolve(rest, [{:left_square, line, col} | acc], ctx, depth + 1)
  end
  defp do_resolve_token([{:right_square, line, col} | rest], acc, ctx, depth) do
    do_resolve(rest, [{:right_square, line, col} | acc], ctx, max(0, depth - 1))
  end
  defp do_resolve_token([{:left_brace, line, col} | rest], acc, ctx, depth) do
    do_resolve(rest, [{:left_brace, line, col} | acc], ctx, depth + 1)
  end
  defp do_resolve_token([{:right_brace, line, col} | rest], acc, ctx, depth) do
    do_resolve(rest, [{:right_brace, line, col} | acc], ctx, max(0, depth - 1))
  end

  defp do_resolve_token([{:where, line, col} | rest], acc, ctx, depth) do
    acc = [{:where, line, col} | acc]
    case rest do
       [] ->
         do_resolve([], [{:left_brace, line, col + 1} | acc], [{:where, col + 1, depth} | ctx], depth)
       [next | _] ->
         next_line = elem(next, 1)
         next_col = elem(next, 2)
         do_resolve(rest, [{:left_brace, next_line, next_col} | acc], [{:where, next_col, depth} | ctx], depth)
    end
  end

  defp do_resolve_token([{:let, line, col} | rest], acc, ctx, depth) do
    acc = [{:let, line, col} | acc]
    case rest do
       [] -> do_resolve([], acc, ctx, depth)
       [next | _] ->
         next_line = elem(next, 1)
         next_col = elem(next, 2)
         do_resolve(rest, [{:left_brace, next_line, next_col} | acc], [{:let, next_col, depth} | ctx], depth)
    end
  end

  defp do_resolve_token([{:do, line, col} | rest], acc, ctx, depth) do
    acc = [{:do, line, col} | acc]
    case rest do
       [] -> do_resolve([], acc, ctx, depth)
       [next | _] ->
         next_line = elem(next, 1)
         next_col = elem(next, 2)
         do_resolve(rest, [{:left_brace, next_line, next_col} | acc], [{:do, next_col, depth} | ctx], depth)
    end
  end

  defp do_resolve_token([{:of, line, col} | rest], acc, ctx, depth) do
    acc = [{:of, line, col} | acc]
    case rest do
       [] -> do_resolve([], acc, ctx, depth)
       [next | _] ->
         next_line = elem(next, 1)
         next_col = elem(next, 2)
         do_resolve(rest, [{:left_brace, next_line, next_col} | acc], [{:of, next_col, depth} | ctx], depth)
    end
  end

  defp do_resolve_token([token | rest], acc, ctx, depth) do
    do_resolve(rest, [token | acc], ctx, depth)
  end

  defp has_let_in_ctx?([], _), do: false
  defp has_let_in_ctx?([{_, _, block_depth} | _], depth) when block_depth < depth, do: false
  defp has_let_in_ctx?([{:let, _, _} | _], _), do: true
  defp has_let_in_ctx?([_ | ctx], depth), do: has_let_in_ctx?(ctx, depth)
end
