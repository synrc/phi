defmodule ASTWalker do
  def walk(node, parent) do
    if is_list(node) and length(node) > 0 and Enum.all?(node, &(is_struct(&1) && String.starts_with?(to_string(&1.__struct__), "Elixir.Phi.AST.Expr"))) do
      IO.puts("FOUND LIST in parent: #{inspect(parent)}")
      IO.inspect(node, limit: 3)
    end
    cond do
      is_struct(node) ->
        node |> Map.from_struct() |> Enum.each(fn {k, v} -> walk(v, {node.__struct__, k}) end)
      is_list(node) ->
        Enum.each(node, &walk(&1, parent))
      true -> :ok
    end
  end
end

ast = Phi.Parser.parse(Phi.Lexer.lex(File.read!("lib/Text/Parsec.hm"))) |> elem(1)
desugared_ast = Phi.Desugar.desugar(ast)
ASTWalker.walk(desugared_ast, :root)
