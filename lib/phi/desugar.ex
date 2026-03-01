defmodule Phi.Desugar do
  @moduledoc """
  Desugars the Phi AST, transforming syntactic sugar into core representations.
  """
  alias Phi.AST

  def desugar(%AST.Module{declarations: decls} = mod) do
    %{mod | declarations: Enum.map(decls, &desugar_decl/1)}
  end
  def desugar(other), do: other

  defp desugar_decl(%AST.DeclValue{name: name, binders: binders, expr: expr}) do
    # Transform `f x y = e` into `f = \x -> \y -> e`
    desugared_expr = desugar_expr(expr)

    final_expr = Enum.reduce(Enum.reverse(binders), desugared_expr, fn binder, acc ->
      %AST.ExprLam{binder: binder, body: acc}
    end)

    %AST.DeclValue{name: name, binders: [], expr: final_expr}
  end
  defp desugar_decl(decl), do: decl

  defp desugar_expr(%AST.ExprApp{func: f, arg: a}) do
    %AST.ExprApp{func: desugar_expr(f), arg: desugar_expr(a)}
  end
  defp desugar_expr(%AST.ExprLam{binder: b, body: body}) do
    %AST.ExprLam{binder: b, body: desugar_expr(body)}
  end
  defp desugar_expr(%AST.ExprLet{bindings: bindings, body: body}) do
    desugared_bindings = Enum.map(bindings, &desugar_decl/1)
    %AST.ExprLet{bindings: desugared_bindings, body: desugar_expr(body)}
  end
  defp desugar_expr(expr), do: expr

end
