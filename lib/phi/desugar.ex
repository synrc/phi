defmodule Phi.Desugar do
  @moduledoc """
  Desugars the Phi AST, transforming syntactic sugar into core representations.
  """
  alias Phi.AST

  def desugar(%AST.Module{declarations: decls} = mod) do
    grouped_decls = group_decls(decls)
    %{mod | declarations: Enum.map(grouped_decls, &desugar_decl/1)}
  end
  def desugar(other), do: other

  defp group_decls([]), do: []
  defp group_decls([%AST.DeclValue{name: name} = d1 | rest]) do
    {equations, other_decls} = Enum.split_while(rest, fn
      %AST.DeclValue{name: ^name} -> true
      _ -> false
    end)

    if equations == [] do
      [d1 | group_decls(rest)]
    else
      # We have multiple equations for the same name
      all_eqs = [d1 | equations]
      # Assuming all have the same arity for now
      arity = length(d1.binders)

      # Create fresh variables for arguments
      arg_names = Enum.map(1..arity, fn i -> "arg#{i}" end)
      target_exprs = Enum.map(arg_names, fn n -> %AST.ExprVar{name: n} end)

      # Create branches
      branches = Enum.map(all_eqs, fn eq ->
        {eq.binders, eq.expr}
      end)

      case_expr = %AST.ExprCase{exprs: target_exprs, branches: branches}

      # Wrap in lambdas
      final_expr = Enum.reduce(Enum.reverse(arg_names), case_expr, fn name, acc ->
        %AST.ExprLam{binder: %AST.BinderVar{name: name}, body: acc}
      end)

      consolidated = %AST.DeclValue{name: name, binders: [], expr: final_expr}
      [consolidated | group_decls(other_decls)]
    end
  end
  defp group_decls([other | rest]), do: [other | group_decls(rest)]

  defp desugar_decl(%AST.DeclValue{name: name, binders: binders, expr: expr}) do
    # Transform `f x y = e` into `f = \x -> \y -> e`
    # (Existing DeclValue with binders should also be desugared to lambdas)
    desugared_expr = desugar_expr(expr)

    final_expr = if binders == [] do
      desugared_expr
    else
      Enum.reduce(Enum.reverse(binders), desugared_expr, fn binder, acc ->
        %AST.ExprLam{binder: binder, body: acc}
      end)
    end

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
    grouped_bindings = group_decls(bindings)
    desugared_bindings = Enum.map(grouped_bindings, &desugar_decl/1)
    %AST.ExprLet{bindings: desugared_bindings, body: desugar_expr(body)}
  end
  defp desugar_expr(expr), do: expr

end
