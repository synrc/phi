defmodule Phi.Desugar do
  @moduledoc """
  Desugars the AST after parsing.
  Handles:
  - Multi-equation function definitions (merges them).
  - Guards (converts to if-then-else).
  - Patterns in let/where (converts to lambdas or Erlang matches).
  - Do-notation (converts to bind applications).
  - Class instance methods (groups them).
  """
  alias Phi.AST

  def desugar(%AST.Module{declarations: decls} = mod) do
    grouped_decls = group_decls(decls)
    desugared_decls = Enum.map(grouped_decls, &desugar_decl/1)
    %{mod | declarations: desugared_decls}
  end

  defp group_decls([]), do: []
  defp group_decls([%AST.DeclValue{name: name} = d | rest]) when name != "_pat" do
    {same, different} = Enum.split_with(rest, fn
      %AST.DeclValue{name: ^name} -> true
      _ -> false
    end)

    merged = if same == [] do
      # Single equation but might still have guards
      if d.guards && length(d.guards) > 0 do
        %{d | expr: desugar_guards(d.guards), guards: nil}
      else
        d
      end
    else
      # Multiple equations: f p1 p2 = e1; f q1 q2 = e2
      all_eqs = [d | same]
      arity = length(d.binders)

      arg_names = Enum.map(1..arity, fn i -> "_arg#{i}" end)
      new_binders = Enum.map(arg_names, fn n -> %AST.ExprVar{name: n} end)

      case_target = case arity do
        1 -> hd(new_binders)
        _ -> %AST.ExprTuple{elems: new_binders}
      end

      branches = Enum.map(all_eqs, fn eq ->
        pat = case length(eq.binders) do
          1 -> hd(eq.binders)
          _ -> %AST.ExprTuple{elems: eq.binders}
        end
        body = if eq.guards && length(eq.guards) > 0, do: desugar_guards(eq.guards), else: eq.expr
        {pat, body}
      end)
      %AST.DeclValue{name: name, binders: new_binders, expr: %AST.ExprCase{exprs: [case_target], branches: branches}, guards: nil}
    end
    [merged | group_decls(different)]
  end
  defp group_decls([other | rest]), do: [other | group_decls(rest)]

  defp desugar_decl(%AST.DeclValue{name: name, binders: binders, expr: expr, guards: guards}) do
    base_expr = if guards && length(guards) > 0 do
      desugar_guards(guards)
    else
      expr
    end

    desugared_expr = if base_expr, do: desugar_expr(base_expr), else: nil

    final_expr = if binders == [] or name == "_pat" do
      desugared_expr
    else
      if desugared_expr do
        Enum.reduce(Enum.reverse(binders), desugared_expr, fn binder, acc ->
          %AST.ExprLam{binder: binder, body: acc}
        end)
      else
        nil
      end
    end

    new_binders = if name == "_pat", do: binders, else: []
    %AST.DeclValue{name: name, binders: new_binders, expr: final_expr}
  end
  defp desugar_decl(%AST.DeclInstance{members: members} = inst) do
    desugared_members = members |> group_decls() |> Enum.map(&desugar_decl/1)
    %{inst | members: desugared_members}
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
  defp desugar_expr(%AST.ExprIf{cond: c, then_br: t, else_br: e}) do
    %AST.ExprIf{cond: desugar_expr(c), then_br: desugar_expr(t), else_br: desugar_expr(e)}
  end
  defp desugar_expr(%AST.ExprDo{statements: stmts}) do
    desugar_do(stmts)
  end
  defp desugar_expr(%AST.ExprCase{exprs: exprs, branches: branches} = ec) do
    %{ec | exprs: Enum.map(exprs, &desugar_expr/1),
           branches: Enum.map(branches, fn {pat, body} -> {pat, desugar_expr(body)} end)}
  end
  defp desugar_expr(%AST.ExprRecord{fields: fields}) do
    %AST.ExprRecord{fields: Enum.map(fields, fn {n, e} -> {n, desugar_expr(e)} end)}
  end
  defp desugar_expr(%AST.ExprRecordUpdate{base: base, fields: fields}) do
    %AST.ExprRecordUpdate{base: desugar_expr(base),
                          fields: Enum.map(fields, fn {n, e} -> {n, desugar_expr(e)} end)}
  end
  defp desugar_expr(other), do: other

  defp desugar_do([]), do: %AST.ExprVar{name: "unit"}
  defp desugar_do([{:bind, pat, expr} | rest]) do
    inner = desugar_do(rest)
    %AST.ExprApp{
      func: %AST.ExprApp{
        func: %AST.ExprVar{name: "bind"},
        arg: desugar_expr(expr)
      },
      arg: %AST.ExprLam{binder: pat, body: inner}
    }
  end
  defp desugar_do([{:let, decls} | rest]) do
    inner = desugar_do(rest)
    %AST.ExprLet{bindings: decls, body: inner}
  end
  defp desugar_do([{:expr, expr} | rest]) do
    inner = desugar_do(rest)
    %AST.ExprApp{
      func: %AST.ExprApp{
        func: %AST.ExprVar{name: "bind"},
        arg: desugar_expr(expr)
      },
      arg: %AST.ExprLam{binder: %AST.ExprVar{name: "_"}, body: inner}
    }
  end
  defp desugar_do([{:expr, expr}]), do: desugar_expr(expr)
  defp desugar_do([expr | rest]) do
    inner = desugar_do(rest)
    %AST.ExprApp{
      func: %AST.ExprApp{
        func: %AST.ExprVar{name: "bind"},
        arg: desugar_expr(expr)
      },
      arg: %AST.ExprLam{binder: %AST.ExprVar{name: "_"}, body: inner}
    }
  end
  defp desugar_do([expr]), do: desugar_expr(expr)

  defp desugar_guards([]), do: %AST.ExprVar{name: "undefined"}
  defp desugar_guards([{guard, body} | rest]) do
    %AST.ExprIf{cond: desugar_expr(guard), then_br: desugar_expr(body), else_br: desugar_guards(rest)}
  end
end
