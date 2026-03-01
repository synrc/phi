defmodule Phi.Codegen do
  @moduledoc """
  Translates Phi.AST into Erlang Abstract Format (Erlang AST)
  so that it can be compiled natively on the BEAM via `:compile.forms/2`.
  """
  alias Phi.AST
  alias Phi.Typechecker.Env
  alias Phi.Type

  @doc """
  Generates Erlang AST forms for a given Phi Module.
  """
  def generate(%AST.Module{name: name, declarations: decls}, env) do
    # Convention: module Data.List -> 'data_list'
    mod_atom = name |> String.downcase() |> String.replace(".", "_") |> String.to_atom()

    # Foreign module is the last part of the module name
    foreign_mod = List.last(String.split(name, ".")) |> String.to_atom()

    # Provide module attributes: -module(name). -compile(export_all).
    module_attr = {:attribute, 1, :module, mod_atom}
    export_all = {:attribute, 1, :compile, :export_all}

    # Generate functions for value declarations
    functions = Enum.flat_map(decls, fn d ->
      case generate_decl(d, mod_atom, foreign_mod, env) do
        nil -> []
        {:multi, fs} -> fs
        f -> [f]
      end
    end)

    # Forms are a list starting with module attributes, then exports/functions, ending with EOF
    forms = [module_attr, export_all] ++ functions ++ [{:eof, 1}]
    {:ok, forms}
  end

  # Translates a value declaration into an Erlang function
  defp generate_decl(%AST.DeclValue{name: name, expr: expr}, current_mod, _foreign_mod, env) do
    func_name = String.to_atom(name)
    {_mod, scheme, _real_name} = get_info(name, env)
    {num_dicts, num_args} = if scheme, do: split_arity(scheme), else: {0, 0}
    _sig_arity = num_dicts + num_args

    # Extract arguments from nested Lambdas
    {args, body_expr, env_func} = extract_args(expr, MapSet.new(), [])

    # Dictionaries are always missing from binders in Phi
    dict_names = get_dict_names(scheme)
    dict_vars = Enum.map(dict_names, fn n -> {:var, 1, String.to_atom(n)} end)

    # If signature has more args than the lambda binders, we must eta-expand
    {final_args, final_body, final_env} = if length(args) < num_args do
       needed = num_args - length(args)
       new_names = Enum.map(1..needed, fn i -> "eta_#{i}" end)
       new_erl_vars = Enum.map(new_names, fn n -> {:var, 1, String.capitalize(n) |> String.to_atom()} end)

       expanded_expr = Enum.reduce(new_names, body_expr, fn n, acc ->
         %AST.ExprApp{func: acc, arg: %AST.ExprVar{name: n}}
       end)

       {dict_vars ++ args ++ new_erl_vars, expanded_expr, Enum.reduce(dict_names ++ new_names, env_func, &MapSet.put(&2, &1))}
    else
       {dict_vars ++ args, body_expr, Enum.reduce(dict_names, env_func, &MapSet.put(&2, &1))}
    end

    erl_body = generate_expr(final_body, final_env, env, current_mod)

    # Erlang function clause
    clause = {:clause, 1, final_args, [], [erl_body]}
    {:function, 1, func_name, length(final_args), [clause]}
  end

  defp generate_decl(%AST.DeclForeign{name: name, type: type}, _current_mod, foreign_mod, _env) do
    func_name = String.to_atom(name)
    arity = type_arity(type)

    args = if arity > 0 do
      Enum.map(1..arity, fn i -> {:var, 1, String.to_atom("A#{i}")} end)
    else
      []
    end

    # name(A1, ...) -> foreign_mod:name(A1, ...).
    clause = {:clause, 1, args, [], [{:call, 1, {:remote, 1, {:atom, 1, foreign_mod}, {:atom, 1, func_name}}, args}]}
    {:function, 1, func_name, arity, [clause]}
  end

  defp generate_decl(%AST.DeclClass{members: members}, _current_mod, _foreign_mod, _env) do
    # For each member in class, generate an accessor:
    # m(Dict, Args...) -> (maps:get(m, Dict))(Args...)
    # But since we have curried functions, it might be more complex.
    # Simple approach: m(Dict) -> maps:get(m, Dict).
    # Then applications will work.

    # We return a list of Erlang forms (functions), but generate_decl expects one.
    # We'll use a wrapper to return multiple.
    functions = Enum.map(members, fn %AST.DeclTypeSignature{name: m_name} ->
      func_name = String.to_atom(m_name)
      # eq(Dict) -> maps:get(eq, Dict).
      dict_var = {:var, 1, :Dict}
      m_atom = {:atom, 1, func_name}
      get_call = {:call, 1, {:remote, 1, {:atom, 1, :maps}, {:atom, 1, :get}}, [m_atom, dict_var]}
      clause = {:clause, 1, [dict_var], [], [get_call]}
      {:function, 1, func_name, 1, [clause]}
    end)
    {:multi, functions}
  end

  defp generate_decl(%AST.DeclInstance{class: class_name, types: types, members: members, constraints: constraints}, current_mod, _foreign_mod, env) do
    # dict_Eq_Int() -> #{ eq => fun ... }.
    itypes = Enum.map(types, &Phi.Typechecker.ast_to_type(&1, env))
    type_suffixes = Enum.map(itypes, fn
      %Type.TApp{func: %Type.TApp{func: %Type.TCon{name: "Tuple"}}} -> "Tuple"
      %Type.TCon{name: n} -> n
      %Type.TApp{func: %Type.TCon{name: "List"}} -> "List"
      _ -> "Var"
    end)
    dict_name = Enum.join(["dict", class_name | type_suffixes], "_") |> String.to_atom()

    # Instance constraints as arguments (dictionaries)
    # We wrap them in a dummy constrained type to use get_dict_names
    dummy_type = %AST.TypeConstrained{constraints: constraints, type: %AST.TypeConstructor{name: "Unit"}}
    dict_names = get_dict_names(dummy_type)
    erl_args = Enum.map(dict_names, fn n -> {:var, 1, String.to_atom(n)} end)
    local_env = Enum.reduce(dict_names, MapSet.new(), &MapSet.put(&2, &1))

    # Map fields
    map_fields = Enum.map(members, fn %AST.DeclValue{name: m_name, expr: m_expr} ->
      m_atom = {:atom, 1, String.to_atom(m_name)}
      # m_expr is usually a variable or lambda.
      # We need to generate Erlang expr for it.
      erl_val = generate_expr(m_expr, local_env, env, current_mod)
      {:map_field_assoc, 1, m_atom, erl_val}
    end)

    erl_map = {:map, 1, map_fields}
    clause = {:clause, 1, erl_args, [], [erl_map]}
    {:function, 1, dict_name, length(erl_args), [clause]}
  end

  # Ignore types for code generation right now (they are erased at runtime)
  defp generate_decl(_, _, _, _), do: nil

  defp type_arity(%Type.TApp{func: %Type.TApp{func: %Type.TCon{name: "->"}}, arg: c}), do: 1 + type_arity(c)
  defp type_arity(%AST.TypeArrow{codomain: c}), do: 1 + type_arity(c)
  defp type_arity(%AST.TypeForall{type: t}), do: type_arity(t)
  defp type_arity(%AST.TypeConstrained{constraints: c, type: t}), do: length(c) + type_arity(t)
  defp type_arity(%Type.Forall{type: t}), do: type_arity(t)
  defp type_arity(%Type.TConstrained{type: t}), do: 1 + type_arity(t)
  defp type_arity(_), do: 0

  # Helper to get arity and module from Env
  defp get_info(name, env) do
    real_name = Phi.Typechecker.Env.resolve_term_alias(env, name)
    case Env.lookup(env, real_name) do
      {:ok, {mod, scheme}} -> {mod, scheme, real_name}
      :error -> {nil, nil, real_name}
    end
  end

  defp split_arity(scheme) do
    do_split_arity(scheme, 0, 0)
  end
  defp do_split_arity(%AST.TypeForall{type: t}, c, a), do: do_split_arity(t, c, a)
  defp do_split_arity(%AST.TypeConstrained{constraints: cs, type: t}, c, a), do: do_split_arity(t, c + length(cs), a)
  defp do_split_arity(%AST.TypeArrow{codomain: res}, c, a), do: do_split_arity(res, c, a + 1)
  defp do_split_arity(%Type.Forall{type: t}, c, a), do: do_split_arity(t, c, a)
  defp do_split_arity(%Type.TConstrained{type: t}, c, a), do: do_split_arity(t, c + 1, a)
  defp do_split_arity(%Type.TApp{func: %Type.TApp{func: %Type.TCon{name: "->"}}, arg: res}, c, a), do: do_split_arity(res, c, a + 1)
  defp do_split_arity(_, c, a), do: {c, a}

  defp get_dict_names(scheme) do
    do_get_dict_names(scheme, [])
  end
  defp do_get_dict_names(%AST.TypeForall{type: t}, acc), do: do_get_dict_names(t, acc)
  defp do_get_dict_names(%AST.TypeConstrained{constraints: cs, type: t}, acc) do
    names = Enum.map(cs, fn c ->
       case flatten_type_app(c, []) do
         [%AST.TypeConstructor{name: cn} | args] ->
           suffix = Enum.map(args, fn
             %AST.TypeVar{name: n} -> n
             %AST.TypeConstructor{name: n} -> n
             _ -> "Var"
           end) |> Enum.join("_")
           "Dict_#{cn}_#{suffix}"
         _ -> "Dict"
       end
    end)
    do_get_dict_names(t, acc ++ names)
  end
  defp do_get_dict_names(%AST.TypeArrow{codomain: res}, acc), do: do_get_dict_names(res, acc)
  defp do_get_dict_names(%Type.Forall{type: t}, acc), do: do_get_dict_names(t, acc)
  defp do_get_dict_names(%Type.TConstrained{class_name: cn, args: args, type: t}, acc) do
    suffix = Enum.map(args, fn
      %Type.TVar{id: id} -> "#{id}"
      %Type.TCon{name: n} -> n
      _ -> "Var"
    end) |> Enum.join("_")
    do_get_dict_names(t, acc ++ ["Dict_#{cn}_#{suffix}"])
  end
  defp do_get_dict_names(%Type.TApp{func: %Type.TApp{func: %Type.TCon{name: "->"}}, arg: res}, acc), do: do_get_dict_names(res, acc)
  defp do_get_dict_names(_, acc), do: acc

  defp flatten_type_app(%AST.TypeApp{func: f, arg: a}, acc), do: flatten_type_app(f, [a | acc])
  defp flatten_type_app(t, acc), do: [t | acc]

  defp extract_args(%AST.ExprLam{binder: %AST.BinderVar{name: arg_name}, body: body}, env, acc_args) do
    erl_arg = {:var, 1, String.capitalize(arg_name) |> String.to_atom()}
    extract_args(body, MapSet.put(env, arg_name), acc_args ++ [erl_arg])
  end
  defp extract_args(%AST.ExprLam{binder: %AST.BinderConstructor{name: c_name, args: c_args}, body: body}, env, acc_args) do
    arg_vars = Enum.map(c_args, fn %AST.BinderVar{name: arg_name} ->
      {:var, 1, String.capitalize(arg_name) |> String.to_atom()}
    end)
    env2 = Enum.reduce(c_args, env, fn %AST.BinderVar{name: arg_name}, acc ->
      MapSet.put(acc, arg_name)
    end)
    erl_tuple_pattern = {:tuple, 1, [{:atom, 1, String.to_atom(c_name)} | arg_vars]}
    extract_args(body, env2, acc_args ++ [erl_tuple_pattern])
  end
  defp extract_args(expr, env, acc_args) do
    case try_detect_ffi(expr) do
      {:ok, n, _f, _args} when n > 0 ->
        new_names = Enum.map(1..n, fn i -> "eta#{i}" end)
        new_erl_vars = Enum.map(new_names, fn n -> {:var, 1, String.capitalize(n) |> String.to_atom()} end)
        final_expr = Enum.reduce(new_names, expr, fn name, acc ->
          %AST.ExprApp{func: acc, arg: %AST.ExprVar{name: name}}
        end)
        {acc_args ++ new_erl_vars, final_expr, Enum.reduce(new_names, env, &MapSet.put(&2, &1))}
      _ ->
        {acc_args, expr, env}
    end
  end

  defp try_detect_ffi(%AST.ExprApp{func: f, arg: _a}) do
    case f do
      %AST.ExprApp{func: %AST.ExprVar{name: "ffi" <> n_str}, arg: _mod} ->
        {n, ""} = Integer.parse(n_str)
        {:ok, n, f, []}
      _ -> try_detect_ffi(f)
    end
  end
  defp try_detect_ffi(_), do: :error

  defp generate_expr(expr, local_env, global_env, current_mod)

  defp generate_expr(%AST.ExprVar{name: "num_" <> num_str}, _local_env, _global_env, _current_mod) do
    {num, ""} = Integer.parse(num_str)
    {:integer, 1, num}
  end
  defp generate_expr(%AST.ExprVar{name: "true"}, _local_env, _global_env, _current_mod), do: {:atom, 1, :true}
  defp generate_expr(%AST.ExprVar{name: "false"}, _local_env, _global_env, _current_mod), do: {:atom, 1, :false}
  defp generate_expr(%AST.ExprVar{name: "unit"}, _local_env, _global_env, _current_mod), do: {:atom, 1, :unit}
  defp generate_expr(%AST.ExprVar{name: "literal"}, _local_env, _global_env, _current_mod), do: {:atom, 1, :literal}

  defp generate_expr(%AST.ExprVar{name: name}, local_env, global_env, current_mod) do
    if MapSet.member?(local_env, name) do
      erl_var = String.capitalize(name) |> String.to_atom()
      {:var, 1, erl_var}
    else
      # Special cases for literals/builtins handled as vars in AST
      case name do
        "literal" -> {:atom, 1, :todo_literal}
        "unit" -> {:atom, 1, :unit}
        _ ->
          {mod, scheme, real_name} = get_info(name, global_env)
          {num_dicts, num_args} = if scheme, do: split_arity(scheme), else: {0, 0}
          total_arity = num_dicts + num_args

          if num_dicts > 0 do
             # Constrained function or member.
             class_name = global_env.member_to_class[real_name]
             dict_arg_name = find_dictionary(class_name, local_env)

             if class_name do
               # Class member accessor pattern
               if dict_arg_name do
                 dict_expr = {:var, 1, String.to_atom(dict_arg_name)}
                 if mod && mod != current_mod do
                   {:call, 1, {:remote, 1, {:atom, 1, mod}, {:atom, 1, String.to_atom(real_name)}}, [dict_expr]}
                 else
                   {:call, 1, {:atom, 1, String.to_atom(real_name)}, [dict_expr]}
                 end
               else
                 # No dict in scope, return function reference
                 generate_static_call(real_name, mod, 1, [], current_mod)
               end
             else
               # Not a class member but has constraints
               if dict_arg_name do
                  dict_expr = {:var, 1, String.to_atom(dict_arg_name)}
                  generate_static_call(real_name, mod, total_arity, [dict_expr], current_mod)
               else
                  generate_static_call(real_name, mod, total_arity, [], current_mod)
               end
             end
          else
            if total_arity == 0 do
              # Constant or variable
              if mod && mod != current_mod do
                {:remote, 1, {:atom, 1, mod}, {:atom, 1, String.to_atom(real_name)}}
              else
                {:atom, 1, String.to_atom(real_name)}
              end
            else
              # Partial application (0 args provided)
              generate_static_call(real_name, mod, total_arity, [], current_mod)
            end
          end
      end
    end
  end

  defp generate_expr(%AST.ExprLam{binder: binder, body: body}, local_env, global_env, current_mod) do
    erl_pat = generate_pattern(binder)
    bound_vars = find_bound_vars(binder, MapSet.new())
    local_env2 = MapSet.union(local_env, bound_vars)
    erl_body = generate_expr(body, local_env2, global_env, current_mod)
    clause = {:clause, 1, [erl_pat], [], [erl_body]}
    {:fun, 1, {:clauses, [clause]}}
  end

  defp generate_expr(%AST.ExprApp{} = app, local_env, global_env, current_mod) do
    {f, args} = flatten_app(app, [])
    erl_args = Enum.map(args, &generate_expr(&1, local_env, global_env, current_mod))

    case f do
      %AST.ExprVar{name: name} ->
        if MapSet.member?(local_env, name) do
          erl_f = {:var, 1, String.capitalize(name) |> String.to_atom()}
          Enum.reduce(erl_args, erl_f, fn arg, acc -> {:call, 1, acc, [arg]} end)
        else
          {mod, scheme, real_name} = get_info(name, global_env)
          {num_dicts, num_args} = if scheme, do: split_arity(scheme), else: {0, 0}
          total_arity = num_dicts + num_args

          if num_dicts > 0 do
             class_name = global_env.member_to_class[real_name]
             dict_arg_name = find_dictionary(class_name, local_env)

             if class_name do
               # Class member: real_name is an accessor (arity 1).
               # Generate (accessor(Dict))(arg1)(arg2)...
               accessor_call = if dict_arg_name do
                 dict_expr = {:var, 1, String.to_atom(dict_arg_name)}
                 if mod && mod != current_mod do
                   {:call, 1, {:remote, 1, {:atom, 1, mod}, {:atom, 1, String.to_atom(real_name)}}, [dict_expr]}
                 else
                   {:call, 1, {:atom, 1, String.to_atom(real_name)}, [dict_expr]}
                 end
               else
                 # No dict in scope, just call the accessor without dict (will likely fail at runtime)
                 if mod && mod != current_mod do
                   {:remote, 1, {:atom, 1, mod}, {:atom, 1, String.to_atom(real_name)}}
                 else
                   {:atom, 1, String.to_atom(real_name)}
                 end
               end
               # Apply remaining args via currying
               Enum.reduce(erl_args, accessor_call, fn arg, acc -> {:call, 1, acc, [arg]} end)
             else
               # Not a class member, but still has dict constraints
               final_args = if dict_arg_name do
                  [{:var, 1, String.to_atom(dict_arg_name)} | erl_args]
               else
                  erl_args
               end
               generate_static_call(real_name, mod, total_arity, final_args, current_mod)
             end
          else
             generate_static_call(real_name, mod, total_arity, erl_args, current_mod)
          end
        end
      other ->
        erl_f = generate_expr(other, local_env, global_env, current_mod)
        Enum.reduce(erl_args, erl_f, fn arg, acc -> {:call, 1, acc, [arg]} end)
    end
  end

  defp generate_expr(%AST.ExprLet{bindings: [%AST.DeclValue{name: name, expr: val_expr}], body: body}, local_env, global_env, current_mod) do
    erl_var = {:var, 1, String.capitalize(name) |> String.to_atom()}
    erl_val = generate_expr(val_expr, local_env, global_env, current_mod)
    local_env2 = MapSet.put(local_env, name)
    erl_body = generate_expr(body, local_env2, global_env, current_mod)
    match_expr = {:match, 1, erl_var, erl_val}
    {:block, 1, [match_expr, erl_body]}
  end

  defp generate_expr(%AST.ExprCase{exprs: exprs, branches: branches}, local_env, global_env, current_mod) do
    erl_targets = Enum.map(exprs, &generate_expr(&1, local_env, global_env, current_mod))
    erl_target = case erl_targets do
      [single] -> single
      multiple -> {:tuple, 1, multiple}
    end
    erl_clauses = Enum.map(branches, fn {patterns, body} ->
      erl_pats = Enum.map(patterns, &generate_pattern/1)
      erl_pat = case erl_pats do
        [single_p] -> single_p
        multiple_p -> {:tuple, 1, multiple_p}
      end
      new_vars = Enum.reduce(patterns, MapSet.new(), &find_bound_vars/2)
      local_env2 = MapSet.union(local_env, new_vars)
      erl_body = generate_expr(body, local_env2, global_env, current_mod)
      {:clause, 1, [erl_pat], [], [erl_body]}
    end)
    {:case, 1, erl_target, erl_clauses}
  end

  defp generate_expr(%AST.ExprTuple{elems: elems}, local_env, global_env, current_mod) do
    erl_elems = Enum.map(elems, &generate_expr(&1, local_env, global_env, current_mod))
    {:tuple, 1, erl_elems}
  end

  defp generate_expr(%AST.ExprList{elems: elems, tail: t}, local_env, global_env, current_mod) do
    erl_elems = Enum.map(elems, &generate_expr(&1, local_env, global_env, current_mod))
    erl_tail = case t do
      nil -> {nil, 1}
      other -> generate_expr(other, local_env, global_env, current_mod)
    end
    Enum.reduce(Enum.reverse(erl_elems), erl_tail, fn elem, acc ->
      {:cons, 1, elem, acc}
    end)
  end

  defp generate_expr(%AST.ExprIf{cond: c, then_br: t, else_br: e}, local_env, global_env, current_mod) do
    {:if, 1, [
      {:clause, 1, [], [[generate_expr(c, local_env, global_env, current_mod)]], [generate_expr(t, local_env, global_env, current_mod)]},
      {:clause, 1, [], [[{:atom, 1, :true}]], [generate_expr(e, local_env, global_env, current_mod)]}
    ]}
  end

  defp generate_expr(expr, _, _, _) do
    raise "Unsupported expression for codegen: #{inspect(expr)}"
  end

  defp generate_pattern(%AST.BinderVar{name: "_"}), do: {:var, 1, :_}
  defp generate_pattern(%AST.BinderVar{name: "true"}), do: {:atom, 1, :true}
  defp generate_pattern(%AST.BinderVar{name: "false"}), do: {:atom, 1, :false}
  defp generate_pattern(%AST.BinderVar{name: name}), do: {:var, 1, String.capitalize(name) |> String.to_atom()}
  defp generate_pattern(%AST.BinderConstructor{name: "true", args: []}), do: {:atom, 1, :true}
  defp generate_pattern(%AST.BinderConstructor{name: "false", args: []}), do: {:atom, 1, :false}
  defp generate_pattern(%AST.BinderConstructor{name: c_name, args: args}) do
    erl_pats = Enum.map(args, &generate_pattern/1)
    {:tuple, 1, [{:atom, 1, String.to_atom(c_name)} | erl_pats]}
  end
  defp generate_pattern(%AST.BinderList{head: h, tail: t}) do
    {:cons, 1, generate_pattern(h), generate_pattern(t)}
  end
  defp generate_pattern(%AST.BinderTuple{elems: elems}) do
    {:tuple, 1, Enum.map(elems, &generate_pattern/1)}
  end
  defp generate_pattern(other), do: raise "Unsupported pattern for codegen: #{inspect(other)}"

  defp find_bound_vars(%AST.BinderVar{name: "_"}, acc), do: acc
  defp find_bound_vars(%AST.BinderVar{name: name}, acc), do: MapSet.put(acc, name)
  defp find_bound_vars(%AST.BinderConstructor{args: args}, acc) do
    Enum.reduce(args, acc, &find_bound_vars/2)
  end
  defp find_bound_vars(_, acc), do: acc

  defp flatten_app(%AST.ExprApp{func: f, arg: a}, acc_args) do
    flatten_app(f, [a | acc_args])
  end
  defp flatten_app(f, acc_args) do
    {f, acc_args}
  end

  defp find_dictionary(nil, _), do: nil
  defp find_dictionary(class_name, local_env) do
    prefix = "Dict_#{class_name}_"
    Enum.find(local_env, fn name -> String.starts_with?(name, prefix) end)
  end

  defp generate_static_call(name, mod, 0, args, current_mod) do
    f_atom = String.to_atom(name)
    base_call = if mod && mod != current_mod && mod != :local do
       {:call, 1, {:remote, 1, {:atom, 1, mod}, {:atom, 1, f_atom}}, []}
    else
       {:call, 1, {:atom, 1, f_atom}, []}
    end
    Enum.reduce(args, base_call, fn arg, acc -> {:call, 1, acc, [arg]} end)
  end
  defp generate_static_call(name, mod, arity, args, current_mod) do
    num_provided = length(args)
    f_atom = String.to_atom(name)
    call_target = if mod && mod != current_mod && mod != :local do
       {:remote, 1, {:atom, 1, mod}, {:atom, 1, f_atom}}
    else
       {:atom, 1, f_atom}
    end

    cond do
      num_provided == arity ->
        {:call, 1, call_target, args}
      num_provided < arity ->
        needed = arity - num_provided
        p_args = Enum.map(1..needed, fn i -> {:var, 1, String.to_atom("P#{i}")} end)
        all_args = args ++ p_args
        body = {:call, 1, call_target, all_args}
        clause = {:clause, 1, p_args, [], [body]}
        {:fun, 1, {:clauses, [clause]}}
      num_provided > arity ->
        {arity_args, extra_args} = Enum.split(args, arity)
        base_call = {:call, 1, call_target, arity_args}
        Enum.reduce(extra_args, base_call, fn arg, acc -> {:call, 1, acc, [arg]} end)
    end
  end

  defp generate_partial_call(name, mod, arity, current_mod) do
    args = Enum.map(1..arity, fn i -> {:var, 1, String.to_atom("P#{i}")} end)

    call_target = if mod && mod != current_mod && mod != :local do
      {:remote, 1, {:atom, 1, mod}, {:atom, 1, String.to_atom(name)}}
    else
      {:atom, 1, String.to_atom(name)}
    end

    _clause = {:clause, 1, args, [], [{:call, 1, call_target, args}]}
    if mod && mod != current_mod && mod != :local do
       {:fun, 1, {:function, {:atom, 1, mod}, {:atom, 1, String.to_atom(name)}, {:integer, 1, arity}}}
    else
       {:fun, 1, {:function, String.to_atom(name), arity}}
    end
  end

end
