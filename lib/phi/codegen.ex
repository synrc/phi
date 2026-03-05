defmodule Phi.Codegen do
  @moduledoc """
  Translates Phi.AST into Erlang Abstract Format (Erlang AST)
  so that it can be compiled natively on the BEAM via `:compile.forms/2`.
  """
  alias Phi.AST
  alias Phi.Typechecker.Env
  alias Phi.Type

  @doc """
  Generates Erlang forms for a module.
  Expects a desugared AST and a global typing environment.
  """
  def generate(%AST.Module{name: mod_name, declarations: decls}, env, opts \\ []) do
    current_mod = mod_name |> String.to_atom()
    foreign_mod = Keyword.get(opts, :foreign_mod, current_mod)

    try do
      functions =
        Enum.flat_map(decls, fn decl ->
          case generate_decl(decl, current_mod, foreign_mod, env) do
            nil -> []
            {:multi, list} -> list
            func -> [func]
          end
        end)

      module_attr = {:attribute, 1, :module, current_mod}
      compile_attr = {:attribute, 1, :compile, :no_auto_import}
      export_all = {:attribute, 1, :compile, :export_all}

      forms = [module_attr, compile_attr, export_all] ++ functions ++ [{:eof, 1}]
      {:ok, forms}
    catch
      {:unresolved_any_name, name} -> {:error, {:unresolved, name}}
    end
  end

  defp generate_decl(
         %AST.DeclValue{name: name, binders: binders, expr: expr},
         current_mod,
         _foreign_mod,
         env
       ) do
    func_name = String.to_atom(name)
    {_mod, scheme, _real_name, _base_name} = get_info(name, env)
    {_num_dicts, num_args} = if scheme, do: split_arity(scheme), else: {0, 0}

    erl_binders = Enum.map(binders || [], &generate_pattern/1)
    bound_from_binders = Enum.reduce(binders || [], MapSet.new(), &find_bound_vars/2)
    {args, body_expr, env_func} = extract_args(expr, bound_from_binders, [])
    acc_args = erl_binders ++ args

    dict_names = get_dict_names(scheme)
    dict_vars = Enum.map(dict_names, fn n -> {:var, 1, String.to_atom(n)} end)

    {final_args, final_body, final_env} =
      if length(acc_args) < num_args do
        needed = num_args - length(acc_args)
        new_names = Enum.map(1..needed, fn i -> "eta_#{i}" end)

        new_erl_vars =
          Enum.map(new_names, fn n -> {:var, 1, String.capitalize(n) |> String.to_atom()} end)

        expanded_expr =
          Enum.reduce(new_names, body_expr, fn n, acc ->
            %AST.ExprApp{func: acc, arg: %AST.ExprVar{name: n}}
          end)

        {dict_vars ++ acc_args ++ new_erl_vars, expanded_expr,
         Enum.reduce(dict_names ++ new_names, env_func, &MapSet.put(&2, &1))}
      else
        {dict_vars ++ acc_args, body_expr, Enum.reduce(dict_names, env_func, &MapSet.put(&2, &1))}
      end

    erl_body = generate_expr(final_body, final_env, env, current_mod)
    clause = {:clause, 1, final_args, [], [erl_body]}
    {:function, 1, func_name, length(final_args), [clause]}
  end

  defp generate_decl(%AST.DeclForeign{name: name, type: type}, _current_mod, foreign_mod, _env) do
    func_name = String.to_atom(name)
    arity = type_arity(type)

    args =
      if arity > 0,
        do: Enum.map(1..arity, fn i -> {:var, 1, String.to_atom("A#{i}")} end),
        else: []

    call = {:call, 1, {:remote, 1, {:atom, 1, foreign_mod}, {:atom, 1, func_name}}, args}
    {:function, 1, func_name, arity, [{:clause, 1, args, [], [call]}]}
  end

  defp generate_decl(%AST.DeclData{constructors: ctors}, _, _, _) do
    functions =
      Enum.flat_map(ctors, fn {c_name, c_args} ->
        f_atom = String.to_atom(c_name)

        case c_args do
          [%AST.TypeRecord{fields: fields}] ->
            # Record constructor takes 1 arg (the map), accessor functions use maps:get.
            # Generated accessor handles both {Ctor, Map} tuples and plain maps so that
            # pattern-matched inner records (which are plain maps) work correctly.
            constructor_fn =
              {:function, 1, f_atom, 1,
               [
                 {:clause, 1, [{:var, 1, :X1}], [],
                  [{:tuple, 1, [{:atom, 1, f_atom}, {:var, 1, :X1}]}]}
               ]}

            accessor_fns =
              Enum.map(fields, fn field_name ->
                acc_atom = String.to_atom("access_#{field_name}")
                arg_var = {:var, 1, :R}
                field_atom = {:atom, 1, String.to_atom(field_name)}

                is_tuple_guard =
                  {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_tuple}}, [arg_var]}

                is_map_guard =
                  {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_map}}, [arg_var]}

                inner_map =
                  {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :element}},
                   [{:integer, 1, 2}, arg_var]}

                body_tuple =
                  {:call, 1, {:remote, 1, {:atom, 1, :maps}, {:atom, 1, :get}},
                   [field_atom, inner_map]}

                body_map =
                  {:call, 1, {:remote, 1, {:atom, 1, :maps}, {:atom, 1, :get}},
                   [field_atom, arg_var]}

                {:function, 1, acc_atom, 1,
                 [
                   {:clause, 1, [arg_var], [[is_tuple_guard]], [body_tuple]},
                   {:clause, 1, [arg_var], [[is_map_guard]], [body_map]}
                 ]}
              end)

            [constructor_fn | accessor_fns]

          _ ->
            arity = length(c_args)

            args =
              if arity > 0,
                do: Enum.map(1..arity, fn i -> {:var, 1, String.to_atom("X#{i}")} end),
                else: []

            [
              {:function, 1, f_atom, arity,
               [{:clause, 1, args, [], [{:tuple, 1, [{:atom, 1, f_atom} | args]}]}]}
            ]
        end
      end)

    {:multi, functions}
  end

  defp generate_decl(%AST.DeclNewtype{constructor: ctor_name}, _, _, _) do
    f_atom = String.to_atom(ctor_name)
    # Newtype constructor: wraps its argument as {ctor, arg}
    {:function, 1, f_atom, 1,
     [{:clause, 1, [{:var, 1, :X1}], [], [{:tuple, 1, [{:atom, 1, f_atom}, {:var, 1, :X1}]}]}]}
  end

  defp generate_decl(%AST.DeclClass{members: members}, _, _, _) do
    functions =
      Enum.map(members, fn %AST.DeclTypeSignature{name: m_name} ->
        f_atom = String.to_atom(m_name)
        dict_var = {:var, 1, :Dict}

        {:function, 1, f_atom, 1,
         [
           {:clause, 1, [dict_var], [],
            [
              {:call, 1, {:remote, 1, {:atom, 1, :maps}, {:atom, 1, :get}},
               [{:atom, 1, f_atom}, dict_var]}
            ]}
         ]}
      end)

    {:multi, functions}
  end

  defp generate_decl(
         %AST.DeclInstance{class: class_name, types: types, members: members, constraints: cs},
         current_mod,
         _,
         env
       ) do
    itypes = Enum.map(types, &Phi.Typechecker.ast_to_type(&1, env))

    type_suffixes =
      Enum.map(itypes, fn
        %Type.TApp{func: %Type.TApp{func: %Type.TCon{name: "Tuple"}}} -> "Tuple"
        %Type.TApp{func: %Type.TApp{func: %Type.TCon{name: "->"}}} -> "Fun"
        %Type.TApp{func: %Type.TCon{name: n}} -> n
        %Type.TCon{name: name} -> name
        %Type.TVar{id: id} -> "v#{id}"
        _ -> "Any"
      end)

    dict_name = String.to_atom("dict_#{class_name}_#{Enum.join(type_suffixes, "_")}")

    # Handle constraints: instance Eq a => Eq (Maybe a)
    dict_args =
      if cs do
        Enum.map(cs, fn
          %AST.TypeApp{func: %AST.TypeConstructor{name: cn}, arg: %AST.TypeVar{name: vn}} ->
            "Dict_#{cn}_#{vn}"

          _ ->
            "Dict_K"
        end)
      else
        []
      end

    erl_args = Enum.map(dict_args, fn n -> {:var, 1, String.to_atom(n)} end)
    local_env = MapSet.new(dict_args)

    map_fields =
      Enum.map(members, fn %AST.DeclValue{name: m_name, expr: m_expr} ->
        erl_val = generate_expr(m_expr, local_env, env, current_mod)
        {:map_field_assoc, 1, {:atom, 1, String.to_atom(m_name)}, erl_val}
      end)

    {:function, 1, dict_name, length(erl_args),
     [{:clause, 1, erl_args, [], [{:map, 1, map_fields}]}]}
  end

  defp generate_decl(_, _, _, _), do: nil

  defp type_arity(%Type.TApp{func: %Type.TApp{func: %Type.TCon{name: "->"}}, arg: c}),
    do: 1 + type_arity(c)

  defp type_arity(%AST.TypeArrow{codomain: c}), do: 1 + type_arity(c)
  defp type_arity(%AST.TypeForall{type: t}), do: type_arity(t)
  defp type_arity(%AST.TypeConstrained{constraints: cs, type: t}), do: length(cs) + type_arity(t)
  defp type_arity(%Type.Forall{type: t}), do: type_arity(t)
  defp type_arity(%Type.TConstrained{type: t}), do: 1 + type_arity(t)
  defp type_arity(_), do: 0

  defp split_arity(%Type.Forall{type: t}), do: do_split_arity(t, 0, 0)
  defp split_arity(t), do: do_split_arity(t, 0, 0)

  defp do_split_arity(%Type.Forall{type: t}, c, a), do: do_split_arity(t, c, a)
  defp do_split_arity(%Type.TConstrained{type: t}, c, a), do: do_split_arity(t, c + 1, a)

  defp do_split_arity(%Type.TApp{func: %Type.TApp{func: %Type.TCon{name: "->"}}, arg: res}, c, a),
    do: do_split_arity(res, c, a + 1)

  defp do_split_arity(_, c, a), do: {c, a}

  defp extract_class_name(%Type.Forall{type: t}), do: extract_class_name(t)
  defp extract_class_name(%Type.TConstrained{class_name: cn}), do: cn

  defp extract_class_name(_t) do
    # IO.puts("Failed to extract class_name from: \#{inspect(t)}")
    nil
  end

  defp get_dispatch_index(scheme) do
    do_get_dispatch_index(scheme) || 0
  end

  defp do_get_dispatch_index(%Type.Forall{type: t}), do: do_get_dispatch_index(t)

  defp do_get_dispatch_index(%Type.TConstrained{args: args, type: t}) do
    var_name =
      case args do
        [%Type.TVar{id: v} | _] -> v
        _ -> nil
      end

    if var_name, do: find_var_index_in_spine(t, var_name, 0), else: 0
  end

  defp do_get_dispatch_index(_), do: 0

  defp find_var_index_in_spine(
         %Type.TApp{func: %Type.TApp{func: %Type.TCon{name: "->"}, arg: arg_t}, arg: ret_t},
         var_name,
         idx
       ) do
    if contains_var?(arg_t, var_name) do
      idx
    else
      find_var_index_in_spine(ret_t, var_name, idx + 1)
    end
  end

  defp find_var_index_in_spine(%Type.Forall{type: t}, var_name, idx),
    do: find_var_index_in_spine(t, var_name, idx)

  defp find_var_index_in_spine(%Type.TConstrained{type: t}, var_name, idx),
    do: find_var_index_in_spine(t, var_name, idx)

  defp find_var_index_in_spine(_, _, _), do: nil

  defp contains_var?(%Type.TVar{id: v}, var_name), do: v == var_name

  defp contains_var?(%Type.TApp{func: f, arg: a}, var_name),
    do: contains_var?(f, var_name) or contains_var?(a, var_name)

  defp contains_var?(_, _), do: false

  defp get_dict_names(scheme) do
    do_get_dict_names(scheme, [])
  end

  defp do_get_dict_names(%AST.TypeForall{type: t}, acc), do: do_get_dict_names(t, acc)

  defp do_get_dict_names(%AST.TypeConstrained{constraints: cs, type: t}, acc) do
    names =
      Enum.map(cs, fn
        %AST.TypeApp{func: %AST.TypeConstructor{name: cn}, arg: %AST.TypeVar{name: vn}} ->
          "Dict_#{cn}_#{vn}"

        _ ->
          "Dict_K"
      end)

    do_get_dict_names(t, acc ++ names)
  end

  defp do_get_dict_names(%Type.Forall{type: t}, acc), do: do_get_dict_names(t, acc)

  defp do_get_dict_names(%Type.TConstrained{class_name: cn, type: t}, acc) do
    do_get_dict_names(t, acc ++ ["Dict_#{cn}_X"])
  end

  defp do_get_dict_names(_, acc), do: acc

  defp extract_args(%AST.ExprLam{binder: binder, body: body}, env, acc_args) do
    erl_pat = generate_pattern(binder)
    new_vars = find_bound_vars(binder, MapSet.new())
    extract_args(body, MapSet.union(env, new_vars), acc_args ++ [erl_pat])
  end

  defp extract_args(expr, env, acc_args), do: {acc_args, expr, env}

  defp get_info(name, env) do
    # Prefer direct unqualified binding over term_alias redirect (local definitions win)
    {real_name, result} =
      case Env.lookup(env, name) do
        {:ok, _} = ok ->
          {name, ok}

        :error ->
          aliased = Env.resolve_term_alias(env, name)
          {aliased, Env.lookup(env, aliased)}
      end

    case result do
      {:ok, {mod, scheme}} ->
        base_name = real_name |> String.split(".") |> List.last()
        {mod, scheme, real_name, base_name}

      :error ->
        if constructor_name?(real_name) do
          base_name = real_name |> String.split(".") |> List.last()
          {nil, nil, real_name, base_name}
        else
          if String.contains?(real_name, ".") do
            parts = String.split(real_name, ".")
            {mod_parts, [base_name]} = Enum.split(parts, length(parts) - 1)
            mod = Enum.join(mod_parts, ".") |> String.to_atom()
            {mod, nil, real_name, base_name}
          else
            throw({:unresolved_any_name, real_name})
          end
        end
    end
  end

  defp flatten_lam(%AST.ExprLam{binder: binder, body: body}, acc_pats, acc_env) do
    erl_pat = generate_pattern(binder)
    bound_vars = find_bound_vars(binder, MapSet.new())
    new_env = MapSet.union(acc_env, bound_vars)

    case body do
      %AST.ExprLam{} -> flatten_lam(body, acc_pats ++ [erl_pat], new_env)
      _ -> {acc_pats ++ [erl_pat], body, new_env}
    end
  end

  defp generate_expr(expr, local_env, global_env, current_mod, name \\ nil, fix_env \\ %{})

  defp generate_expr(
         %AST.ExprRecord{fields: fields},
         local_env,
         global_env,
         current_mod,
         name,
         fix_env
       ) do
    erl_fields =
      Enum.map(fields, fn {field_name, val_expr} ->
        {:map_field_assoc, 1, {:atom, 1, String.to_atom(field_name)},
         generate_expr(val_expr, local_env, global_env, current_mod, name, fix_env)}
      end)

    {:map, 1, erl_fields}
  end

  defp generate_expr(
         %AST.ExprRecordUpdate{base: base, fields: fields},
         local_env,
         global_env,
         current_mod,
         name,
         fix_env
       ) do
    erl_base = generate_expr(base, local_env, global_env, current_mod, name, fix_env)

    erl_update_fields =
      Enum.map(fields, fn {field_name, val_expr} ->
        {:map_field_assoc, 1, {:atom, 1, String.to_atom(field_name)},
         generate_expr(val_expr, local_env, global_env, current_mod, name, fix_env)}
      end)

    erl_update_map = {:map, 1, erl_update_fields}
    {:call, 1, {:remote, 1, {:atom, 1, :maps}, {:atom, 1, :merge}}, [erl_base, erl_update_map]}
  end

  defp generate_expr(%AST.ExprAtom{value: v}, _, _, _, _, _), do: {:atom, 1, String.to_atom(v)}

  defp generate_expr(%AST.ExprBinary{value: values}, _, _, _, _, _) do
    bin_elements =
      Enum.map(values, fn
        {:number, val} ->
          {:bin_element, 1, {:integer, 1, val}, :default, :default}

        {:string, val} ->
          {:bin_element, 1, {:string, 1, String.to_charlist(val)}, :default, :default}
      end)

    {:bin, 1, bin_elements}
  end

  defp generate_expr(
         %AST.ExprReceive{clauses: clauses, after_clause: after_clause},
         local_env,
         global_env,
         current_mod,
         name,
         fix_env
       ) do
    # Generate receive clauses
    erl_clauses =
      Enum.map(clauses, fn {pattern, expr} ->
        pattern_erl = generate_pattern(pattern)
        clause_locals = MapSet.union(local_env, find_bound_vars(pattern, MapSet.new()))
        expr_erl = generate_expr(expr, clause_locals, global_env, current_mod, name, fix_env)
        {:clause, 1, [pattern_erl], [], [expr_erl]}
      end)

    # Generate after clause if present
    case after_clause do
      :none ->
        {:receive, 1, erl_clauses}

      {:ok, after_body} ->
        after_erl = generate_expr(after_body, local_env, global_env, current_mod, name, fix_env)
        {:receive, 1, erl_clauses, {:integer, 1, 0}, [after_erl]}

      {:ok, timeout_expr, after_body} ->
        timeout_erl =
          generate_expr(timeout_expr, local_env, global_env, current_mod, name, fix_env)

        after_erl = generate_expr(after_body, local_env, global_env, current_mod, name, fix_env)
        {:receive, 1, erl_clauses, timeout_erl, [after_erl]}
    end
  end

  defp generate_expr(%AST.ExprVar{name: "num_" <> ns}, _, _, _, _, _),
    do: {:integer, 1, String.to_integer(ns)}

  defp generate_expr(%AST.ExprVar{name: "float_" <> fs}, _, _, _, _, _),
    do: {:float, 1, String.to_float(fs)}

  defp generate_expr(%AST.ExprVar{name: "str_" <> s}, _, _, _, _, _),
    do: {:string, 1, String.to_charlist(s)}

  defp generate_expr(%AST.ExprVar{name: "char_" <> cs}, _, _, _, _, _),
    do: {:integer, 1, String.to_integer(cs)}

  defp generate_expr(%AST.ExprVar{name: "true"}, _, _, _, _, _), do: {:atom, 1, true}
  defp generate_expr(%AST.ExprVar{name: "false"}, _, _, _, _, _), do: {:atom, 1, false}
  defp generate_expr(%AST.ExprVar{name: "unit"}, _, _, _, _, _), do: {:atom, 1, :unit}
  # undefined is the Haskell bottom value — generate erlang:error(undefined)
  defp generate_expr(%AST.ExprVar{name: "undefined"}, _, _, _, _, _),
    do:
      {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :error}}, [{:atom, 1, :undefined}]}

  defp generate_expr(
         %AST.ExprDo{statements: statements},
         local_env,
         global_env,
         current_mod,
         name,
         fix_env
       ) do
    desugared = desugar_do(statements)
    generate_expr(desugared, local_env, global_env, current_mod, name, fix_env)
  end

  defp generate_expr(%AST.ExprVar{name: name}, local_env, global_env, current_mod, _name, fix_env) do
    if Map.has_key?(fix_env, name) do
      Map.get(fix_env, name)
    else
      if MapSet.member?(local_env, name) do
        {:var, 1, String.capitalize(name) |> String.to_atom()}
      else
        {mod, scheme, real_name, base_name} = get_info(name, global_env)

        if mod == nil and String.contains?(name, ".") do
          raise "Unresolved qualified name: #{name}"
        end

        if scheme == nil && constructor_name?(real_name) do
          {:tuple, 1, [{:atom, 1, String.to_atom(base_name)}]}
        else
          {num_dicts, num_args} = if scheme, do: split_arity(scheme), else: {0, 0}
          total_arity = num_dicts + num_args

          if total_arity == 0 and not constructor_name?(real_name) do
            if mod && mod != current_mod do
              # Call arity 0 if it's a top-level value
              {:call, 1, {:remote, 1, {:atom, 1, mod}, {:atom, 1, String.to_atom(base_name)}}, []}
            else
              {:atom, 1, String.to_atom(base_name)}
            end
          else
            if num_dicts > 0 do
              class_name = global_env.member_to_class[real_name] || extract_class_name(scheme)
              dict_arg_name = find_dictionary(class_name, local_env)

              if class_name do
                is_method = Map.has_key?(global_env.member_to_class, real_name)

                if dict_arg_name do
                  dict_expr = {:var, 1, String.to_atom(dict_arg_name)}

                  if is_method do
                    if mod && mod != current_mod do
                      {:call, 1,
                       {:remote, 1, {:atom, 1, mod}, {:atom, 1, String.to_atom(real_name)}},
                       [dict_expr]}
                    else
                      {:call, 1, {:atom, 1, String.to_atom(real_name)}, [dict_expr]}
                    end
                  else
                    generate_static_call(base_name, mod, total_arity, [dict_expr], current_mod)
                  end
                else
                  if is_method do
                    generate_static_call(real_name, mod, 1, [], current_mod)
                  else
                    generate_static_call(base_name, mod, total_arity, [], current_mod)
                  end
                end
              else
                if dict_arg_name do
                  generate_static_call(
                    real_name,
                    mod,
                    total_arity,
                    [{:var, 1, String.to_atom(dict_arg_name)}],
                    current_mod
                  )
                else
                  generate_static_call(real_name, mod, total_arity, [], current_mod)
                end
              end
            else
              if total_arity == 0 do
                # 0-arity constructors (e.g. Nothing, LT) are always tuples
                {:tuple, 1, [{:atom, 1, String.to_atom(real_name)}]}
              else
                generate_static_call(real_name, mod, total_arity, [], current_mod)
              end
            end
          end
        end
      end
    end
  end

  defp generate_expr(
         %AST.ExprLam{binder: binder, body: body} = lam,
         local_env,
         global_env,
         current_mod,
         name,
         fix_env
       ) do
    if name && MapSet.member?(find_used_vars(body, MapSet.new()), name) do
      # Recursive lambda: stay arity 1 for now or handle properly
      erl_pat = generate_pattern(binder)
      bound_vars = find_bound_vars(binder, MapSet.new())
      local_env2 = MapSet.union(MapSet.put(local_env, name), bound_vars)
      erl_body = generate_expr(body, local_env2, global_env, current_mod, nil, fix_env)
      clause = {:clause, 1, [erl_pat], [], [erl_body]}
      {:named_fun, 1, String.capitalize(name) |> String.to_atom(), [clause]}
    else
      {pats, final_body, final_env} = flatten_lam(lam, [], local_env)
      erl_body = generate_expr(final_body, final_env, global_env, current_mod, nil, fix_env)
      {:fun, 1, {:clauses, [{:clause, 1, pats, [], [erl_body]}]}}
    end
  end

  defp generate_expr(%AST.ExprApp{} = app, local_env, global_env, current_mod, _name, fix_env) do
    {f, args} = flatten_app(app, [])

    erl_args =
      Enum.map(args, &generate_expr(&1, local_env, global_env, current_mod, nil, fix_env))

    case f do
      %AST.ExprVar{name: name} ->
        # Emit native Erlang ops for known primitive operators — avoids typeclass
        # dispatch case expressions that cause Erlang compiler exponential blowup.
        native_op = native_erlang_op(name, global_env)

        if native_op && length(erl_args) == 2 do
          [a, b] = erl_args
          {:op, 1, native_op, a, b}
        else
          cond do
            Map.has_key?(fix_env, name) ->
              erl_f = Map.get(fix_env, name)
              Enum.reduce(erl_args, erl_f, fn arg, acc -> {:call, 1, acc, [arg]} end)

            MapSet.member?(local_env, name) ->
              erl_f = {:var, 1, String.capitalize(name) |> String.to_atom()}
              Enum.reduce(erl_args, erl_f, fn arg, acc -> {:call, 1, acc, [arg]} end)

            true ->
              {mod, scheme, real_name, base_name} = get_info(name, global_env)

              if scheme == nil && constructor_name?(real_name) do
                {:tuple, 1, [{:atom, 1, String.to_atom(base_name)} | erl_args]}
              else
                {num_dicts, num_args} = if scheme, do: split_arity(scheme), else: {0, 0}
                total_arity = num_dicts + num_args

                if num_dicts > 0 do
                  class_name = global_env.member_to_class[real_name] || extract_class_name(scheme)
                  dict_arg_name = find_dictionary(class_name, local_env)

                  if class_name do
                    is_method = Map.has_key?(global_env.member_to_class, real_name)

                    if dict_arg_name do
                      dict_expr = {:var, 1, String.to_atom(dict_arg_name)}

                      if is_method do
                        accessor_call =
                          if mod && mod != current_mod do
                            {:call, 1,
                             {:remote, 1, {:atom, 1, mod}, {:atom, 1, String.to_atom(base_name)}},
                             [dict_expr]}
                          else
                            {:call, 1, {:atom, 1, String.to_atom(base_name)}, [dict_expr]}
                          end

                        Enum.reduce(erl_args, accessor_call, fn arg, acc ->
                          {:call, 1, acc, [arg]}
                        end)
                      else
                        generate_static_call(
                          base_name,
                          mod,
                          total_arity,
                          [dict_expr | erl_args],
                          current_mod
                        )
                      end
                    else
                      if is_method do
                        generate_dispatch_call(
                          real_name,
                          mod,
                          erl_args,
                          class_name,
                          global_env,
                          current_mod
                        )
                      else
                        # Constrained non-method with no dict in scope.
                        # Preserve dict-first calling convention by wrapping:
                        #   fun(D1..Dn) -> f(D1..Dn, args...) end
                        dict_vars =
                          if class_name do
                            instances = Map.get(global_env.instances, class_name, [])

                            case Enum.find(instances, fn _ -> true end) do
                              %{dict_name: dn, mod: dict_mod}
                              when is_atom(dict_mod) and not is_nil(dict_mod) ->
                                [
                                  {:call, 1,
                                   {:remote, 1, {:atom, 1, dict_mod},
                                    {:atom, 1, String.to_atom(dn)}}, []}
                                ]

                              _ ->
                                Enum.map(1..num_dicts, fn _ ->
                                  {:atom, 1, :undefined_dict}
                                end)
                            end
                          else
                            Enum.map(1..num_dicts, fn _ -> {:atom, 1, :undefined_dict} end)
                          end

                        generate_static_call(
                          base_name,
                          mod,
                          total_arity,
                          dict_vars ++ erl_args,
                          current_mod
                        )
                      end
                    end
                  else
                    final_args =
                      if dict_arg_name,
                        do: [{:var, 1, String.to_atom(dict_arg_name)} | erl_args],
                        else: erl_args

                    generate_static_call(base_name, mod, total_arity, final_args, current_mod)
                  end
                else
                  # For unknown functions (scheme=nil), use actual arg count
                  effective_arity = if scheme == nil, do: length(erl_args), else: total_arity
                  generate_static_call(base_name, mod, effective_arity, erl_args, current_mod)
                end
              end
          end
        end

      other ->
        erl_f = generate_expr(other, local_env, global_env, current_mod, nil, fix_env)
        Enum.reduce(erl_args, erl_f, fn arg, acc -> {:call, 1, acc, [arg]} end)
    end
  end

  defp generate_expr(
         %AST.ExprLet{bindings: bs, body: body},
         local_env,
         global_env,
         current_mod,
         _name,
         fix_env
       ) do
    all_names =
      Enum.map(bs, fn
        %AST.DeclValue{name: n} -> n
        _ -> nil
      end)
      |> Enum.reject(&is_nil/1)

    env_with_names = Enum.reduce(all_names, local_env, &MapSet.put(&2, &1))
    sorted = sort_bindings(bs)

    {match_exprs, final_env} =
      Enum.reduce(sorted, {[], env_with_names}, fn
        %AST.DeclValue{name: "_pat", binders: [pat], expr: rhs}, {acc_s, acc_e} ->
          {:match, 1, generate_pattern(pat),
           generate_expr(rhs, acc_e, global_env, current_mod, nil, fix_env)}
          |> then(fn m ->
            {acc_s ++ [m], MapSet.union(acc_e, find_bound_vars(pat, MapSet.new()))}
          end)

        %AST.DeclValue{name: name, expr: rhs}, {acc_s, acc_e} ->
          used_in_rhs = find_used_vars(rhs, MapSet.new())

          if MapSet.member?(used_in_rhs, name) do
            # Self-referential binding: use process dictionary to avoid unbound var in Erlang
            ref_var_atom = (String.capitalize(name) <> "__FIX_REF") |> String.to_atom()
            n_atom = String.capitalize(name) |> String.to_atom()

            ref_assign =
              {:match, 1, {:var, 1, ref_var_atom},
               {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :make_ref}}, []}}

            get_expr =
              {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :get}},
               [{:var, 1, ref_var_atom}]}

            new_fix_env = Map.put(fix_env, name, get_expr)
            env_for_rhs = MapSet.delete(acc_e, name)
            erl_rhs = generate_expr(rhs, env_for_rhs, global_env, current_mod, name, new_fix_env)
            match = {:match, 1, {:var, 1, n_atom}, erl_rhs}

            put_call =
              {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :put}},
               [{:var, 1, ref_var_atom}, {:var, 1, n_atom}]}

            {acc_s ++ [ref_assign, match, put_call], acc_e}
          else
            match =
              {:match, 1, {:var, 1, String.capitalize(name) |> String.to_atom()},
               generate_expr(rhs, acc_e, global_env, current_mod, name, fix_env)}

            {acc_s ++ [match], acc_e}
          end

        _, acc ->
          acc
      end)

    erl_body = generate_expr(body, final_env, global_env, current_mod, nil, fix_env)
    {:block, 1, match_exprs ++ [erl_body]}
  end

  defp generate_expr(
         %AST.ExprCase{exprs: es, branches: bs},
         local_env,
         global_env,
         current_mod,
         _name,
         fix_env
       ) do
    erl_targets =
      Enum.map(es, &generate_expr(&1, local_env, global_env, current_mod, nil, fix_env))

    erl_target =
      case erl_targets do
        [s] -> s
        m -> {:tuple, 1, m}
      end

    erl_clauses =
      Enum.map(bs, fn {pats, body} ->
        pl = if is_list(pats), do: pats, else: [pats]
        erl_pats = Enum.map(pl, &generate_pattern/1)

        erl_p =
          case erl_pats do
            [sp] -> sp
            mp -> {:tuple, 1, mp}
          end

        local_env2 = MapSet.union(local_env, Enum.reduce(pl, MapSet.new(), &find_bound_vars/2))

        {:clause, 1, [erl_p], [],
         [generate_expr(body, local_env2, global_env, current_mod, nil, fix_env)]}
      end)

    {:case, 1, erl_target, erl_clauses}
  end

  defp generate_expr(%AST.ExprTuple{elems: es}, local_env, global_env, current_mod, _, fix_env),
    do:
      {:tuple, 1,
       Enum.map(es, &generate_expr(&1, local_env, global_env, current_mod, nil, fix_env))}

  defp generate_expr(
         %AST.ExprList{elems: es, tail: t},
         local_env,
         global_env,
         current_mod,
         _,
         fix_env
       ) do
    erl_elems = Enum.map(es, &generate_expr(&1, local_env, global_env, current_mod, nil, fix_env))

    erl_tail =
      if t, do: generate_expr(t, local_env, global_env, current_mod, nil, fix_env), else: {nil, 1}

    Enum.reduce(Enum.reverse(erl_elems), erl_tail, fn e, acc -> {:cons, 1, e, acc} end)
  end

  defp generate_expr(
         %AST.ExprIf{cond: c, then_br: t, else_br: e},
         local_env,
         global_env,
         current_mod,
         _,
         fix_env
       ) do
    {:case, 1, generate_expr(c, local_env, global_env, current_mod, nil, fix_env),
     [
       {:clause, 1, [{:atom, 1, true}], [],
        [generate_expr(t, local_env, global_env, current_mod, nil, fix_env)]},
       {:clause, 1, [{:atom, 1, false}], [],
        [generate_expr(e, local_env, global_env, current_mod, nil, fix_env)]}
     ]}
  end

  defp generate_expr(nil, _, _, _, _, _), do: {:atom, 1, :undefined}
  defp generate_expr(expr, _, _, _, _, _), do: raise("Unsupported: #{inspect(expr)}")

  defp desugar_do([]), do: %AST.ExprVar{name: "unit"}

  defp desugar_do([{:expr, expr}]), do: expr

  defp desugar_do([{:bind, binder, expr}]),
    do: %AST.ExprApp{
      func: %AST.ExprVar{name: "bind"},
      arg: %AST.ExprTuple{
        elems: [expr, %AST.ExprLam{binder: binder, body: %AST.ExprVar{name: "unit"}}]
      }
    }

  defp desugar_do([{:let, decls} | rest]) do
    %AST.ExprLet{bindings: decls, body: desugar_do(rest)}
  end

  defp desugar_do([{:bind, binder, expr} | rest]) do
    lam = %AST.ExprLam{binder: binder, body: desugar_do(rest)}
    %AST.ExprApp{func: %AST.ExprApp{func: %AST.ExprVar{name: "bind"}, arg: expr}, arg: lam}
  end

  defp desugar_do([{:expr, expr} | rest]) do
    %AST.ExprApp{
      func: %AST.ExprApp{func: %AST.ExprVar{name: "discard"}, arg: expr},
      arg: desugar_do(rest)
    }
  end

  defp constructor_name?(name) when is_binary(name) do
    last_part = name |> String.split(".") |> List.last()
    match?(<<first::utf8, _::binary>> when first >= ?A and first <= ?Z, last_part)
  end

  defp constructor_name?(_), do: false

  defp generate_pattern(%AST.BinderVar{name: "_"}), do: {:var, 1, :_}
  defp generate_pattern(%AST.BinderVar{name: "true"}), do: {:atom, 1, true}
  defp generate_pattern(%AST.BinderVar{name: "false"}), do: {:atom, 1, false}

  defp generate_pattern(%AST.BinderVar{name: "num_" <> ns}),
    do: {:integer, 1, String.to_integer(ns)}

  defp generate_pattern(%AST.BinderVar{name: "float_" <> fs}),
    do: {:float, 1, String.to_float(fs)}

  defp generate_pattern(%AST.BinderVar{name: "str_" <> s}),
    do: {:string, 1, String.to_charlist(s)}

  defp generate_pattern(%AST.BinderVar{name: "char_" <> cs}),
    do: {:integer, 1, String.to_integer(cs)}

  defp generate_pattern(%AST.BinderAtom{value: v}), do: {:atom, 1, String.to_atom(v)}

  defp generate_pattern(%AST.BinderBinary{binder: b}) do
    inner = generate_pattern(b)
    {:bin, 1, [{:bin_element, 1, inner, :default, :default}]}
  end

  defp generate_pattern(%AST.BinderVar{name: name}),
    do: {:var, 1, String.capitalize(name) |> String.to_atom()}

  defp generate_pattern(%AST.ExprVar{name: name}),
    do: generate_pattern(%AST.BinderVar{name: name})

  defp generate_pattern(%AST.ExprTuple{elems: es}),
    do: {:tuple, 1, Enum.map(es, &generate_pattern/1)}

  defp generate_pattern(%AST.BinderTuple{elems: es}),
    do: {:tuple, 1, Enum.map(es, &generate_pattern/1)}

  defp generate_pattern(%AST.BinderAs{name: name, binder: binder}) do
    {:match, 1, {:var, 1, String.capitalize(name) |> String.to_atom()}, generate_pattern(binder)}
  end

  defp generate_pattern(%AST.BinderConstructor{name: name, args: args}),
    do: {:tuple, 1, [{:atom, 1, String.to_atom(name)} | Enum.map(args, &generate_pattern/1)]}

  defp generate_pattern(%AST.ExprApp{} = app) do
    {f, args} = flatten_app(app, [])

    case f do
      %AST.ExprVar{name: cn} ->
        {:tuple, 1, [{:atom, 1, String.to_atom(cn)} | Enum.map(args, &generate_pattern/1)]}
    end
  end

  defp generate_pattern(%AST.ExprList{elems: es, tail: t}),
    do:
      Enum.reduce(Enum.reverse(es), if(t, do: generate_pattern(t), else: {nil, 1}), fn e, acc ->
        {:cons, 1, generate_pattern(e), acc}
      end)

  defp generate_pattern(%AST.BinderList{head: h, tail: t}) do
    erl_tail = if(t, do: generate_pattern(t), else: {nil, 1})

    case h do
      nil ->
        erl_tail

      [] ->
        erl_tail

      # Build a cons chain for [h1, h2, ... | tail]
      elems when is_list(elems) ->
        Enum.reduce(Enum.reverse(elems), erl_tail, fn e, acc ->
          {:cons, 1, generate_pattern(e), acc}
        end)

      single ->
        {:cons, 1, generate_pattern(single), erl_tail}
    end
  end

  defp generate_pattern(l) when is_list(l) do
    case l do
      [s] -> generate_pattern(s)
      m -> {:tuple, 1, Enum.map(m, &generate_pattern/1)}
    end
  end

  defp generate_pattern(nil), do: {:var, 1, :_}
  defp generate_pattern(o), do: raise("Unsupported pattern: #{inspect(o)}")

  defp find_bound_vars(%AST.BinderVar{name: "_"}, acc), do: acc
  defp find_bound_vars(%AST.BinderVar{name: "num_" <> _}, acc), do: acc
  defp find_bound_vars(%AST.BinderVar{name: "float_" <> _}, acc), do: acc
  defp find_bound_vars(%AST.BinderVar{name: "str_" <> _}, acc), do: acc
  defp find_bound_vars(%AST.BinderVar{name: "char_" <> _}, acc), do: acc
  defp find_bound_vars(%AST.BinderVar{name: n}, acc), do: MapSet.put(acc, n)

  defp find_bound_vars(%AST.BinderAtom{}, acc), do: acc

  defp find_bound_vars(%AST.BinderBinary{binder: b}, acc), do: find_bound_vars(b, acc)

  defp find_bound_vars(%AST.BinderAs{name: n, binder: b}, acc),
    do: find_bound_vars(b, MapSet.put(acc, n))

  defp find_bound_vars(%AST.BinderConstructor{args: as}, acc),
    do: Enum.reduce(as, acc, &find_bound_vars/2)

  defp find_bound_vars(%AST.BinderTuple{elems: es}, acc),
    do: Enum.reduce(es, acc, &find_bound_vars/2)

  defp find_bound_vars(%AST.ExprVar{name: n}, acc),
    do:
      if(
        constructor_name?(n) or String.starts_with?(n, "num_") or String.starts_with?(n, "float_") or
          String.starts_with?(n, "str_") or String.starts_with?(n, "char_"),
        do: acc,
        else: MapSet.put(acc, n)
      )

  defp find_bound_vars(%AST.ExprTuple{elems: es}, acc),
    do: Enum.reduce(es, acc, &find_bound_vars/2)

  defp find_bound_vars(%AST.ExprList{elems: es, tail: t}, acc),
    do: Enum.reduce(es, if(t, do: find_bound_vars(t, acc), else: acc), &find_bound_vars/2)

  defp find_bound_vars(%AST.ExprApp{} = app, acc) do
    {f, args} = flatten_app(app, [])
    Enum.reduce(args, find_bound_vars(f, acc), &find_bound_vars/2)
  end

  defp find_bound_vars(ls, acc) when is_list(ls), do: Enum.reduce(ls, acc, &find_bound_vars/2)

  defp find_bound_vars(%AST.BinderList{head: h, tail: t}, acc),
    do: find_bound_vars(h, if(t, do: find_bound_vars(t, acc), else: acc))

  defp find_bound_vars(_, acc), do: acc

  defp flatten_app(%AST.ExprApp{func: f, arg: a}, acc), do: flatten_app(f, [a | acc])
  defp flatten_app(f, acc), do: {f, acc}

  defp find_dictionary(class_name, env) do
    prefix = if class_name, do: "Dict_#{class_name}_", else: "Dict_"
    Enum.find(env, &String.starts_with?(&1, prefix))
  end

  defp type_outer_name(%Phi.Type.TApp{func: f}), do: type_outer_name(f)
  defp type_outer_name(%Phi.Type.TCon{name: n}), do: n
  defp type_outer_name(_), do: nil

  # Map known Phi operators to native Erlang ops when they resolve to typeclass methods.
  # This avoids generating dispatch case expressions for primitive ops, preventing
  # exponential Erlang compiler blowup (e.g., Data.Read.phi's splitTop with many == and ||).
  @native_ops %{
    # Eq
    "eq" => :"=:=",
    # Eq
    "notEq" => :"/=",
    # Ord — no direct Erlang equivalent
    "compare" => nil,
    # Ord
    "lessThan" => :<,
    # Ord
    "lessThanOrEq" => :"=<",
    # Ord
    "greaterThan" => :>,
    # Ord
    "greaterThanOrEq" => :>=,
    # Semiring
    "add" => :+,
    # Semiring
    "mul" => :*,
    # Ring
    "sub" => :-,
    # Division
    "div" => :div,
    # Semigroup (String/List)
    "append" => :++,
    # HeytingAlgebra (&&)
    "conj" => :andalso,
    # HeytingAlgebra (||)
    "disj" => :orelse
  }

  defp native_erlang_op(name, global_env) do
    # Only short-circuit to native ops if the name resolves to a known typeclass method
    real_name =
      case Env.resolve_term_alias(global_env, name) do
        {:ok, rn} -> rn
        _ -> name
      end

    base = real_name |> String.split(".") |> List.last()
    is_method = Map.has_key?(global_env.member_to_class, real_name)

    if is_method do
      Map.get(@native_ops, base)
    else
      nil
    end
  end

  defp erlang_type_guard(type, arg_var) do
    case type_outer_name(type) do
      "Fun" ->
        {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_function}}, [arg_var]}

      "->" ->
        {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_function}}, [arg_var]}

      "List" ->
        {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_list}}, [arg_var]}

      "String" ->
        {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_list}}, [arg_var]}

      "Integer" ->
        {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_integer}}, [arg_var]}

      "Float" ->
        {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_float}}, [arg_var]}

      "Boolean" ->
        {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_boolean}}, [arg_var]}

      "Atom" ->
        {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_atom}}, [arg_var]}

      "Binary" ->
        {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_binary}}, [arg_var]}

      "Char" ->
        {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_integer}}, [arg_var]}

      "IO" ->
        {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_function}},
         [arg_var, {:integer, 1, 0}]}

      ctor_name ->
        {:op, 1, :andalso,
         {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :is_tuple}}, [arg_var]},
         {:op, 1, :"=:=",
          {:call, 1, {:remote, 1, {:atom, 1, :erlang}, {:atom, 1, :element}},
           [{:integer, 1, 1}, arg_var]}, {:atom, 1, String.to_atom(ctor_name)}}}
    end
  end

  defp generate_dispatch_call(real_name, mod, erl_args, class_name, global_env, current_mod) do
    instances = Map.get(global_env.instances, class_name, [])
    base_name = real_name |> String.split(".") |> List.last()
    is_method = Map.has_key?(global_env.member_to_class, real_name)

    # Pick the first available instance — typechecker already resolved the correct one.
    # No case expression, no guards, no runtime dispatch overhead.
    first_instance =
      Enum.find(instances, fn
        %{mod: dict_mod} when is_atom(dict_mod) and not is_nil(dict_mod) -> true
        _ -> false
      end)

    if first_instance do
      %{dict_name: dn, mod: dict_mod, constraints: cs} = first_instance

      derived_dict_args =
        if cs do
          Enum.map(cs, fn
            %Type.TConstrained{class_name: cn, args: [%Type.TVar{id: vn} | _]} ->
              {:var, 1, String.to_atom("Dict_#{cn}_#{vn}")}

            %Type.TConstrained{class_name: cn, args: [%Type.TCon{name: tn} | _]} ->
              {:var, 1, String.to_atom("Dict_#{cn}_#{tn}")}

            _ ->
              {:atom, 1, :undefined_dict}
          end)
        else
          []
        end

      dict_call =
        {:call, 1, {:remote, 1, {:atom, 1, dict_mod}, {:atom, 1, String.to_atom(dn)}},
         derived_dict_args}

      if is_method do
        base_accessor =
          if mod && mod != current_mod do
            {:call, 1, {:remote, 1, {:atom, 1, mod}, {:atom, 1, String.to_atom(base_name)}},
             [dict_call]}
          else
            {:call, 1, {:atom, 1, String.to_atom(base_name)}, [dict_call]}
          end

        case erl_args do
          [] -> base_accessor
          _ -> {:call, 1, base_accessor, erl_args}
        end
      else
        {_, scheme, _, _} = get_info(real_name, global_env)
        {_, ea} = if scheme, do: split_arity(scheme), else: {0, 0}

        target =
          if mod && mod != current_mod do
            {:remote, 1, {:atom, 1, mod}, {:atom, 1, String.to_atom(base_name)}}
          else
            {:atom, 1, String.to_atom(base_name)}
          end

        generate_static_call(base_name, mod, ea + 1, [dict_call | erl_args], current_mod)
      end
    else
      # No instance found — fallback
      if is_method do
        accessor_call = generate_static_call(real_name, mod, 1, [], current_mod)
        Enum.reduce(erl_args, accessor_call, fn arg, acc -> {:call, 1, acc, [arg]} end)
      else
        {_, scheme, _, _} = get_info(real_name, global_env)
        {num_dicts, num_args} = if scheme, do: split_arity(scheme), else: {0, 0}
        total_arity = num_dicts + num_args

        generate_static_call(
          base_name,
          mod,
          total_arity,
          [{:atom, 1, :no_dict_resolved} | erl_args],
          current_mod
        )
      end
    end
  end

  defp generate_static_call(name, mod, arity, args, current_mod) do
    f_atom = String.to_atom(name)

    target =
      if mod && mod != current_mod && mod != :local,
        do: {:remote, 1, {:atom, 1, mod}, {:atom, 1, f_atom}},
        else: {:atom, 1, f_atom}

    num = length(args)

    cond do
      num == arity ->
        {:call, 1, target, args}

      num < arity ->
        vs = Enum.map(1..(arity - num), fn i -> {:var, 1, String.to_atom("P#{i}")} end)
        {:fun, 1, {:clauses, [{:clause, 1, vs, [], [{:call, 1, target, args ++ vs}]}]}}

      num > arity ->
        {ba, ea} = Enum.split(args, arity)
        Enum.reduce(ea, {:call, 1, target, ba}, fn a, acc -> {:call, 1, acc, [a]} end)
    end
  end

  defp sort_bindings(bs) do
    names =
      Enum.reduce(bs, MapSet.new(), fn
        %AST.DeclValue{name: "_pat", binders: [p]}, a -> find_bound_vars(p, a)
        %AST.DeclValue{name: n}, a -> MapSet.put(a, n)
        _, a -> a
      end)

    wd =
      Enum.map(bs, fn
        %AST.DeclValue{name: n, expr: r} = b ->
          {b, MapSet.intersection(find_used_vars(r, MapSet.new()), MapSet.delete(names, n))}

        o ->
          {o, MapSet.new()}
      end)

    do_sort_bindings(wd, MapSet.new(), [])
  end

  @dialyzer {:nowarn_function, do_sort_bindings: 3}
  defp do_sort_bindings([], _, acc), do: Enum.reverse(acc)

  @dialyzer {:nowarn_function, do_sort_bindings: 3}
  defp do_sort_bindings(rem, av, acc) do
    case Enum.find_index(rem, fn {_, d} -> MapSet.subset?(d, av) end) do
      nil ->
        Enum.reverse(acc) ++ Enum.map(rem, &elem(&1, 0))

      idx ->
        {b, _} = Enum.at(rem, idx)

        ns =
          if(match?(%AST.DeclValue{name: "_pat"}, b),
            do: find_bound_vars(hd(b.binders), MapSet.new()),
            else: MapSet.new([b.name])
          )

        do_sort_bindings(List.delete_at(rem, idx), MapSet.union(av, ns), [b | acc])
    end
  end

  defp find_used_vars(%AST.ExprVar{name: n}, acc),
    do: if(constructor_name?(n), do: acc, else: MapSet.put(acc, n))

  defp find_used_vars(%AST.ExprApp{func: f, arg: a}, acc),
    do: find_used_vars(a, find_used_vars(f, acc))

  defp find_used_vars(%AST.ExprLam{binder: b, body: bo}, acc),
    do:
      MapSet.union(
        acc,
        MapSet.difference(find_used_vars(bo, MapSet.new()), find_bound_vars(b, MapSet.new()))
      )

  defp find_used_vars(%AST.ExprLet{bindings: bs, body: bo}, acc) do
    ru =
      Enum.reduce(bs, MapSet.new(), fn
        %AST.DeclValue{expr: r}, a -> find_used_vars(r, a)
        _, a -> a
      end)

    MapSet.union(acc, MapSet.union(ru, find_used_vars(bo, MapSet.new())))
  end

  defp find_used_vars(%AST.ExprTuple{elems: es}, acc), do: Enum.reduce(es, acc, &find_used_vars/2)

  defp find_used_vars(%AST.ExprList{elems: es, tail: t}, acc),
    do: Enum.reduce(es, if(t, do: find_used_vars(t, acc), else: acc), &find_used_vars/2)

  defp find_used_vars(%AST.ExprCase{exprs: ts, branches: bs}, acc) do
    acc1 = Enum.reduce(ts, acc, &find_used_vars/2)

    Enum.reduce(bs, acc1, fn {p, b}, a ->
      MapSet.union(
        a,
        MapSet.difference(find_used_vars(b, MapSet.new()), find_bound_vars(p, MapSet.new()))
      )
    end)
  end

  defp find_used_vars(%AST.ExprIf{cond: c, then_br: t, else_br: e}, acc),
    do: find_used_vars(e, find_used_vars(t, find_used_vars(c, acc)))

  defp find_used_vars(%AST.ExprDo{statements: stats}, acc) do
    Enum.reduce(stats, acc, fn
      {:bind, _binder, expr}, a ->
        find_used_vars(expr, a)

      {:expr, expr}, a ->
        find_used_vars(expr, a)

      {:let, decls}, a ->
        Enum.reduce(decls, a, fn
          %AST.DeclValue{expr: r}, a2 -> find_used_vars(r, a2)
          _, a2 -> a2
        end)
    end)
  end

  defp find_used_vars(%AST.ExprReceive{clauses: clauses, after_clause: after_clause}, acc) do
    acc1 =
      Enum.reduce(clauses, acc, fn {_pat, body}, a ->
        find_used_vars(body, a)
      end)

    case after_clause do
      :none ->
        acc1

      {:ok, after_body} ->
        find_used_vars(after_body, acc1)

      {:ok, timeout_expr, after_body} ->
        find_used_vars(after_body, find_used_vars(timeout_expr, acc1))
    end
  end

  defp find_used_vars(_, acc), do: acc
end
