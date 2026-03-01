defmodule Phi.Typechecker do
  @moduledoc """
  Hindley-Milner type inference engine Prototype.
  """
  alias Phi.Type.{TVar, TCon, TApp, Forall}
  alias Phi.AST

  defmodule Env do
    @moduledoc "Typing Environment mapping names to Polytypes"
    defstruct bindings: %{}

    def new(), do: %Env{}

    def extend(%Env{bindings: b} = env, name, scheme) do
      %{env | bindings: Map.put(b, name, scheme)}
    end

    def lookup(%Env{bindings: b}, name) do
      Map.fetch(b, name)
    end
  end

  defmodule State do
    @moduledoc "Typechecker state for fresh variables and substitutions"
    defstruct next_id: 1, subst: %{}
  end

  # -- API --

  @doc "Infers the type of an expression in a given environment"
  def infer(env, expr) do
    state = %State{}
    case do_infer(env, state, expr) do
      {:ok, type, final_state} ->
        {:ok, apply_subst(final_state.subst, type)}
      err -> err
    end
  end

  @doc "Builds an initial typing environment from a list of AST declarations"
  def build_env(decls, env \\ Env.new()) do
    Enum.reduce(decls, env, fn
      %AST.DeclTypeSignature{name: name, type: ast_type}, acc ->
        # We put the type scheme into the environment
        type = ast_to_type(ast_type)
        scheme = case type do
          %Forall{} = s -> s
          # If it has no forall, it's just a monotype, but we wrap it in an empty scheme
          t -> %Forall{vars: [], type: t}
        end
        Env.extend(acc, name, scheme)

      %AST.DeclData{name: data_name, args: args, constructors: constructors}, acc ->
        # args are %AST.TypeVar{name: "a"}, etc.
        # So the result type for the constructor should be `TApp(TApp(Tuple, a), b)`
        type_vars = Enum.map(args, fn arg -> %TVar{id: arg.name} end)

        ret_type = Enum.reduce(type_vars, %TCon{name: data_name}, fn tv, acc_type ->
          %TApp{func: acc_type, arg: tv}
        end)

        Enum.reduce(constructors, acc, fn {c_name, c_args}, env_acc ->
           # c_args are %AST.TypeVar{name: "a"}, %AST.TypeVar{name: "b"}
           # So the constructor function is `a -> b -> Tuple a b`
           c_arg_types = Enum.map(c_args, &ast_to_type/1)

           con_type = Enum.reduce(Enum.reverse(c_arg_types), ret_type, fn t_arg, acc_t ->
             Phi.Type.arrow(t_arg, acc_t)
           end)

           var_names = Enum.map(args, & &1.name)
           Env.extend(env_acc, c_name, %Forall{vars: var_names, type: con_type})
        end)
      _, acc -> acc
    end)
  end

  @doc "Converts an AST type into a Phi.Type representation"
  def ast_to_type(%AST.TypeConstructor{name: name}), do: %TCon{name: name}
  def ast_to_type(%AST.TypeVar{name: name}), do: %TVar{id: name} # Using string ID for bound variables initially
  def ast_to_type(%AST.TypeArrow{domain: d, codomain: c}) do
    Phi.Type.arrow(ast_to_type(d), ast_to_type(c))
  end
  def ast_to_type(%AST.TypeApp{func: f, arg: a}) do
    %TApp{func: ast_to_type(f), arg: ast_to_type(a)}
  end
  def ast_to_type(%AST.TypeForall{vars: vars, type: t}) do
    var_names = Enum.map(vars, & &1.name)
    %Forall{vars: var_names, type: ast_to_type(t)}
  end

  # -- Algorithm W --

  defp do_infer(env, state, %AST.ExprVar{name: name}) do
    case Env.lookup(env, name) do
      {:ok, scheme} ->
        {type, state2} = instantiate(scheme, state)
        {:ok, type, state2}
      :error ->
        {:error, "Unbound variable: #{name}"}
    end
  end
  defp do_infer(env, state, %AST.ExprLam{binder: binder, body: body}) do
    # Infer the binder (could be a Var or Constructor pattern)
    case infer_binder(env, state, binder) do
      {:ok, type_binder, bound_vars, state2} ->
        # Extend environment with the bound variables from pattern matching
        env2 = Enum.reduce(bound_vars, env, fn {name, t}, acc_env ->
          Env.extend(acc_env, name, %Forall{vars: [], type: t})
        end)
        case do_infer(env2, state2, body) do
          {:ok, t_body, state3} ->
            {:ok, Phi.Type.arrow(apply_subst(state3.subst, type_binder), t_body), state3}
          err -> err
        end
      err -> err
    end
  end

  defp do_infer(env, state, %AST.ExprApp{func: f, arg: arg}) do
    t_ret = %TVar{id: state.next_id}
    state2 = %{state | next_id: state.next_id + 1}

    with {:ok, t_func, state3} <- do_infer(env, state2, f),
         {:ok, t_arg, state4} <- do_infer(env, state3, arg),
         # Unify t_func with `t_arg -> t_ret`
         t_expected = Phi.Type.arrow(t_arg, t_ret),
         {:ok, state5} <- unify(t_func, t_expected, state4) do
      {:ok, apply_subst(state5.subst, t_ret), state5}
    end
  end

  defp do_infer(env, state, %AST.ExprLet{bindings: [%AST.DeclValue{name: name, expr: expr}], body: body}) do
    case do_infer(env, state, expr) do
      {:ok, t_expr, state2} ->
        # Generalize t_expr
        scheme = generalize(env, state2.subst, t_expr)
        env2 = Env.extend(env, name, scheme)
        do_infer(env2, state2, body)
      err -> err
    end
  end
  defp do_infer(_env, _state, expr), do: {:error, "Unsupported expression for inference: #{inspect(expr)}"}

  defp infer_binder(_env, state, %AST.BinderVar{name: name}) do
    t_binder = %TVar{id: state.next_id}
    state2 = %{state | next_id: state.next_id + 1}
    {:ok, t_binder, %{name => t_binder}, state2}
  end
  defp infer_binder(env, state, %AST.BinderConstructor{name: name, args: args}) do
    # 1. Lookup the constructor's type
    case Env.lookup(env, name) do
      {:ok, scheme} ->
        {t_con, state2} = instantiate(scheme, state)

        # 2. Extract types for each argument in the pattern
        {arg_types, bound_vars, state3} = Enum.reduce(args, {[], %{}, state2}, fn arg, {t_args, b_vars, st} ->
          case infer_binder(env, st, arg) do
            {:ok, t_arg, new_bounds, st2} ->
              {[t_arg | t_args], Map.merge(b_vars, new_bounds), st2}
            err -> throw err
          end
        end)
        arg_types = Enum.reverse(arg_types)

        # 3. Reconstruct expected function type from args: t_arg1 -> t_arg2 -> ... -> t_ret
        t_ret = %TVar{id: state3.next_id}
        state4 = %{state3 | next_id: state3.next_id + 1}
        t_expected = Enum.reduce(Enum.reverse(arg_types), t_ret, fn arg_t, acc -> Phi.Type.arrow(arg_t, acc) end)

        # 4. Unify constructor's actual type with the expected application
        case unify(t_con, t_expected, state4) do
          {:ok, state5} ->
            {:ok, apply_subst(state5.subst, t_ret), bound_vars, state5}
          err -> err
        end

      :error ->
        {:error, "Unknown constructor in pattern: #{name}"}
    end
  catch
    err -> err
  end

  def unify(t1, t2, state) do
    t1 = apply_subst(state.subst, t1)
    t2 = apply_subst(state.subst, t2)
    do_unify(t1, t2, state)
  end

  defp do_unify(%TCon{name: a}, %TCon{name: a}, state), do: {:ok, state}
  defp do_unify(%TVar{id: a}, %TVar{id: a}, state), do: {:ok, state}
  defp do_unify(%TVar{id: a}, t, state), do: bind(a, t, state)
  defp do_unify(t, %TVar{id: a}, state), do: bind(a, t, state)
  defp do_unify(%TApp{func: f1, arg: a1}, %TApp{func: f2, arg: a2}, state) do
    with {:ok, state2} <- unify(f1, f2, state),
         {:ok, state3} <- unify(a1, a2, state2) do
      {:ok, state3}
    end
  end

  defp do_unify(%Phi.Type.TRowEmpty{}, %Phi.Type.TRowEmpty{}, state), do: {:ok, state}
  defp do_unify(%Phi.Type.TRowExtend{label: l1, type: t1, rest: r1}, %Phi.Type.TRowExtend{} = row2, state) do
    case rewrite_row(row2, l1, state) do
      {:ok, t2, r2, state2} ->
        with {:ok, state3} <- unify(t1, t2, state2),
             {:ok, state4} <- unify(r1, r2, state3) do
          {:ok, state4}
        end
      :error ->
        {:error, "Cannot unify row, missing label #{l1}"}
    end
  end

  defp do_unify(%Phi.Type.TConstrained{class_name: c1, args: a1, type: t1}, %Phi.Type.TConstrained{class_name: c2, args: a2, type: t2}, state) do
    if c1 == c2 and length(a1) == length(a2) do
      state2 = Enum.zip(a1, a2) |> Enum.reduce(state, fn {arg1, arg2}, st ->
        case unify(arg1, arg2, st) do
          {:ok, st_next} -> st_next
          err -> throw err
        end
      end)
      unify(t1, t2, state2)
    else
      {:error, "Cannot unify constrained types"}
    end
  catch
    err -> err
  end

  defp do_unify(t1, t2, _state), do: {:error, "Cannot unify #{inspect(t1)} with #{inspect(t2)}"}

  defp rewrite_row(%Phi.Type.TRowExtend{label: l2, type: t2, rest: r2}, target_label, state) do
    if l2 == target_label do
      {:ok, t2, r2, state}
    else
      r2 = apply_subst(state.subst, r2)
      case r2 do
        %Phi.Type.TRowExtend{} ->
          case rewrite_row(r2, target_label, state) do
            {:ok, t_found, r_rest, state2} ->
              {:ok, t_found, %Phi.Type.TRowExtend{label: l2, type: t2, rest: r_rest}, state2}
            :error ->
              :error
          end
        %Phi.Type.TVar{id: a} ->
          t_found = %Phi.Type.TVar{id: state.next_id}
          r_rest = %Phi.Type.TVar{id: state.next_id + 1}
          state2 = %{state | next_id: state.next_id + 2}
          new_row = %Phi.Type.TRowExtend{label: target_label, type: t_found, rest: r_rest}
          case bind(a, new_row, state2) do
            {:ok, state3} ->
              {:ok, t_found, %Phi.Type.TRowExtend{label: l2, type: t2, rest: r_rest}, state3}
            err -> throw err
          end
        _ -> :error
      end
    end
  end

  defp bind(id, type, state) do
    if occurs_check(id, type) do
      {:error, "Occurs check failed: infinite type"}
    else
      new_subst = Map.put(state.subst, id, type)
      # Compose substitutions
      composed = Map.new(state.subst, fn {k, v} -> {k, apply_subst(new_subst, v)} end)
                 |> Map.put(id, type)
      {:ok, %{state | subst: composed}}
    end
  end

  # -- Substitution & Free Variables --

  defp apply_subst(subst, %TVar{id: id} = t) do
    case Map.fetch(subst, id) do
      {:ok, t2} -> apply_subst(subst, t2)
      :error -> t
    end
  end
  defp apply_subst(_subst, %TCon{} = t), do: t
  defp apply_subst(subst, %TApp{func: f, arg: a}) do
    %TApp{func: apply_subst(subst, f), arg: apply_subst(subst, a)}
  end
  defp apply_subst(subst, %Forall{vars: vars, type: t}) do
    # Remove bound vars from substitution
    subst2 = Map.drop(subst, vars)
    %Forall{vars: vars, type: apply_subst(subst2, t)}
  end
  defp apply_subst(subst, %Phi.Type.TConstrained{class_name: c, args: args, type: t}) do
    %Phi.Type.TConstrained{class_name: c, args: Enum.map(args, &apply_subst(subst, &1)), type: apply_subst(subst, t)}
  end
  defp apply_subst(_subst, %Phi.Type.TRowEmpty{} = t), do: t
  defp apply_subst(subst, %Phi.Type.TRowExtend{label: l, type: t, rest: r}) do
    %Phi.Type.TRowExtend{label: l, type: apply_subst(subst, t), rest: apply_subst(subst, r)}
  end

  defp occurs_check(id, type) do
    id in free_vars(type)
  end

  defp free_vars(%TVar{id: id}), do: MapSet.new([id])
  defp free_vars(%TCon{}), do: MapSet.new()
  defp free_vars(%TApp{func: f, arg: a}), do: MapSet.union(free_vars(f), free_vars(a))
  defp free_vars(%Forall{vars: vars, type: t}), do: MapSet.difference(free_vars(t), MapSet.new(vars))
  defp free_vars(%Phi.Type.TRowEmpty{}), do: MapSet.new()
  defp free_vars(%Phi.Type.TRowExtend{type: t, rest: r}), do: MapSet.union(free_vars(t), free_vars(r))
  defp free_vars(%Phi.Type.TConstrained{args: args, type: t}) do
    Enum.reduce(args, free_vars(t), fn arg, acc -> MapSet.union(acc, free_vars(arg)) end)
  end

  defp env_free_vars(%Env{bindings: b}, subst) do
    Enum.reduce(b, MapSet.new(), fn {_, scheme}, acc ->
      scheme_with_subst = apply_subst(subst, scheme)
      MapSet.union(acc, free_vars(scheme_with_subst))
    end)
  end

  # -- Instantiation & Generalization --

  defp instantiate(%Forall{vars: vars, type: t}, state) do
    {subst, state2} = Enum.reduce(vars, {%{}, state}, fn var, {s, st} ->
      new_var = %TVar{id: st.next_id}
      {Map.put(s, var, new_var), %{st | next_id: st.next_id + 1}}
    end)
    {apply_subst(subst, t), state2}
  end

  defp generalize(env, subst, type) do
    type = apply_subst(subst, type)
    env_fvs = env_free_vars(env, subst)
    type_fvs = free_vars(type)

    # Quantify any free variables in the type that are NOT free in the environment
    quantified = MapSet.difference(type_fvs, env_fvs) |> MapSet.to_list()
    %Forall{vars: quantified, type: type}
  end

end
