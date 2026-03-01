defmodule Phi.Parser do
  alias Phi.AST

  def parse(tokens) do
    # IO.inspect(tokens, limit: :infinity, label: "TOKENS")
    case parse_module(tokens) do
      {:ok, module, []} -> {:ok, module}
      {:ok, module, rest} -> {:ok, module, rest}
      err -> err
    end
  end

  defp parse_module([{:module, _, _} | rest]) do
    case parse_module_name(rest, "") do
      {:ok, name, rest2} ->
        # Skip injected semicolons
        rest3 = case rest2 do
          [{:semicolon, _, _} | r] -> r
          _ -> rest2
        end

        case rest3 do
          [{:where, _, _}, {:left_brace, _, _} | rest4] ->
            parse_body(name, rest4)
          [{:left_paren, _, _} | _] ->
            case parse_export_list(rest3) do
              {:ok, _exports, rest5} ->
                 # Skip injected semicolons after export list
                 rest6 = case rest5 do
                    [{:semicolon, _, _} | r] -> r
                    _ -> rest5
                 end
                 case rest6 do
                    [{:where, _, _}, {:left_brace, _, _} | rest7] -> parse_body(name, rest7)
                    _ -> parse_body(name, rest6)
                 end
              err -> err
            end
          _ ->
            # Optional where
            case rest3 do
               [{:left_brace, _, _} | rest4] -> parse_body(name, rest4)
               _ -> parse_body(name, rest3)
            end
        end
      err -> err
    end
  end
  defp parse_module(tokens), do: {:error, :invalid_module, tokens}

  defp parse_body(name, tokens) do
    case parse_decls(tokens, []) do
      {:ok, decls, rest} -> {:ok, %AST.Module{name: name, declarations: Enum.reverse(decls)}, rest}
      err -> err
    end
  end

  defp parse_module_name([{:proper_name, _, _, name} | rest], acc) do
    new_acc = if acc == "", do: name, else: acc <> "." <> name
    case rest do
      [{:dot, _, _} | rest2] -> parse_module_name(rest2, new_acc)
      _ -> {:ok, new_acc, rest}
    end
  end
  defp parse_module_name(_, _), do: :error

  defp parse_decls([{:right_brace, _, _} | rest], acc) do
    # Skip redundant braces
    case rest do
      [{:right_brace, _, _} | _] -> parse_decls(rest, acc)
      _ -> {:ok, acc, rest}
    end
  end
  defp parse_decls(tokens, acc) do
    case parse_decl(tokens) do
      {:ok, decl, rest} ->
        case rest do
          [{:semicolon, _, _} | rest2] -> parse_decls(rest2, [decl | acc])
          [{:else_kw, _, _} | _] -> parse_decls(rest, [decl | acc])
          [{:right_brace, _, _} | _] -> parse_decls(rest, [decl | acc])
          [{tok, _, _} | _] when tok in [:right_paren, :right_square, :in] -> {:ok, Enum.reverse([decl | acc]), rest}
          [] -> {:ok, Enum.reverse([decl | acc]), []}
          _ -> {:error, :expected_semicolon_or_brace, rest}
        end
      err -> err
    end
  end

  # --- Declaration Rules ---

  defp parse_decl([{:import, _, _} | rest]) do
    case parse_module_name(rest, "") do
      {:ok, mod_name, rest2} ->
        {import_list, rest3, hiding} = case rest2 do
          [{:left_paren, _, _} | _] ->
            case parse_import_list(rest2) do
              {:ok, list, r} -> {list, r, false}
              _ -> {[], rest2, false}
            end
          [{:hiding, _, _}, {:left_paren, _, _} | _] ->
            case parse_import_list(tl(rest2)) do
              {:ok, list, r} -> {list, r, true}
              _ -> {[], rest2, true}
            end
          _ -> {[], rest2, false}
        end

        {alias_name, rest4} = case rest3 do
          [{:as, _, _}, {:proper_name, _, _, alias_name} | rest_as] -> {alias_name, rest_as}
          _ -> {nil, rest3}
        end

        {:ok, %AST.DeclImport{module: mod_name, import_list: import_list, alias: alias_name, hiding: hiding}, rest4}
      err -> err
    end
  end

  defp parse_decl([{fix, _, _} | rest]) when fix in [:infix, :infixl, :infixr] do
    case rest do
       [{:number, _, _, prec}, item | rest2] ->
         name = case item do
           {:var_ident, _, _, n} -> n
           {:proper_name, _, _, n} -> n
           _ -> nil
         end
         case rest2 do
           [{:as, _, _}, {:operator, _, _, op} | rest3] ->
             {:ok, %AST.DeclFixity{prec: prec, op: op, name: name, assoc: fix}, rest3}
           [{:operator, _, _, op} | rest3] ->
             {:ok, %AST.DeclFixity{prec: prec, op: op, name: name, assoc: fix}, rest3}
           _ -> {:error, :invalid_fixity, rest2}
         end
       _ -> {:error, :invalid_fixity, rest}
    end
  end

  defp parse_decl([{:foreign, _, _}, {:import, _, _}, {:var_ident, _, _, name} | rest]) do
    rest2 = case rest do
      [{:semicolon, _, _} | r] -> r
      _ -> rest
    end

    case rest2 do
      [{:double_colon, _, _} | rest3] ->
        case parse_type(rest3) do
          {:ok, type, rest4} -> {:ok, %AST.DeclForeign{name: name, type: type}, rest4}
          err -> err
        end
      _ ->
        {:error, :expected_foreign_signature, rest2}
    end
  end

  defp parse_decl([{:foreign, _, _}, {:import, _, _}, {:data, _, _}, {:proper_name, _, _, name}, {:double_colon, _, _} | rest]) do
    case parse_type(rest) do
      {:ok, type, rest2} -> {:ok, %AST.DeclForeign{name: name, type: type}, rest2}
      err -> err
    end
  end

  defp parse_decl([{:type_kw, _, _}, {:proper_name, _, _, name} | rest]) do
    case parse_type_vars(rest, []) do
      {:ok, vars, [{:=, _, _} | rest2]} ->
        case parse_type(rest2) do
          {:ok, type, rest3} ->
            {:ok, %AST.DeclTypeAlias{name: name, args: vars, type: type}, rest3}
          err -> err
        end
      err -> err
    end
  end

  defp parse_decl([{:newtype, _, _}, {:proper_name, _, _, name} | rest]) do
    case parse_type_vars(rest, []) do
      {:ok, vars, [{:=, _, _}, {:proper_name, _, _, constr} | rest2]} ->
        case parse_type(rest2) do
          {:ok, type, rest3} ->
            {:ok, %AST.DeclNewtype{name: name, args: vars, constructor: constr, type: type}, rest3}
          err -> err
        end
      err -> err
    end
  end

  defp parse_decl([{:data, _, _}, {:proper_name, _, _, name} | rest]) do
    case parse_type_vars(rest, []) do
      {:ok, vars, [{:=, _, _} | rest2]} ->
        case parse_data_constructors(rest2, []) do
          {:ok, constructors, rest3} ->
            {:ok, %AST.DeclData{name: name, args: vars, constructors: constructors}, rest3}
          err -> err
        end
      {:ok, vars, rest2} ->
        {:ok, %AST.DeclData{name: name, args: vars, constructors: []}, rest2}
      err -> err
    end
  end

  defp parse_decl([{:class, _, _} | rest]) do
    case parse_type(rest) do
      {:ok, t, rest2} ->
        {rest3, _fundeps} = case rest2 do
          [{:pipe, _, _} | rest_fundep] -> find_fundeps(rest_fundep)
          _ -> {rest2, []}
        end
        case flatten_type_app(case t do %AST.TypeConstrained{type: inner} -> inner; _ -> t end, []) do
          [%AST.TypeConstructor{name: name} | args] ->
             case rest3 do
               [{:where, _, _}, {:left_brace, _, _} | rest4] ->
                  case parse_decls(rest4, []) do
                    {:ok, members, rest5} ->
                      {:ok, %AST.DeclClass{name: name, args: args, members: Enum.reverse(members)}, rest5}
                    err -> err
                  end
               _ -> {:ok, %AST.DeclClass{name: name, args: args, members: []}, rest3}
             end
          _ -> {:error, :invalid_class_header, rest3}
        end
      err -> err
    end
  end

  defp parse_decl([{:else_kw, _, _}, {:instance, _, _} | rest]), do: parse_instance_with_where(rest)
  defp parse_decl([{:instance, _, _} | rest]), do: parse_instance_with_where(rest)

  defp parse_decl([{:left_paren, _, _}, {:operator, _, _, name}, {:right_paren, _, _}, {:double_colon, _, _} | rest]) do
    case parse_type(rest) do
      {:ok, type, rest2} -> {:ok, %AST.DeclTypeSignature{name: name, type: type}, rest2}
      err -> err
    end
  end

  defp parse_decl([tok | _] = tokens) when elem(tok, 0) in [:left_paren, :left_square, :proper_name, :unit, :number, :string, :char] do
    case parse_binder(tokens) do
      {:ok, binder, [{:=, _, _} | rest]} ->
        case parse_expr(rest) do
          {:ok, expr, rest2} -> {:ok, %AST.DeclValue{name: "_pat", binders: [binder], expr: expr}, rest2}
          err -> err
        end
      err -> err
    end
  end

  defp parse_decl([{:var_ident, _, _, name} | rest]) do
    case rest do
      [{:double_colon, _, _} | rest2] ->
        case parse_type(rest2) do
          {:ok, type, rest3} -> {:ok, %AST.DeclTypeSignature{name: name, type: type}, rest3}
          err -> err
        end
      _ ->
        case parse_lhs_binders(rest, []) do
          {:ok, binders, [{:=, _, _} | rest2]} ->
            case parse_expr(rest2) do
              {:ok, expr, rest3} ->
                case rest3 do
                   [{:where, _, _}, {:left_brace, _, _} | rest4] ->
                      case parse_decls(rest4, []) do
                         {:ok, _local_decls, rest5} ->
                            {:ok, %AST.DeclValue{name: name, binders: binders, expr: expr}, rest5}
                         err -> err
                      end
                   _ -> {:ok, %AST.DeclValue{name: name, binders: binders, expr: expr}, rest3}
                end
              err -> err
            end
          {:ok, binders, [{:pipe, _, _} | _] = rest_g} ->
            case parse_guards(rest_g, []) do
               {:ok, guards, rest2} ->
                 case rest2 do
                    [{:where, _, _}, {:left_brace, _, _} | rest3] ->
                       case parse_decls(rest3, []) do
                          {:ok, _local, rest4} -> {:ok, %AST.DeclValue{name: name, binders: binders, guards: guards}, rest4}
                          err -> err
                       end
                    _ -> {:ok, %AST.DeclValue{name: name, binders: binders, guards: guards}, rest2}
                 end
               err -> err
            end
          _ ->
            case parse_binder([{:var_ident, 0, 0, name} | rest]) do
               {:ok, binder, [{:=, _, _} | rest2]} ->
                  case parse_expr(rest2) do
                    {:ok, expr, rest3} -> {:ok, %AST.DeclValue{name: "_pat", binders: [binder], expr: expr}, rest3}
                    err -> err
                  end
               _ -> {:error, :invalid_declaration, [{:var_ident, 0, 0, name} | rest]}
            end
        end
    end
  end

  defp parse_decl(tokens) do
    case tokens do
      [{:else_kw, _, _} | _] -> {:error, :unexpected_else, tokens}
      _ -> {:error, :invalid_declaration, tokens}
    end
  end

  # --- Helpers ---

  defp parse_instance_with_where(tokens) do
    case parse_type(tokens) do
      {:ok, type, rest} ->
        {ctx, t} = case type do
          %AST.TypeConstrained{constraints: c, type: inner} -> {c, inner}
          _ -> {[], type}
        end
        case flatten_type_app(t, []) do
          [%AST.TypeConstructor{name: class_name} | types] ->
            case rest do
              [{:where, _, _}, {:left_brace, _, _} | rest2] ->
                case parse_decls(rest2, []) do
                  {:ok, members, rest3} ->
                    {:ok, %AST.DeclInstance{name: "anon", class: class_name, types: types, members: Enum.reverse(members), constraints: ctx}, rest3}
                  err -> err
                end
              _ -> {:ok, %AST.DeclInstance{name: "anon", class: class_name, types: types, members: [], constraints: ctx}, rest}
            end
          _ -> {:error, :invalid_instance_header, rest}
        end
      err -> err
    end
  end

  defp parse_import_list([{:left_paren, _, _} | rest]) do
    parse_import_list_items(rest, [])
  end
  defp parse_import_list(_), do: {:error, :invalid_import_list}

  defp parse_import_list_items([{:right_paren, _, _} | rest], acc), do: {:ok, Enum.reverse(acc), rest}
  defp parse_import_list_items([{:comma, _, _} | rest], acc), do: parse_import_list_items(rest, acc)
  defp parse_import_list_items([{:module, _, _} | rest], acc) do
    case parse_module_name(rest, "") do
      {:ok, name, rest2} -> parse_import_list_items(rest2, [{:module, name} | acc])
      err -> err
    end
  end
  defp parse_import_list_items([{:type_kw, _, _}, {:proper_name, _, _, name} | rest], acc) do
    parse_import_list_items(rest, [{:type, name} | acc])
  end
  defp parse_import_list_items([{:proper_name, _, _, name} | rest], acc) do
    case rest do
       [{:left_paren, _, _}, {:dot, _, _}, {:dot, _, _}, {:right_paren, _, _} | rest2] ->
         parse_import_list_items(rest2, [{:type_all, name} | acc])
       _ ->
         parse_import_list_items(rest, [{:type, name} | acc])
    end
  end
  defp parse_import_list_items([{:var_ident, _, _, name} | rest], acc) do
    parse_import_list_items(rest, [{:value, name} | acc])
  end
  defp parse_import_list_items([{:class, _, _}, {:proper_name, _, _, name} | rest], acc) do
    parse_import_list_items(rest, [{:class, name} | acc])
  end
  defp parse_import_list_items([{:operator, _, _, op} | rest], acc) do
    parse_import_list_items(rest, [{:operator, op} | acc])
  end
  defp parse_import_list_items([{:left_paren, _, _}, {:operator, _, _, op}, {:right_paren, _, _} | rest], acc) do
    parse_import_list_items(rest, [{:operator, op} | acc])
  end

  defp parse_export_list([{:left_paren, _, _} | rest]) do
    parse_export_list_items(rest, [])
  end
  defp parse_export_list_items([{:right_paren, _, _} | rest], acc), do: {:ok, Enum.reverse(acc), rest}
  defp parse_export_list_items([{:comma, _, _} | rest], acc), do: parse_export_list_items(rest, acc)
  defp parse_export_list_items([{:module, _, _} | rest], acc) do
    case parse_module_name(rest, "") do
      {:ok, name, rest2} -> parse_export_list_items(rest2, [{:module, name} | acc])
      err -> err
    end
  end
  defp parse_export_list_items([{:type_kw, _, _}, {:proper_name, _, _, name} | rest], acc) do
    parse_export_list_items(rest, [{:type, name} | acc])
  end
  defp parse_export_list_items([{:class, _, _}, {:proper_name, _, _, name} | rest], acc) do
    parse_export_list_items(rest, [{:class, name} | acc])
  end
  defp parse_export_list_items([{:proper_name, _, _, name} | rest], acc) do
     case rest do
        [{:left_paren, _, _}, {:dot, _, _}, {:dot, _, _}, {:right_paren, _, _} | rest2] ->
          parse_export_list_items(rest2, [{:type_all, name} | acc])
        _ ->
          parse_export_list_items(rest, [{:type, name} | acc])
     end
  end
  defp parse_export_list_items([{:var_ident, _, _, name} | rest], acc) do
    parse_export_list_items(rest, [{:value, name} | acc])
  end
  defp parse_export_list_items([{:operator, _, _, op} | rest], acc) do
    parse_export_list_items(rest, [{:operator, op} | acc])
  end
  defp parse_export_list_items([{:left_paren, _, _}, {:operator, _, _, op}, {:right_paren, _, _} | rest], acc) do
    parse_export_list_items(rest, [{:operator, op} | acc])
  end

  defp parse_type(tokens) do
    case parse_type_atom_or_app(tokens) do
      {:ok, t, [{:arrow, _, _} | rest]} ->
        case parse_type(rest) do
          {:ok, t2, rest2} -> {:ok, %AST.TypeArrow{domain: t, codomain: t2}, rest2}
          err -> err
        end
      {:ok, t, [{:double_arrow, _, _} | rest]} ->
        case parse_type(rest) do
          {:ok, t2, rest2} -> {:ok, %AST.TypeConstrained{constraints: [t], type: t2}, rest2}
          err -> err
        end
      res -> res
    end
  end

  defp parse_type_atom_or_app([{:forall, _, _} | rest]) do
    case parse_type_vars(rest, []) do
      {:ok, vars, [{:dot, _, _} | rest2]} ->
        case parse_type(rest2) do
          {:ok, type, rest3} -> {:ok, %AST.TypeForall{vars: vars, type: type}, rest3}
          err -> err
        end
      err -> err
    end
  end
  defp parse_type_atom_or_app(tokens) do
    case parse_type_atom(tokens) do
      {:ok, t, rest} -> parse_type_app_tail(t, rest)
      err -> err
    end
  end

  defp parse_type_atom([{:var_ident, _, _, name} | rest]), do: {:ok, %AST.TypeVar{name: name}, rest}
  defp parse_type_atom([{:proper_name, _, _, name} | rest]) do
    case rest do
       [{:dot, _, _} | _] ->
          case parse_module_name([{:proper_name, 0, 0, name} | rest], "") do
             {:ok, full_name, rest2} -> {:ok, %AST.TypeConstructor{name: full_name}, rest2}
             _ -> {:ok, %AST.TypeConstructor{name: name}, rest}
          end
       _ -> {:ok, %AST.TypeConstructor{name: name}, rest}
    end
  end
  defp parse_type_atom([{:unit, _, _} | rest]), do: {:ok, %AST.TypeConstructor{name: "Unit"}, rest}
  defp parse_type_atom([{:atom, _, _, _} | rest]), do: {:ok, %AST.TypeConstructor{name: "Atom"}, rest}
  defp parse_type_atom([{:left_paren, _, _} | rest]) do
    case parse_comma_separated(rest, &parse_type/1) do
      {:ok, [t], [{:right_paren, _, _} | rest2]} -> {:ok, t, rest2}
      {:ok, types, [{:right_paren, _, _} | rest2]} -> {:ok, %AST.TypeTuple{elems: types}, rest2}
      err -> err
    end
  end
  defp parse_type_atom([{:left_square, _, _} | rest]) do
    case parse_type(rest) do
      {:ok, t, [{:right_square, _, _} | rest2]} -> {:ok, %AST.TypeList{element: t}, rest2}
      err -> err
    end
  end
  defp parse_type_atom([{:number, _, _, _Val} | rest]), do: {:ok, %AST.TypeConstructor{name: "Number"}, rest}
  defp parse_type_atom([{:left_brace, _, _} | rest]) do
    case parse_comma_separated(rest, &parse_record_field_type/1) do
      {:ok, _fields, [{:right_brace, _, _} | rest2]} -> {:ok, %AST.TypeConstructor{name: "Record"}, rest2}
      err -> err
    end
  end
  defp parse_type_atom(_), do: {:error, :invalid_type_atom}

  defp parse_type_app_tail(f, tokens) do
    case parse_type_atom(tokens) do
      {:ok, arg, rest} -> parse_type_app_tail(%AST.TypeApp{func: f, arg: arg}, rest)
      _ -> {:ok, f, tokens}
    end
  end

  defp parse_type_vars([{:var_ident, _, _, name} | rest], acc), do: parse_type_vars(rest, [name | acc])
  defp parse_type_vars(tokens, acc), do: {:ok, Enum.reverse(acc), tokens}

  defp parse_data_constructors(tokens, acc) do
    case tokens do
      [{:pipe, _, _} | rest] -> parse_data_constructors(rest, acc)
      _ ->
        case rest_is_data_constr?(tokens) do
          true ->
            case parse_data_constructor(tokens) do
              {:ok, constr, rest2} -> parse_data_constructors(rest2, [constr | acc])
              err -> err
            end
          false -> {:ok, Enum.reverse(acc), tokens}
        end
    end
  end
  defp rest_is_data_constr?([{:proper_name, _, _, _} | _]), do: true
  defp rest_is_data_constr?(_), do: false

  defp parse_data_constructor([{:proper_name, _, _, name} | rest]) do
    case parse_data_constructor_args(rest, []) do
      {:ok, args, rest2} -> {:ok, {name, args}, rest2}
      err -> err
    end
  end
  defp parse_data_constructor_args(tokens, acc) do
    case parse_type_atom(tokens) do
      {:ok, t, rest} -> parse_data_constructor_args(rest, [t | acc])
      _ -> {:ok, Enum.reverse(acc), tokens}
    end
  end

  defp find_fundeps([{:var_ident, _, _, _} | _] = tokens) do
     # skip for now
     case Enum.find_index(tokens, fn t -> elem(t, 0) == :where end) do
       nil -> {tokens, []}
       idx -> {Enum.slice(tokens, idx, length(tokens)), []}
     end
  end
  defp find_fundeps(tokens), do: {tokens, []}

  defp flatten_type_app(%AST.TypeApp{func: f, arg: a}, acc), do: flatten_type_app(f, [a | acc])
  defp flatten_type_app(t, acc), do: [t | acc]

  defp parse_expr([{:right_brace, _, _} | rest]), do: parse_expr(rest)
  defp parse_expr(tokens) do
    case tokens do
      [{:backslash, _, _} | rest] ->
        case parse_binder_args(rest, []) do
          {:ok, args, [{:arrow, _, _} | rest2]} ->
            case parse_expr(rest2) do
              {:ok, body, rest3} ->
                # Currying
                lam = Enum.reduce(Enum.reverse(args), body, fn b, acc -> %AST.ExprLam{binder: b, body: acc} end)
                {:ok, lam, rest3}
              err -> err
            end
          err -> err
        end
      [{:let, _, _}, {:left_brace, _, _} | rest] ->
        case parse_decls(rest, []) do
          {:ok, decls, [{:in, _, _} | rest2]} ->
            case parse_expr(rest2) do
              {:ok, body, rest3} -> {:ok, %AST.ExprLet{bindings: decls, body: body}, rest3}
              err -> err
            end
          {:ok, _decls, rest2} -> {:error, :expected_in, rest2}
          err -> err
        end
      [{:if_kw, _, _} | rest] ->
        case parse_expr(rest) do
          {:ok, cond, [{:then_kw, _, _} | rest2]} ->
            case parse_expr(rest2) do
              {:ok, then_br, [{:else_kw, _, _} | rest3]} ->
                 case parse_expr(rest3) do
                   {:ok, else_br, rest4} ->
                     {:ok, %AST.ExprIf{cond: cond, then_br: then_br, else_br: else_br}, rest4}
                   err -> err
                 end
              err -> err
            end
          err -> err
        end
      [{:case, _, _} | rest] ->
        case parse_expr(rest) do
          {:ok, case_expr, [{:of, _, _}, {:left_brace, _, _} | rest2]} ->
            case parse_case_branches(rest2, []) do
              {:ok, branches, rest3} ->
                {:ok, %AST.ExprCase{exprs: [case_expr], branches: branches}, rest3}
              err -> err
            end
          err -> err
        end
      [{:do, _, _}, {:left_brace, _, _} | rest] ->
        case parse_do_statements(rest, []) do
          {:ok, stats, rest2} -> {:ok, {:do, stats}, rest2}
          err -> err
        end
      _ ->
        parse_expr_infix(tokens)
    end
  end

  defp parse_expr_infix(tokens) do
    case parse_expr_atom_or_app(tokens) do
      {:ok, left, rest} ->
        case rest do
          [{:operator, _, _, op} | rest2] -> parse_expr_infix_tail(left, {:op, op}, rest2)
          [{:backtick, _, _}, {:var_ident, _, _, op}, {:backtick, _, _} | rest2] -> parse_expr_infix_tail(left, {:backtick, op}, rest2)
          [{:double_colon, _, _} | rest2] ->
            case parse_type(rest2) do
               {:ok, _type, rest3} ->
                  # Simple type annotation, just return the expression for now
                  {:ok, left, rest3}
               err -> err
            end
          _ -> {:ok, left, rest}
        end
      err -> err
    end
  end

  defp parse_expr_infix_tail(left, op_info, tokens) do
    op_name = case op_info do
      {:op, name} -> name
      {:backtick, name} -> name
    end
    case tokens do
      [{tok_type, _, _} | _] when tok_type in [:backslash, :let, :if_kw, :case, :do] ->
         case parse_expr(tokens) do
           {:ok, right, rest} ->
              {:ok, %AST.ExprApp{func: %AST.ExprApp{func: %AST.ExprVar{name: op_name}, arg: left}, arg: right}, rest}
           err -> err
         end
      [{tok, _, _} | _] when tok in [:right_brace, :right_paren, :right_square, :in, :else_kw, :then_kw, :of, :comma, :semicolon, :pipe, :backtick] ->
        {:ok, left, tokens}
      _ ->
        case parse_expr_atom_or_app(tokens) do
          {:ok, right, rest_t} ->
            new_left = %AST.ExprApp{func: %AST.ExprApp{func: %AST.ExprVar{name: op_name}, arg: left}, arg: right}
            case rest_t do
               [{:double_colon, _, _} | rest_ty] ->
                 case parse_type(rest_ty) do
                   {:ok, _type, rest_ty2} ->
                      case rest_ty2 do
                        [{:operator, _, _, op2} | rest_inf] -> parse_expr_infix_tail(new_left, {:op, op2}, rest_inf)
                        _ -> {:ok, new_left, rest_ty2}
                      end
                   err -> err
                 end
               [{:operator, _, _, op2} | rest_loop] -> parse_expr_infix_tail(new_left, {:op, op2}, rest_loop)
               [{:backtick, _, _}, {:var_ident, _, _, op2}, {:backtick, _, _} | rest_loop] -> parse_expr_infix_tail(new_left, {:backtick, op2}, rest_loop)
               _ -> {:ok, new_left, rest_t}
            end
          _ -> {:ok, left, tokens}
        end
    end
  end

  defp parse_expr_atom_or_app(tokens) do
    case parse_expr_atom(tokens) do
      {:ok, expr, rest} -> parse_expr_app_tail(expr, rest)
      err -> err
    end
  end

  defp parse_expr_app_tail(func_expr, [{tok, _, _} | _] = tokens)
       when tok in [:right_brace, :right_paren, :right_square, :in, :else_kw, :then_kw, :of, :comma, :semicolon, :pipe, :backtick, :double_colon, :operator], do: {:ok, func_expr, tokens}
  defp parse_expr_app_tail(func_expr, tokens) do
    case parse_expr_atom(tokens) do
      {:ok, arg_expr, rest} ->
        parse_expr_app_tail(%AST.ExprApp{func: func_expr, arg: arg_expr}, rest)
      _ ->
        {:ok, func_expr, tokens}
    end
  end

  defp parse_expr_atom(tokens) do
    case parse_expr_atom_base(tokens) do
      {:ok, expr, rest} -> parse_expr_atom_tail(expr, rest)
      err -> err
    end
  end

  defp parse_expr_atom_base([{:var_ident, _, _, name} | rest]) do
    case rest do
       [{:dot, _, _}, {:var_ident, _, _, _field} | _] ->
          case parse_module_name([{:proper_name, 0, 0, name} | rest], "") do
            {:ok, full_name, rest2} -> {:ok, %AST.ExprVar{name: full_name}, rest2}
            _ -> {:ok, %AST.ExprVar{name: name}, rest}
          end
       _ -> {:ok, %AST.ExprVar{name: name}, rest}
    end
  end
  defp parse_expr_atom_base([{:proper_name, _, _, name} | rest]) do
    case rest do
       [{:dot, _, _} | _] ->
          case parse_module_name([{:proper_name, 0, 0, name} | rest], "") do
             {:ok, full_name, rest2} ->
                case rest2 do
                   [{:var_ident, _, _, v} | r] -> {:ok, %AST.ExprVar{name: full_name <> "." <> v}, r}
                   _ -> {:ok, %AST.ExprVar{name: full_name}, rest2}
                end
             _ -> {:ok, %AST.ExprVar{name: name}, rest}
          end
       _ -> {:ok, %AST.ExprVar{name: name}, rest}
    end
  end
  defp parse_expr_atom_base([{:number, _, _, _Val} | rest]), do: {:ok, %AST.ExprVar{name: "literal"}, rest}
  defp parse_expr_atom_base([{:float, _, _, _Val} | rest]), do: {:ok, %AST.ExprVar{name: "literal"}, rest}
  defp parse_expr_atom_base([{:string, _, _, _Val} | rest]), do: {:ok, %AST.ExprVar{name: "literal"}, rest}
  defp parse_expr_atom_base([{:char, _, _, _Val} | rest]), do: {:ok, %AST.ExprVar{name: "literal"}, rest}
  defp parse_expr_atom_base([{:atom, _, _, _Val} | rest]), do: {:ok, %AST.ExprVar{name: "literal"}, rest}
  defp parse_expr_atom_base([{:unit, _, _} | rest]), do: {:ok, %AST.ExprVar{name: "unit"}, rest}
  defp parse_expr_atom_base([{:left_paren, _, _} | rest]) do
    case rest do
      [{:operator, _, _, op}, {:right_paren, _, _} | rest2] -> {:ok, %AST.ExprVar{name: op}, rest2}
      _ ->
        case parse_comma_separated(rest, &parse_expr/1) do
          {:ok, [e], [{:right_paren, _, _} | rest2]} -> {:ok, e, rest2}
          {:ok, exprs, [{:right_paren, _, _} | rest2]} -> {:ok, %AST.ExprTuple{elems: exprs}, rest2}
          {:ok, _, [{tok, line, col} | _]} -> {:error, :expected_right_paren_found, {tok, line, col}}
          {:ok, _, []} -> {:error, :expected_right_paren_eof}
          err -> err
        end
    end
  end
  defp parse_expr_atom_base([{:left_square, _, _} | rest]) do
    case parse_comma_separated(rest, &parse_expr/1) do
      {:ok, exprs, [{:pipe, _, _} | rest_p]} ->
        case parse_expr(rest_p) do
          {:ok, tail, [{:right_square, _, _} | rest2]} -> {:ok, %AST.ExprList{elems: exprs, tail: tail}, rest2}
          err -> err
        end
      {:ok, exprs, [{:right_square, _, _} | rest2]} -> {:ok, %AST.ExprList{elems: exprs}, rest2}
      err -> err
    end
  end
  defp parse_expr_atom_base([{:unit, _, _} | rest]), do: {:ok, %AST.ExprVar{name: "unit"}, rest}
  defp parse_expr_atom_base([{:backslash, _, _} | _] = tokens), do: parse_expr(tokens)
  defp parse_expr_atom_base([{:left_brace, _, _} | rest]), do: parse_record_expr(rest)
  defp parse_expr_atom_base(_), do: {:error, :invalid_expression_atom}

  defp parse_expr_atom_tail(_expr, [{:dot, _line, _col}, {:var_ident, _, _, field} | rest]) do
    # Field access: expr.field
    parse_expr_atom_tail(%AST.ExprVar{name: "access_#{field}"}, rest)
  end
  defp parse_expr_atom_tail(expr, [{:left_brace, _, _} | rest]) do
    # Record update: expr { field = val }
    case parse_record_expr(rest) do
      {:ok, _update, rest2} -> {:ok, expr, rest2}
      err -> err
    end
  end
  defp parse_expr_atom_tail(expr, tokens), do: {:ok, expr, tokens}

  defp parse_case_branches([{:right_brace, _, _} | rest], acc), do: {:ok, Enum.reverse(acc), rest}
  defp parse_case_branches(tokens, acc) do
    case parse_case_branch(tokens) do
      {:ok, branch, rest} ->
        case rest do
          [{:semicolon, _, _} | rest2] -> parse_case_branches(rest2, [branch | acc])
          [{:right_brace, _, _} | _] -> parse_case_branches(rest, [branch | acc])
          [{tok, line, col} | _] -> {:error, :expected_semicolon_or_brace_in_case, {tok, line, col}}
          [] -> {:ok, Enum.reverse([branch | acc]), []}
        end
      err -> err
    end
  end
  defp parse_case_branch(tokens) do
    case parse_binder(tokens) do
      {:ok, binder, [{:arrow, _, _} | rest]} ->
        case parse_expr(rest) do
          {:ok, expr, rest2} -> {:ok, {binder, expr}, rest2}
          err -> err
        end
      err -> err
    end
  end

  defp parse_do_statements([{:right_brace, _, _} | rest], acc) do
    case rest do
      [{:right_brace, _, _} | _] -> parse_do_statements(rest, acc)
      _ -> {:ok, Enum.reverse(acc), rest}
    end
  end
  defp parse_do_statements(tokens, acc) do
    case parse_do_statement(tokens) do
      {:ok, stat, rest} ->
         case rest do
           [{:semicolon, _, _} | rest2] -> parse_do_statements(rest2, [stat | acc])
           [{:right_brace, _, _} | _] -> parse_do_statements(rest, [stat | acc])
           [{tok, _, _} | _] when tok in [:right_paren, :right_square, :in] -> {:ok, Enum.reverse([stat | acc]), rest}
           _ -> {:error, :expected_semicolon_or_brace, rest}
         end
      err -> err
    end
  end
  defp parse_do_statement(tokens) do
    case parse_binder(tokens) do
      {:ok, binder, [{:operator, _, _, "<-"} | rest]} ->
         case parse_expr(rest) do
           {:ok, expr, rest2} -> {:ok, {:bind, binder, expr}, rest2}
           err -> err
         end
      _ ->
        case tokens do
          [{:let, _, _}, {:left_brace, _, _} | rest] ->
            case parse_decls(rest, []) do
              {:ok, decls, rest2} -> {:ok, {:let, decls}, rest2}
              err -> err
            end
          _ ->
            case parse_expr(tokens) do
              {:ok, expr, rest} -> {:ok, {:expr, expr}, rest}
              err -> err
            end
        end
    end
  end

  defp parse_record_field_type([{:var_ident, _, _, _name}, {:double_colon, _, _} | rest]) do
    case parse_type(rest) do
      {:ok, type, rest2} -> {:ok, type, rest2}
      err -> err
    end
  end
  defp parse_record_field_type(_), do: {:error, :invalid_record_field}

  defp parse_record_expr(tokens) do
    case parse_comma_separated(tokens, &parse_record_field_expr/1) do
      {:ok, _fields, [{:right_brace, _, _} | rest]} -> {:ok, %AST.ExprVar{name: "record_literal"}, rest}
      err -> err
    end
  end
  defp parse_record_field_expr([{:var_ident, _, _, _name}, {:=, _, _} | rest]) do
    case parse_expr(rest) do
      {:ok, expr, rest2} -> {:ok, expr, rest2}
      err -> err
    end
  end
  defp parse_record_field_expr([{:var_ident, _, _, name} | rest]) do
    {:ok, %AST.ExprVar{name: name}, rest}
  end
  defp parse_record_field_expr(_), do: {:error, :invalid_record_field}

  defp parse_binder_args(tokens, acc) do
    case parse_binder(tokens) do
      {:ok, b, rest} -> parse_binder_args(rest, [b | acc])
      _ -> {:ok, Enum.reverse(acc), tokens}
    end
  end

  defp parse_lhs_binders([{:operator, _, _, "="} | _] = tokens, acc), do: {:ok, Enum.reverse(acc), tokens}
  defp parse_lhs_binders([tok | _] = tokens, acc) when elem(tok, 0) == :operator, do: {:ok, Enum.reverse(acc), tokens}
  defp parse_lhs_binders(tokens, acc) do
    case parse_binder(tokens) do
      {:ok, b, rest} -> parse_lhs_binders(rest, [b | acc])
      _ -> {:ok, Enum.reverse(acc), tokens}
    end
  end

  defp parse_binder([{:var_ident, _, _, name} | rest]), do: {:ok, %AST.BinderVar{name: name}, rest}
  defp parse_binder([{:proper_name, _, _, name} | rest]) do
    case parse_binder_args(rest, []) do
      {:ok, args, rest2} -> {:ok, %AST.BinderConstructor{name: name, args: args}, rest2}
      err -> err
    end
  end
  defp parse_binder([{:number, _, _, _Val} | rest]), do: {:ok, %AST.BinderVar{name: "literal"}, rest}
  defp parse_binder([{:string, _, _, _Val} | rest]), do: {:ok, %AST.BinderVar{name: "literal"}, rest}
  defp parse_binder([{:char, _, _, _Val} | rest]), do: {:ok, %AST.BinderVar{name: "literal"}, rest}
  defp parse_binder([{:left_paren, _, _} | rest]) do
    case rest do
      [{:operator, _, _, op}, {:right_paren, _, _} | rest2] -> {:ok, %AST.BinderVar{name: op}, rest2}
      _ ->
        case parse_comma_separated(rest, &parse_binder/1) do
          {:ok, [b], [{:right_paren, _, _} | rest2]} -> {:ok, b, rest2}
          {:ok, binders, [{:right_paren, _, _} | rest2]} -> {:ok, %AST.BinderTuple{elems: binders}, rest2}
          err -> err
        end
    end
  end
  defp parse_binder([{:left_square, _, _} | rest]) do
    case parse_comma_separated(rest, &parse_binder/1) do
      {:ok, binders, [{:pipe, _, _} | rest_p]} ->
        case parse_binder(rest_p) do
          {:ok, tail, [{:right_square, _, _} | rest2]} -> {:ok, %AST.BinderList{head: binders, tail: tail}, rest2}
          err -> err
        end
      {:ok, _binders, [{:right_square, _, _} | rest2]} -> {:ok, %AST.BinderList{head: nil, tail: nil}, rest2}
      err -> err
    end
  end
  defp parse_binder([{:unit, _, _} | rest]), do: {:ok, %AST.BinderVar{name: "unit"}, rest}
  defp parse_binder([{:wildcard, _, _} | rest]), do: {:ok, %AST.BinderVar{name: "_"}, rest}
  defp parse_binder(_), do: {:error, :invalid_binder}

  defp parse_guards([{:pipe, _, _} | rest], acc) do
    case parse_expr(rest) do
      {:ok, guard, [{:=, _, _} | rest2]} ->
        case parse_expr(rest2) do
          {:ok, body, rest3} -> parse_guards(rest3, [{guard, body} | acc])
          err -> err
        end
      err -> err
    end
  end
  defp parse_guards(tokens, acc), do: {:ok, Enum.reverse(acc), tokens}


  defp parse_comma_separated(tokens, parse_fn) do
    case parse_fn.(tokens) do
      {:ok, item, [{:comma, _, _} | rest]} ->
        case parse_comma_separated(rest, parse_fn) do
          {:ok, items, rest2} -> {:ok, [item | items], rest2}
          err -> err
        end
      {:ok, item, rest} -> {:ok, [item], rest}
      _ -> {:ok, [], tokens}
    end
  end
end
