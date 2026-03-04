defmodule Mix.Tasks.Phi.Repl do
  use Mix.Task

  @shortdoc "Starts a Phi REPL"
  def run(_) do
    IO.puts("Phi Interactive REPL")
    IO.puts("Type :quit to exit, :reset to clear bindings")

    files = Path.wildcard("lib/**/*.hm")
    IO.puts("Loading Standard Library (#{length(files)} files)...")

    base_env = load_files(files, Phi.Typechecker.Env.new(), 1)

    IO.puts("Library loaded. Ready.")

    repl_loop(base_env, [], [], 1)
  end

  defp repl_loop(env, imports, prev_decls, line_num) do
    input = IO.gets("ϕ> ")
    cond do
      input == :eof ->
        :ok
      input in ["", "\n"] ->
        repl_loop(env, imports, prev_decls, line_num)
      String.trim(input) == ":quit" ->
        :ok
      String.trim(input) == ":reset" ->
        IO.puts("Bindings cleared.")
        repl_loop(env, imports, [], line_num)
      String.starts_with?(String.trim(input), "import ") ->
        new_imports = imports ++ [String.trim(input)]
        repl_loop(env, new_imports, prev_decls, line_num)
      true ->
        {new_env, new_decls} = evaluate_line(input, env, imports, prev_decls, line_num)
        repl_loop(new_env, imports, new_decls, line_num + 1)
    end
  end

  defp evaluate_line(input, env, imports, prev_decls, line_num) do
    imports_str = Enum.join(imports, "\n")
    decls_str = if prev_decls == [], do: "", else: Enum.join(prev_decls, "\n") <> "\n"
    trimmed = String.trim(input)

    # Try as expression first: repl_val = <input>
    expr_source = "module Repl#{line_num} where\nimport Prelude\n#{imports_str}\n#{decls_str}repl_val = #{trimmed}"

    case try_compile(expr_source, env) do
      {:ok, mod, bin, new_env} ->
        :code.load_binary(mod, ~c"#{mod}.beam", bin)
        mod_atom = module_atom(line_num)
        if :erlang.function_exported(mod_atom, :repl_val, 0) do
          result = try_eval(mod_atom, :repl_val)
          case result do
            {:ok, val} -> print_value(val)
            {:error, e} -> IO.puts("Runtime error: #{inspect(e)}")
          end
        end
        {new_env, prev_decls}

      {:error, _} ->
        # Try as declaration(s)
        decl_source = "module Repl#{line_num} where\nimport Prelude\n#{imports_str}\n#{decls_str}#{trimmed}\nrepl_val = unit"

        case try_compile(decl_source, env) do
          {:ok, _mod, bin, new_env} ->
            mod_atom = module_atom(line_num)
            :code.load_binary(mod_atom, ~c"#{mod_atom}.beam", bin)
            # Extract defined names for user feedback
            defined = extract_defined_names(trimmed)
            if defined != [], do: IO.puts("Defined: #{Enum.join(defined, ", ")}")
            {new_env, prev_decls ++ [trimmed]}

          {:error, reason} ->
            IO.puts("Error: #{format_error(reason)}")
            {env, prev_decls}
        end
    end
  end

  defp try_compile(source, env) do
    try do
      case Phi.Compiler.compile_module(source, source_path: "repl.hm", env: env) do
        {:ok, mod, bin, _ast, new_env} -> {:ok, mod, bin, new_env}
        {:error, reason} -> {:error, reason}
      end
    rescue
      e -> {:error, e}
    catch
      kind, val -> {:error, {kind, val}}
    end
  end

  defp try_eval(mod, fun) do
    try do
      val = apply(mod, fun, [])
      val = if is_function(val, 0), do: val.(), else: val
      {:ok, val}
    rescue
      e -> {:error, e}
    catch
      kind, val -> {:error, {kind, val}}
    end
  end

  defp module_atom(line_num), do: String.to_atom("repl#{line_num}")

  defp print_value(:unit), do: :ok
  defp print_value(val) when is_binary(val), do: IO.puts(val)
  defp print_value(val), do: IO.inspect(val)

  defp extract_defined_names(input) do
    input
    |> String.split("\n")
    |> Enum.flat_map(fn line ->
      case Regex.run(~r/^([a-z_][a-zA-Z0-9_']*)\s*(?:[a-z_][a-zA-Z0-9_']*\s*)*=/, String.trim(line)) do
        [_, name] -> [name]
        _ -> []
      end
    end)
    |> Enum.uniq()
  end

  defp format_error({:erl_compile, errors, _warnings}) do
    errors
    |> Enum.flat_map(fn {_, errs} -> Enum.map(errs, &inspect/1) end)
    |> Enum.join(", ")
  end
  defp format_error(reason), do: inspect(reason)

  defp load_files([], env, _pass), do: env
  defp load_files(files, env, pass) do
    IO.write("  Pass #{pass}...")

    {successes, failures, new_env} =
      Enum.reduce(files, {0, [], env}, fn file, {succ, fails, current_env} ->
        try do
          case Phi.Compiler.compile_module(File.read!(file), source_path: file, env: current_env) do
            {:ok, mod, bin, _, e2} ->
              :code.load_binary(mod, ~c"#{mod}.beam", bin)
              {succ + 1, fails, e2}
            {:error, err} ->
              {succ, [{file, err} | fails], current_env}
          end
        rescue
          _ -> {succ, [{file, :exception} | fails], current_env}
        catch
          _, _ -> {succ, [{file, :exception} | fails], current_env}
        end
      end)

    IO.puts(" #{successes} ok, #{length(failures)} remaining")

    cond do
      length(failures) == 0 ->
        new_env
      successes == 0 ->
        IO.puts("  Stuck on #{length(failures)} files (run mix phi.compile for details)")
        new_env
      true ->
        load_files(Enum.map(failures, &elem(&1, 0)), new_env, pass + 1)
    end
  end
end
