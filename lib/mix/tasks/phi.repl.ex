defmodule Mix.Tasks.Phi.Repl do
  use Mix.Task

  @shortdoc "Starts a Phi REPL"
  def run(_) do
    IO.puts("Phi Interactive REPL")
    IO.puts("Type :quit to exit")

    files = Path.wildcard("lib/**/*.hm")
    IO.puts("Loading Standard Library (#{length(files)} files)...")

    base_env = load_files(files, Phi.Typechecker.Env.new(), 1)

    IO.puts("Library loaded.")

    # Initialize Erlang shell so printing and error tracing behaves nicely
    :application.start(:sasl)

    repl_loop(base_env, [], 1)
  end

  defp repl_loop(env, imports, line_num) do
    input = IO.gets("ϕ> ")
    cond do
      input == :eof -> :ok
      input in ["", "\n"] -> repl_loop(env, imports, line_num)
      String.trim(input) == ":quit" -> :ok
      String.starts_with?(String.trim(input), "import ") ->
        new_imports = imports ++ [String.trim(input)]
        repl_loop(env, new_imports, line_num)
      true ->
        env = evaluate_line(input, env, imports, line_num)
        repl_loop(env, imports, line_num + 1)
    end
  end

  defp evaluate_line(input, env, imports, line_num) do
    # Try to parse as an expression or a declaration
    imports_str = Enum.join(imports, "\n")
    source = "module Repl#{line_num} where\nimport Prelude\n#{imports_str}\nrepl_val = " <> input

    try do
      case Phi.Compiler.compile_module(source, source_path: "repl.hm", env: env) do
        {:ok, mod, bin, _, new_env} ->
          :code.load_binary(mod, ~c"#{mod}.beam", bin)
          # Execute the repl_val
          mod_name = String.to_atom("repl#{line_num}")
          if :erlang.function_exported(mod_name, :repl_val, 0) do
            val = apply(mod_name, :repl_val, [])
            # Evaluate IO actions (0-arity closures)
            if is_function(val, 0) do
              result = val.()
              IO.inspect(result)
            else
              IO.puts(inspect(val))
            end
          else
            IO.puts("No value bounds produced.")
          end
          new_env
        err ->
          IO.inspect(err, label: "Compiler Error")
          env
      end
    rescue
      e ->
        IO.puts("Exception:")
        IO.puts(Exception.format(:error, e, __STACKTRACE__))
        env
    catch
      :exit, e ->
        IO.puts("Erlang Exit: #{inspect(e)}")
        env
      :error, e ->
        IO.puts("Erlang Error: #{inspect(e)}")
        env
    end
  end

  defp load_files([], env, _pass), do: env
  defp load_files(files, env, pass) do
    IO.puts("  Pass #{pass} (#{length(files)} remaining)...")

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

    IO.puts("  Succeeded: #{successes}, still failing: #{length(failures)}")

    cond do
      length(failures) == 0 ->
        IO.puts("  All files compiled successfully!")
        new_env
      successes == 0 ->
        IO.puts("  No progress in pass #{pass}. Giving up on #{length(failures)} files:")
        Enum.each(failures, fn {f, err} -> IO.puts("    FAIL #{f}: #{inspect(err)}") end)
        new_env
      true ->
        load_files(Enum.map(failures, &elem(&1, 0)), new_env, pass + 1)
    end
  end
end
