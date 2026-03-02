defmodule Mix.Tasks.Phi.Test do
  @moduledoc """
  Compiles the Hamler stdlib (lib/**/*.hm) and then the Hamler test suite
  (tests/**/*.hm) using the same multi-pass environment accumulation strategy,
  then executes the test main function.

  Usage: mix phi.test
  """
  use Mix.Task

  @shortdoc "Run Hamler native QuickCheck tests"

  def run(_args) do
    Mix.Task.run("compile", [])
    File.mkdir_p!("ebin")
    :code.add_patha(~c"ebin")

    IO.puts("\n=== Phi Native Test Runner ===\n")

    # Step 1: compile stdlib
    IO.puts("--- Compiling stdlib (lib/**/*.hm) ---")
    lib_files = Path.wildcard("lib/**/*.hm")
    IO.puts("Found #{length(lib_files)} stdlib files.")
    stdlib_env = multi_pass_compile(lib_files, Phi.Typechecker.Env.new(), 1, "lib")

    # Step 2: compile test suite
    IO.puts("\n--- Compiling test suite (tests/**/*.hm) ---")
    test_files = Path.wildcard("tests/**/*.hm")
    IO.puts("Found #{length(test_files)} test files.")
    _test_env = multi_pass_compile(test_files, stdlib_env, 1, "tests")

    # Step 3: run tests
    IO.puts("\n--- Running tests ---\n")
    IO.puts("Calling run_test_main()...")
    run_test_main()
    IO.puts("run_test_main() finished.")
  end

  defp multi_pass_compile([], _env, _pass, _label) do
    IO.puts("All compiled.")
    Phi.Typechecker.Env.new()
  end

  defp multi_pass_compile(files, env, pass, label) do
    IO.puts("\n  Pass #{pass} (#{length(files)} remaining)...")

    # Each element of `failing`: {file_path, last_reason}
    {failing, new_env, successes} =
      Enum.reduce(files, {[], env, 0}, fn file, {rem, acc_env, ok} ->
        IO.puts("    Compiling #{file}...")
        source = File.read!(file)

        result =
          try do
            Phi.Compiler.compile_module(source, source_path: file, env: acc_env)
          rescue
            e -> {:error, e}
          catch
            kind, val -> {:error, {kind, val}}
          end

        case result do
          {:ok, mod, beam, _ast, next_env} ->
            File.write!("ebin/#{mod}.beam", beam)
            Phi.Compiler.load_module(mod, beam)
            {rem, next_env, ok + 1}

          {:error, reason} ->
            formatted = format_reason(reason)
            {rem ++ [{file, formatted}], acc_env, ok}

          other ->
            formatted = format_reason(other)
            {rem ++ [{file, formatted}], acc_env, ok}
        end
      end)

    remaining_files = Enum.map(failing, &elem(&1, 0))
    IO.puts("  Succeeded: #{successes}, still failing: #{length(failing)}")

    cond do
      successes == 0 ->
        IO.puts("\n  No progress in pass #{pass}. Giving up on #{length(failing)} files:")
        Enum.each(failing, fn {file, reason_str} ->
          IO.puts("    FAIL #{Path.relative_to_cwd(file)}: #{reason_str}")
        end)
        new_env

      remaining_files == [] ->
        IO.puts("  All #{label} files compiled successfully!")
        new_env

      true ->
        multi_pass_compile(remaining_files, new_env, pass + 1, label)
    end
  end

  defp format_reason({:erl_compile, errors, _warnings}) do
    errors
    |> List.flatten()
    |> Enum.map(fn
      {_file, errs} ->
        Enum.map(errs, fn {_line, _mod, desc} -> :io_lib.format("~p", [desc]) |> to_string() end)

      other ->
        inspect(other)
    end)
    |> List.flatten()
    |> Enum.join("; ")
  end

  defp format_reason(reason) do
    try do
      cond do
        is_exception(reason) -> Exception.message(reason)
        is_tuple(reason) and tuple_size(reason) > 0 -> "Error tuple: #{inspect(elem(reason, 0))}"
        true -> "Unknown error"
      end
    rescue
      _ -> "Formatting error failed"
    end
  end

  defp run_test_main do
    try do
      # tests/Test.hm → module :test → main/0 returns IO ()
      io_action = :test.main()
      if is_function(io_action, 0) do
        io_action.()
      else
        IO.puts("(result: #{inspect(io_action)})")
      end
    rescue
      e ->
        case e do
          %ErlangError{original: {:undef, [{:test, :main, _, _} | _]}} ->
            IO.puts("Could not find test:main/0 — was tests/Test.hm compiled?")
          _ ->
            IO.puts("Error running tests: #{Exception.message(e)}")
            IO.puts(Exception.format(:error, e, __STACKTRACE__))
        end
    catch
      kind, val ->
        IO.puts("Error: #{kind}: #{inspect(val)}")
    end
  end
end
