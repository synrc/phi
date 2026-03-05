defmodule Mix.Tasks.Phi.Base do
  @moduledoc """
  Compiles all Phi .phi standard library files, sharing the accumulated type
  environment across compilations.

  Usage: mix phi.base
  """
  use Mix.Task

  @shortdoc "Compile Base Library"

  def run(_args) do
    Mix.Task.run("compile", [])

    files = Path.wildcard("priv/**/*.phi") |> Enum.sort()
    total = length(files)

    IO.puts("\n#{IO.ANSI.bright()}=== Phi priv Compilation Report ===#{IO.ANSI.reset()}")
    IO.puts("Found #{total} .phi files\n")

    IO.puts("Compiling...\n")

    results = compile_in_order(files)

    # Summary
    passed = Enum.count(results, fn {_, status, _} -> status == :ok end)
    failed = total - passed

    IO.puts("\n#{IO.ANSI.bright()}=== Summary ===#{IO.ANSI.reset()}")
    IO.puts("#{IO.ANSI.green()}PASS: #{passed}/#{total}#{IO.ANSI.reset()}")

    if failed > 0 do
      IO.puts("#{IO.ANSI.red()}FAIL: #{failed}/#{total}#{IO.ANSI.reset()}")
      IO.puts("\n#{IO.ANSI.bright()}Failed files:#{IO.ANSI.reset()}")

      results
      |> Enum.filter(fn {_, status, _} -> status == :error end)
      |> Enum.each(fn {file, _, reason} ->
        short_reason = reason |> String.slice(0, 120) |> String.replace("\n", " ")
        IO.puts("  #{IO.ANSI.red()}✗#{IO.ANSI.reset()} #{file}: #{short_reason}")
      end)
    end

    IO.puts("")

    if failed > 0 do
      IO.puts("#{IO.ANSI.bright()}Error categories:#{IO.ANSI.reset()}")

      results
      |> Enum.filter(fn {_, status, _} -> status == :error end)
      |> Enum.map(fn {_, _, reason} -> categorize_error(reason) end)
      |> Enum.frequencies()
      |> Enum.sort_by(fn {_, count} -> -count end)
      |> Enum.each(fn {category, count} ->
        IO.puts("  #{count}x #{category}")
      end)

      IO.puts("")
    end
  end

  # ---------------------------------------------------------------------------
  # Compilation with shared environment
  # ---------------------------------------------------------------------------

  defp compile_in_order(files) do
    {results, _final_env} =
      Enum.reduce(files, {[], Phi.Typechecker.Env.new()}, fn file, {acc, env} ->
        {status, reason, new_env} = compile_file(file, env)

        label =
          if status == :ok,
            do: "#{IO.ANSI.green()}✓#{IO.ANSI.reset()}",
            else: "#{IO.ANSI.red()}✗#{IO.ANSI.reset()}"

        mod_hint =
          file
          |> String.replace_prefix("priv/", "")
          |> String.replace_suffix(".phi", "")
          |> String.replace("/", ".")

        IO.puts("  #{label} #{file} (#{mod_hint})")

        {[{file, status, reason} | acc], new_env}
      end)

    Enum.reverse(results)
  end

  defp compile_file(file, env) do
    result =
      try do
        source = File.read!(file)

        case Phi.Compiler.compile_module(source, source_path: file, env: env) do
          {:ok, mod, bin, _ast, new_env} ->
            Phi.Compiler.load_module(mod, bin)
            {:ok, "", new_env}

          {:error, reason} ->
            {:error, inspect(reason), env}

          other ->
            {:error, inspect(other), env}
        end
      rescue
        e -> {:error, "#{inspect(e.__struct__)}: #{Exception.message(e)}", env}
      catch
        kind, value -> {:error, "#{kind}: #{inspect(value)}", env}
      end

    case result do
      {:ok, _, new_env} -> {:ok, "", new_env}
      {:error, reason, kept_env} -> {:error, reason, kept_env}
    end
  end

  # ---------------------------------------------------------------------------
  # Error categorisation
  # ---------------------------------------------------------------------------

  defp categorize_error(reason) do
    cond do
      String.contains?(reason, "FunctionClauseError") -> "FunctionClauseError"
      String.contains?(reason, "MatchError") -> "MatchError"
      String.contains?(reason, "erl_compile") -> "Erlang compile error"
      String.contains?(reason, "erl_lint") -> "Erlang lint error"
      String.contains?(reason, "parse") or String.contains?(reason, "Parser") -> "Parse error"
      String.contains?(reason, "KeyError") -> "KeyError"
      String.contains?(reason, "UndefinedFunctionError") -> "UndefinedFunctionError"
      String.contains?(reason, "CaseClauseError") -> "CaseClauseError"
      String.contains?(reason, "BadMapError") -> "BadMapError"
      true -> "Other"
    end
  end
end
