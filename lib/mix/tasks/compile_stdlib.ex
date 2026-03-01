defmodule Mix.Tasks.CompileStdlib do
  @moduledoc """
  Attempts to compile all 123 Hamler .hm standard library files.
  Reports pass/fail for each file and a summary at the end.

  Usage: mix compile_stdlib
  """
  use Mix.Task

  @shortdoc "Compile all Hamler stdlib .hm files and report results"

  def run(_args) do
    Mix.Task.run("compile", [])

    files = Path.wildcard("lib/**/*.hm") |> Enum.sort()
    total = length(files)

    IO.puts("\n#{IO.ANSI.bright()}=== Phi Stdlib Compilation Report ===#{IO.ANSI.reset()}")
    IO.puts("Found #{total} .hm files\n")

    # Compile each file independently with a fresh env (no dependency tracking)
    # Then compile again with a shared env to test dependency resolution
    results = compile_all_independent(files)

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

    # Group failures by error type
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

  defp compile_all_independent(files) do
    Enum.map(files, fn file ->
      result = try do
        source = File.read!(file)
        case Phi.Compiler.compile_module(source, source_path: file) do
          {:ok, _mod, _binary, _ast, _env} -> {:ok, ""}
          {:error, reason} -> {:error, inspect(reason)}
          other -> {:error, inspect(other)}
        end
      rescue
        e -> {:error, "#{inspect(e.__struct__)}: #{Exception.message(e)}"}
      catch
        kind, value -> {:error, "#{kind}: #{inspect(value)}"}
      end

      case result do
        {:ok, _} ->
          IO.puts("  #{IO.ANSI.green()}✓#{IO.ANSI.reset()} #{file}")
          {file, :ok, ""}
        {:error, reason} ->
          IO.puts("  #{IO.ANSI.red()}✗#{IO.ANSI.reset()} #{file}")
          {file, :error, reason}
      end
    end)
  end

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
