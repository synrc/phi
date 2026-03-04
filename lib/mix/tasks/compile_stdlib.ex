defmodule Mix.Tasks.CompileStdlib do
  @moduledoc """
  Attempts to compile all 123 Hamler .hm standard library files in topological
  order (dependencies first), sharing the accumulated type environment across
  compilations.

  Usage: mix compile_stdlib
  """
  use Mix.Task

  @shortdoc "Compile all Hamler stdlib .hm files in dependency order"

  def run(_args) do
    Mix.Task.run("compile", [])

    files = Path.wildcard("lib/**/*.hm") |> Enum.sort()
    total = length(files)

    IO.puts("\n#{IO.ANSI.bright()}=== Phi Stdlib Compilation Report ===#{IO.ANSI.reset()}")
    IO.puts("Found #{total} .hm files\n")

    # Build dependency graph and sort topologically
    module_infos = Enum.map(files, &extract_module_info/1)
    name_to_file = Map.new(module_infos, fn {file, mod, _} -> {mod, file} end)
    sorted_files = topological_sort(module_infos, name_to_file)

    IO.puts("Compiling in topological order...\n")

    results = compile_in_order(sorted_files, name_to_file)

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
  # Dependency extraction
  # ---------------------------------------------------------------------------

  defp extract_module_info(file) do
    source = File.read!(file)
    stripped = strip_comments(source)

    mod_name =
      case Regex.run(~r/^module\s+([\w.]+)/m, stripped, capture: :all_but_first) do
        [name] -> name
        _ -> file_to_module_name(file)
      end

    imports =
      Regex.scan(~r/^import\s+([\w.]+)/m, stripped, capture: :all_but_first)
      |> List.flatten()

    {file, mod_name, imports}
  end

  # Remove block comments {- ... -} and line comments -- ...
  # Handles one level of nesting; sufficient for import scanning.
  defp strip_comments(source) do
    source
    |> remove_block_comments()
    |> String.replace(~r/--[^\n]*/, "")
  end

  defp remove_block_comments(source) do
    case Regex.run(~r/\{-/, source, return: :index) do
      nil ->
        source

      [{start, _}] ->
        before = String.slice(source, 0, start)
        rest = String.slice(source, start, String.length(source))

        case Regex.run(~r/-\}/, rest, return: :index) do
          nil ->
            # Unclosed block comment — drop to end
            before

          [{close_start, close_len}] ->
            after_close = String.slice(rest, close_start + close_len, String.length(rest))
            before <> remove_block_comments(after_close)
        end
    end
  end

  # Fallback: derive module name from file path (e.g. lib/Data/List.hm -> Data.List)
  defp file_to_module_name(file) do
    file
    |> String.replace_prefix("lib/", "")
    |> String.replace_suffix(".hm", "")
    |> String.replace("/", ".")
  end

  # ---------------------------------------------------------------------------
  # Topological sort — Kahn's algorithm
  # ---------------------------------------------------------------------------

  defp topological_sort(module_infos, name_to_file) do
    # Build: file -> list of dependency files (only those present in stdlib)
    edges =
      Map.new(module_infos, fn {file, _mod, imports} ->
        deps =
          imports
          |> Enum.map(&Map.get(name_to_file, &1))
          |> Enum.reject(&is_nil/1)
          |> Enum.reject(&(&1 == file))

        {file, deps}
      end)

    all_files = Enum.map(module_infos, fn {file, _, _} -> file end)

    # in-degree per file = number of dependencies it has (incoming edges)
    in_degree = Map.new(all_files, fn file -> {file, length(edges[file] || [])} end)

    # reverse edges: dep -> list of files that depend on dep
    # When dep is compiled, decrement in-degree of each dependent file
    rev_edges =
      Enum.reduce(all_files, %{}, fn file, acc ->
        Enum.reduce(edges[file] || [], acc, fn dep, a ->
          Map.update(a, dep, [file], &[file | &1])
        end)
      end)

    queue = all_files |> Enum.filter(&(Map.get(in_degree, &1, 0) == 0)) |> :queue.from_list()

    kahn(queue, in_degree, rev_edges, [])
  end

  defp kahn(queue, in_degree, rev_edges, acc) do
    case :queue.out(queue) do
      {:empty, _} ->
        # Cycle or remaining — append anything unvisited at the end
        remaining = in_degree |> Enum.filter(fn {_, d} -> d > 0 end) |> Enum.map(&elem(&1, 0))
        if remaining != [] do
          IO.puts(
            "#{IO.ANSI.yellow()}Warning: cycle detected among #{length(remaining)} file(s), appending at end#{IO.ANSI.reset()}"
          )
        end

        Enum.reverse(acc) ++ remaining

      {{:value, file}, rest_queue} ->
        dependents = Map.get(rev_edges, file, [])

        {new_queue, new_in_degree} =
          Enum.reduce(dependents, {rest_queue, in_degree}, fn dep, {q, deg} ->
            new_deg = Map.update!(deg, dep, &(&1 - 1))
            q2 = if new_deg[dep] == 0, do: :queue.in(dep, q), else: q
            {q2, new_deg}
          end)

        kahn(new_queue, new_in_degree, rev_edges, [file | acc])
    end
  end

  # ---------------------------------------------------------------------------
  # Compilation with shared environment
  # ---------------------------------------------------------------------------

  defp compile_in_order(sorted_files, name_to_file) do
    # We also need the file->mod map for printing
    file_to_mod =
      Map.new(name_to_file, fn {mod, file} -> {file, mod} end)

    {results, _final_env} =
      Enum.reduce(sorted_files, {[], Phi.Typechecker.Env.new()}, fn file, {acc, env} ->
        {status, reason, new_env} = compile_file(file, env)

        label =
          if status == :ok,
            do: "#{IO.ANSI.green()}✓#{IO.ANSI.reset()}",
            else: "#{IO.ANSI.red()}✗#{IO.ANSI.reset()}"

        mod_hint = Map.get(file_to_mod, file, "")
        IO.puts("  #{label} #{file}#{if mod_hint != "", do: " (#{mod_hint})", else: ""}")

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
