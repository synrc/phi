defmodule Mix.Tasks.Phi.Compile do
  use Mix.Task

  @shortdoc "Compile *.phi Files"
  @moduledoc """
  Compiles one or more Phi (.phi) source files (and their dependencies) to BEAM
  bytecode that can be executed directly by the Erlang runtime.

  Usage:
    mix phi.compile [options] <file.hm> [file2.hm ...]

  Options:
    -o, --out DIR      Output directory for .beam files (default: ebin)
    --no-stdlib        Skip loading the standard library
    --stdlib-only      Only compile stdlib, not user files

  After compiling, run your program with:
    erl -pz ebin -noshell -s <module> main -s init stop

  where <module> is the Erlang atom for your module name
  (e.g. module MyApp.Main -> my_app_main).
  """

  def run(args) do
    Mix.Task.run("compile", [])

    {opts, files, _} =
      OptionParser.parse(args,
        switches: [out: :string, no_stdlib: :boolean, stdlib_only: :boolean],
        aliases: [o: :out]
      )

    out_dir = Keyword.get(opts, :out, "ebin")
    File.mkdir_p!(out_dir)
    :code.add_patha(String.to_charlist(out_dir))

    stdlib_only = Keyword.get(opts, :stdlib_only, false)
    skip_stdlib = Keyword.get(opts, :no_stdlib, false)

    if !stdlib_only and files == [] do
      IO.puts("Usage: mix phi.compile [options] <file.hm> [file2.hm ...]")
      IO.puts("")
      IO.puts("  -o, --out DIR   output directory (default: ebin)")
      IO.puts("  --no-stdlib     skip stdlib loading (faster if already compiled)")
      IO.puts("  --stdlib-only   compile stdlib to ebin/ and exit")
      System.halt(1)
    end

    stdlib_env =
      if skip_stdlib do
        IO.puts("Skipping stdlib (--no-stdlib). Loading existing beams from #{out_dir}/...")
        load_existing_beams(out_dir)
        Phi.Typechecker.Env.new()
      else
        IO.write("Compiling standard library...")

        stdlib_files =
          Path.wildcard("priv/**/*.hm")
          |> Enum.sort_by(fn path ->
            base = Path.basename(path)

            cond do
              path == "lib/Control/Monad.hm" -> {0, path}
              base == "Foreign.hm" -> {1, path}
              path == "lib/Data/Ring.hm" -> {2, path}
              path == "lib/Data/Semiring.hm" -> {3, path}
              path == "lib/Data/Semigroup.hm" -> {4, path}
              path == "lib/Data/Eq.hm" -> {5, path}
              path == "lib/Data/Ord.hm" -> {6, path}
              path == "lib/Data/Unit.hm" -> {7, path}
              path == "lib/Data/Maybe.hm" -> {8, path}
              path == "lib/Data/Tuple.hm" -> {9, path}
              path == "lib/Data/String.hm" -> {10, path}
              path == "lib/Data/List.hm" -> {11, path}
              path == "lib/Data/Show.hm" -> {12, path}
              path == "lib/Data/Read.hm" -> {13, path}
              String.starts_with?(path, "lib/Data/") -> {20, path}
              String.starts_with?(path, "lib/Control/") -> {3, path}
              String.starts_with?(path, "lib/System/") -> {4, path}
              true -> {5, path}
            end
          end)

        env = load_stdlib(stdlib_files, out_dir)
        IO.puts(" done (#{map_size(env.bindings)} bindings).")
        env
      end

    if stdlib_only do
      IO.puts("Stdlib written to #{out_dir}/. Done.")
    else
      IO.puts("Compiling #{length(files)} user file(s)...")
      {_user_env, compiled_mods} = multi_pass(files, stdlib_env, out_dir, 1)
      summarise(compiled_mods, out_dir)
    end
  end

  # ---------------------------------------------------------------------------
  # Stdlib loading — multi-pass until stable, write beams to disk
  # ---------------------------------------------------------------------------

  defp load_stdlib(stdlib_files, out_dir) do
    do_passes(stdlib_files, Phi.Typechecker.Env.new(), out_dir, 1)
  end

  defp do_passes(files, env, out_dir, pass, successful_files \\ %{}) do
    # Normalize file paths to absolute paths for consistent tracking
    normalized_files = Enum.map(files, fn f -> Path.expand(f) end)

    {successes, failures, new_env, new_successful} =
      Enum.reduce(normalized_files, {0, [], env, successful_files}, fn file,
                                                                       {ok, fails, acc_env, succ} ->
        # Skip if already compiled successfully
        if Map.has_key?(succ, file) do
          {ok, fails, acc_env, succ}
        else
          case compile_and_write(file, acc_env, out_dir) do
            {:ok, mod, next_env} ->
              {ok + 1, fails, next_env, Map.put(succ, file, mod)}

            {:error, _reason, error_env} ->
              # Merge error_env with acc_env to preserve all accumulated bindings
              merged_env = Phi.Typechecker.Env.merge(acc_env, error_env)
              {ok, [file | fails], merged_env, succ}
          end
        end
      end)

    failure_names = Enum.take(failures, 5) |> Enum.map(&Path.basename/1) |> Enum.join(", ")

    IO.puts(
      "Pass #{pass}: #{successes} successes, #{length(failures)} failures [#{failure_names}...], env: #{map_size(new_env.bindings)} keys"
    )

    cond do
      failures == [] ->
        new_env

      successes == 0 ->
        new_env

      true ->
        do_passes(Enum.reverse(failures), new_env, out_dir, pass + 1, new_successful)
    end
  end

  # ---------------------------------------------------------------------------
  # User-file compilation — multi-pass with progress reporting
  # ---------------------------------------------------------------------------

  defp multi_pass(files, env, _out_dir, pass) when pass > 3 do
    # Stop after 3 passes to prevent infinite loops
    IO.puts("  Stopped after 3 passes, #{length(files)} files remaining")
    {env, []}
  end

  defp multi_pass(files, env, out_dir, pass) do
    IO.puts(
      "  multi_pass #{pass}: compiling #{length(files)} files, env has #{map_size(env.bindings)} keys"
    )

    {successes, failures, new_env, compiled} =
      Enum.reduce(files, {0, [], env, []}, fn file, {ok, fails, acc_env, mods} ->
        case compile_and_write(file, acc_env, out_dir) do
          {:ok, mod, next_env} ->
            {ok + 1, fails, next_env, [mod | mods]}

          {:error, _reason, error_env} ->
            # Merge error_env with acc_env to preserve all accumulated bindings
            merged_env = Phi.Typechecker.Env.merge(acc_env, error_env)
            {ok, [file | fails], merged_env, mods}
        end
      end)

    IO.puts("  multi_pass #{pass}: #{successes} successes, #{length(failures)} failures")

    cond do
      failures == [] ->
        {new_env, compiled}

      successes == 0 ->
        Enum.each(Enum.reverse(failures), fn file ->
          case compile_and_write(file, new_env, out_dir) do
            {:error, reason, _} -> IO.puts("  ✗ #{file}: #{format_reason(reason)}")
            _ -> nil
          end
        end)

        {new_env, compiled}

      true ->
        {next_env, more} = multi_pass(Enum.reverse(failures), new_env, out_dir, pass + 1)
        {next_env, compiled ++ more}
    end
  end

  # ---------------------------------------------------------------------------
  # Core: compile one file, write beam to disk, load into VM
  # ---------------------------------------------------------------------------

  defp compile_and_write(file, env, out_dir) do
    try do
      case Phi.Compiler.compile_module(File.read!(file), source_path: file, env: env) do
        {:ok, mod, bin, _ast, new_env} ->
          beam_path = Path.join(out_dir, "#{mod}.beam")
          File.write!(beam_path, bin)
          :code.load_binary(mod, String.to_charlist(beam_path), bin)
          {:ok, mod, new_env}

        {:error, reason} ->
          # Even on error, return the env which may have bindings from successful typechecking
          {:error, reason, env}
      end
    rescue
      e -> {:error, e, env}
    catch
      kind, val -> {:error, {kind, val}, env}
    end
  end

  # ---------------------------------------------------------------------------
  # Load pre-compiled beams from disk into the running VM
  # ---------------------------------------------------------------------------

  defp load_existing_beams(dir) do
    Path.wildcard(Path.join(dir, "*.beam"))
    |> Enum.each(fn path ->
      case File.read(path) do
        {:ok, bin} ->
          case :beam_lib.chunks(bin, [:atoms]) do
            {:ok, {mod, _}} -> :code.load_binary(mod, String.to_charlist(path), bin)
            _ -> :ok
          end

        _ ->
          :ok
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Post-compile summary with run instructions
  # ---------------------------------------------------------------------------

  defp summarise(compiled_mods, out_dir) do
    beam_count = Path.wildcard(Path.join(out_dir, "*.beam")) |> length()
    IO.puts("\n#{beam_count} .beam files in #{out_dir}/")
    IO.puts("")

    mains = Enum.filter(compiled_mods, &:erlang.function_exported(&1, :main, 0))

    if mains == [] do
      IO.puts("To load in the Erlang shell:")
      IO.puts("  erl -pz #{out_dir}")
    else
      IO.puts("To run:")

      Enum.each(mains, fn mod ->
        mod_str = to_string(mod)

        IO.puts(
          "  erl -pz #{out_dir} -noshell -eval \"('#{mod_str}':main())()\" -eval \"init:stop()\""
        )
      end)
    end
  end

  defp format_reason({:unresolved, name}), do: "unresolved: #{name}"

  defp format_reason({:erl_compile, errors, _}),
    do: errors |> Enum.flat_map(fn {_, es} -> Enum.map(es, &inspect/1) end) |> Enum.join(", ")

  defp format_reason(r), do: inspect(r) |> String.slice(0, 100)
end
