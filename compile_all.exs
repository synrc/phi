defmodule CompileAll do
  def run do
    all_files = Path.wildcard("lib/**/*.hm")
    IO.puts "Found #{length(all_files)} files."

    File.mkdir_p!("ebin")

    # Pass-based compilation to handle dependencies automatically
    do_compile(all_files, Phi.Typechecker.Env.new(), 1)
  end

  defp do_compile([], _env, _pass), do: IO.puts "All modules compiled successfully!"
  defp do_compile(remaining, env, pass) do
    IO.puts "\n--- PASS #{pass} (#{length(remaining)} remaining) ---"

    {new_remaining, new_env, success_count} =
      Enum.reduce(remaining, {[], env, 0}, fn file, {rem, acc_env, count} ->
        source = File.read!(file)
        # Suppress noisy logs for the multi-pass run
        case Phi.Compiler.compile_module(source, source_path: file, env: acc_env) do
          {:ok, mod, beam, _ast, next_env} ->
            File.write!("ebin/#{mod}.beam", beam)
            IO.puts "SUCCESS: #{file} -> #{mod}"
            {rem, next_env, count + 1}
          {:error, _reason} ->
            {rem ++ [file], acc_env, count}
        end
      end)

    cond do
      success_count == 0 ->
        IO.puts "\nFAILED: No progress made in pass #{pass}. Remaining modules: #{inspect(new_remaining)}"
        # Optionally show errors for one remaining module to diagnose
        if length(new_remaining) > 0 do
           file = hd(new_remaining)
           IO.puts "Diagnosing first failure: #{file}"
           Phi.Compiler.compile_module(File.read!(file), source_path: file, env: new_env)
           |> IO.inspect()
        end
      true ->
        do_compile(new_remaining, new_env, pass + 1)
    end
  end
end

CompileAll.run()
