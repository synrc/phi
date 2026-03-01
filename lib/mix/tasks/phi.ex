defmodule Mix.Tasks.Phi do
  @shortdoc "Compiles a Phi source file to a BEAM binary"
  use Mix.Task

  @impl Mix.Task
  def run(args) do
    case args do
      [file] -> compile(file, ".")
      [file, out_dir] -> compile(file, out_dir)
      _ -> Mix.shell().error("Usage: mix phi <path/to/file.phi> [out_dir]")
    end
  end

  defp compile(file, out_dir) do
    Mix.shell().info("Compiling #{file}...")
    case Phi.compile_file(file, out_dir) do
      {:ok, beam_path} ->
        Mix.shell().info("Successfully compiled to #{beam_path}")
      {:error, reason} ->
        Mix.shell().error("Compilation failed: #{inspect(reason)}")
    end
  end
end
