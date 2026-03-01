defmodule Phi do
  @moduledoc """
  Phi is an Elixir-native compiler port of Hamler and PureScript,
  targeting the Erlang VM (BEAM).
  """

  def compile_file(path, out_dir \\ ".") do
    with {:ok, bin} <- File.read(path),
         {:ok, tokens} <- Phi.Lexer.lex(bin),
         resolved = Phi.Layout.resolve(tokens),
         {:ok, ast} <- Phi.Parser.parse(resolved),
         desugared_ast = Phi.Desugar.desugar(ast),
         {:ok, forms} <- Phi.Codegen.generate(desugared_ast),
         {:ok, mod_name, binary} <- :compile.forms(forms, [:return_errors]) do

      out_path = Path.join(out_dir, "#{mod_name}.beam")
      File.write!(out_path, binary)
      {:ok, out_path}
    else
      err ->
        {:error, {:compilation_failed, err}}
    end
  end
end
