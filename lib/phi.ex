defmodule Phi do
  @moduledoc """
  Phi is an Elixir-native compiler port of Hamler and PureScript,
  targeting the Erlang VM (BEAM).
  """

  def compile_file(path, out_dir \\ ".") do
    with {:ok, source} <- File.read(path),
         {:ok, tokens} <- Phi.Lexer.lex(source),
         resolved = Phi.Layout.resolve(tokens),
         {:ok, ast} <- Phi.Parser.parse(resolved),
         desugared_ast = Phi.Desugar.desugar(ast),
         env = Phi.Typechecker.build_env(desugared_ast, Phi.Typechecker.Env.new()),
         {:ok, forms} <- Phi.Codegen.generate(desugared_ast, env),
         {:ok, mod_name, binary} <- :compile.forms(forms, [:verbose, :report_errors]) do

      out_path = Path.join(out_dir, "#{mod_name}.beam")
      File.write!(out_path, binary)
      {:ok, out_path}
    else
      err ->
        {:error, {:compilation_failed, err}}
    end
  end
end
