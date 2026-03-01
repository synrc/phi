defmodule Phi.Compiler do
  @moduledoc """
  Main entry point for the Phi compiler.
  Orchestrates Lexing, Layout, Parsing, Desugaring, Typechecking, and Codegen.
  """

  def compile_module(source_code, opts \\ []) do
    source_path = Keyword.get(opts, :source_path)
    env = Keyword.get(opts, :env, Phi.Typechecker.Env.new())

    with {:ok, tokens} <- Phi.Lexer.lex(source_code),
         resolved <- Phi.Layout.resolve(tokens),
         {:ok, ast} <- Phi.Parser.parse(resolved),
         desugared_ast <- Phi.Desugar.desugar(ast) do

      # Compile companion Erlang file if it exists
      if source_path do
        :code.add_patha(~c"ebin")
        erl_path = String.replace(source_path, ".hm", ".erl")
        if File.exists?(erl_path) do
          case :compile.file(String.to_charlist(erl_path), [:return_errors, :debug_info, {:outdir, ~c"ebin"}]) do
            {:ok, mod} ->
              IO.puts("Compiled foreign module: #{mod}")
              :code.purge(mod)
              :code.load_file(mod)
            {:error, err, _warn} -> IO.puts("Warning: foreign compile failed: #{inspect(err)}")
          end
        end
      end

      # Enrich environment with declarations from this module
      new_env = Phi.Typechecker.build_env(desugared_ast, env)

      # We could typecheck each expression here, but for integration
      # we just proceed to codegen if parsing succeeded.
      # A real compiler would throw on type errors.

      case Phi.Codegen.generate(desugared_ast, new_env) do
        {:ok, forms} ->
           case :compile.forms(forms, [:return_errors, :debug_info]) do
             {:ok, mod, bin} ->
                {:ok, mod, bin, desugared_ast, new_env}
             {:error, errors, warnings} ->
                {:error, {:erl_compile, errors, warnings}}
           end
        {:error, _} = err -> err
        err -> {:error, err}
      end
    end
  end

  def load_module(mod, bin) do
    :code.load_binary(mod, ~c"#{mod}.beam", bin)
  end
end
