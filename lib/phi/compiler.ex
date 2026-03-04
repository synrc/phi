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
         {:ok, ast} <- normalize_parse(Phi.Parser.parse(resolved)),
         desugared_ast <- Phi.Desugar.desugar(ast) do
      # Compile companion Erlang file if it exists
      foreign_mod =
        if source_path do
          :code.add_patha(~c"ebin")
          erl_path = String.replace(source_path, ".hm", ".erl")

          if File.exists?(erl_path) do
            erl_content = File.read!(erl_path)
            # Find `-module('M').` using regex
            mod_name_match = Regex.run(~r/-module\('([^']+)'\)\./, erl_content)

            mod_atom =
              if mod_name_match, do: Enum.at(mod_name_match, 1) |> String.to_atom(), else: nil

            # We can compile from forms if we parse it, but wait, `erl_scan` and `erl_parse` is easier:
            # wait, `:erl_scan.string` and `:erl_parse.parse_form` is tedious for a whole file.
            # An easier way: copy the file to a temp directory with the valid module name, compile, and move beam.
            if mod_atom do
              dir = Path.dirname(erl_path)
              temp_erl = Path.join(dir, "#{mod_atom}.erl")
              File.write!(temp_erl, erl_content)

              case :compile.file(String.to_charlist(temp_erl), [
                     :return_errors,
                     :debug_info,
                     {:outdir, ~c"ebin"}
                   ]) do
                {:ok, erlmod} ->
                  File.rm(temp_erl)
                  :code.purge(erlmod)
                  :code.load_file(erlmod)
                  erlmod

                {:error, err, _warn} ->
                  File.rm(temp_erl)
                  IO.puts("Warning: foreign compile failed for #{erl_path}: #{inspect(err)}")
                  nil
              end
            else
              IO.puts("Warning: Could not extract module name from #{erl_path}")
              nil
            end
          end
        end

      new_env = Phi.Typechecker.build_env(desugared_ast, env)

      codegen_opts = if foreign_mod, do: [foreign_mod: foreign_mod], else: []

      case Phi.Codegen.generate(desugared_ast, new_env, codegen_opts) do
        {:ok, forms} ->
          case :compile.forms(forms, [:return_errors, :debug_info]) do
            {:ok, mod, bin} ->
              {:ok, mod, bin, desugared_ast, new_env}

            {:error, errors, warnings} ->
              {:error, {:erl_compile, errors, warnings}}
          end

        {:error, _} = err ->
          err
      end
    end
  end

  # Accept both {:ok, ast} and {:ok, ast, rest} from the parser.
  # A non-empty rest means the parser couldn't consume all tokens (likely
  # unsupported syntax), but we still try to compile what was parsed.
  defp normalize_parse({:ok, ast}), do: {:ok, ast}

  defp normalize_parse({:ok, _ast, rest}) do
    {:error, {:partial_parse, rest}}
  end

  defp normalize_parse(err), do: err

  def load_module(mod, bin) do
    :code.load_binary(mod, ~c"#{mod}.beam", bin)
  end
end
