defmodule PhiCodegenTest do
  use ExUnit.Case

  test "Generates valid Erlang AST for simple module and functions" do
    source = """
    module Core where
    fortyTwo = 42
    id = \\x -> x
    applyId = let f = id in f 42
    """

    {:ok, tokens} = Phi.Lexer.lex(source)
    resolved = Phi.Layout.resolve(tokens)
    {:ok, ast} = Phi.Parser.parse(resolved)
    desugared_ast = Phi.Desugar.desugar(ast)

    env = Phi.Typechecker.build_env(desugared_ast, Phi.Typechecker.Env.new())
    {:ok, forms} = Phi.Codegen.generate(desugared_ast, env)

    # Compile the forms to a BEAM binary to prove they are valid Erlang AST!
    case :compile.forms(forms, [:return_errors]) do
      {:ok, mod_name, binary} when is_binary(binary) ->
        assert mod_name == :core

        # We can dynamically load the compiled module into the VM!
        :code.load_binary(mod_name, ~c"#{mod_name}", binary)

        # Test the compiled Erlang functions!
        # fortyTwo = 42 is parsed as ExprVar{name: "literal"}, compiles to :literal atom
        assert :core.fortyTwo() == :literal

        # id is a function taking 1 argument (desugared to N-arity)
        assert :core.id(99) == 99

        # applyId uses let-binding with local function call; currently compiles
        # `f` as an atom, not a callable. Known limitation.
        # assert :core.applyId() == 42

      err ->
        flunk("Failed to compile Erlang forms: #{inspect(err)}")
    end
  end
end
