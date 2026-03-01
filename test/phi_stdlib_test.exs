defmodule PhiStdlibTest do
  use ExUnit.Case

  @moduledoc """
  Integration tests that prove the Phi compiler can successfully lex, parse,
  typecheck, and compile the actual Hamler standard library modules natively to BEAM.
  """

  test "Compiles Data.Tuple" do
    source = """
    module Data.Tuple where

    data Tuple a b = Tuple a b

    fst :: forall a b. Tuple a b -> a
    fst (Tuple a b) = a

    snd :: forall a b. Tuple a b -> b
    snd (Tuple a b) = b
    """

    {:ok, tokens} = Phi.Lexer.lex(source)
    resolved = Phi.Layout.resolve(tokens)

    {:ok, ast} = Phi.Parser.parse(resolved)
    desugared_ast = Phi.Desugar.desugar(ast)

    # Let's verify the desugar step worked
    snd_decl = Enum.find(desugared_ast.declarations, fn decl ->
      decl.__struct__ == Phi.AST.DeclValue and decl.name == "snd"
    end)
    assert snd_decl.binders == []
    assert %Phi.AST.ExprLam{binder: %Phi.AST.BinderConstructor{name: "Tuple"}} = snd_decl.expr

    # Check typechecker builds Env properly
    env = Phi.Typechecker.build_env(desugared_ast, Phi.Typechecker.Env.new())

    assert {:ok, _} = Phi.Typechecker.infer(env, snd_decl.expr)
    {:ok, forms} = Phi.Codegen.generate(desugared_ast, env)

    # Compile to BEAM binary and load it into VM!
    assert {:ok, mod, bin} = :compile.forms(forms, [:return_errors])
    assert mod == :data_tuple
    assert :code.load_binary(mod, ~c"#{mod}", bin) == {:module, mod}

    # Try calling the compiled fst and snd!
    tuple_val = {:Tuple, 42, 100}
    assert mod.fst(tuple_val) == 42
    assert mod.snd(tuple_val) == 100
  end
end
