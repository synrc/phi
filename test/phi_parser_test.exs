defmodule PhiParserTest do
  use ExUnit.Case

  test "Parser constructs an AST from layouted tokens" do
    source = """
    module Main where
    foo = 1
    bar = 2
    """

    {:ok, tokens} = Phi.Lexer.lex(source)
    resolved_tokens = Phi.Layout.resolve(tokens)

    # Run Parser
    {:ok, ast} = Phi.Parser.parse(resolved_tokens)

    assert ast.name == "Main"
    assert length(ast.declarations) == 2

    # Verify order and content
    [foo_decl, bar_decl] = ast.declarations

    assert foo_decl.name == "foo"
    assert foo_decl.expr == %Phi.AST.ExprVar{name: "num_1"}

    assert bar_decl.name == "bar"
    assert bar_decl.expr == %Phi.AST.ExprVar{name: "num_2"}
  end

  test "Parser constructs AST for Data Declarations" do
    source = """
    module Test where
    data Maybe a = Nothing | Just a
    data Result = Ok | Err
    """

    {:ok, tokens} = Phi.Lexer.lex(source)
    resolved = Phi.Layout.resolve(tokens)
    {:ok, ast} = Phi.Parser.parse(resolved)

    assert length(ast.declarations) == 2
    [maybe_decl, result_decl] = ast.declarations

    assert maybe_decl.name == "Maybe"
    assert length(maybe_decl.args) == 1
    assert ["a"] = maybe_decl.args
    assert length(maybe_decl.constructors) == 2
    assert {"Nothing", []} = Enum.at(maybe_decl.constructors, 0)
    assert {"Just", [%Phi.AST.TypeVar{name: "a"}]} = Enum.at(maybe_decl.constructors, 1)

    assert result_decl.name == "Result"
    assert length(result_decl.args) == 0
    assert length(result_decl.constructors) == 2
  end

  test "Parser constructs AST for Classes and Instances" do
    source = """
    module Main where
    class Eq a where
      eq :: a -> a -> Bool
    instance Eq Int where
      eq = false
    """

    {:ok, tokens} = Phi.Lexer.lex(source)
    resolved = Phi.Layout.resolve(tokens)
    {:ok, ast} = Phi.Parser.parse(resolved)

    assert length(ast.declarations) >= 2
    class_decl = Enum.find(ast.declarations, &match?(%Phi.AST.DeclClass{}, &1))
    inst_decl = Enum.find(ast.declarations, &match?(%Phi.AST.DeclInstance{}, &1))

    assert %Phi.AST.DeclClass{name: "Eq"} = class_decl
    assert length(class_decl.args) == 1
    assert [%Phi.AST.TypeVar{name: "a"}] = class_decl.args
    assert length(class_decl.members) == 1

    [member_sig] = class_decl.members
    assert %Phi.AST.DeclTypeSignature{name: "eq"} = member_sig
    assert %Phi.AST.TypeArrow{} = member_sig.type

    assert %Phi.AST.DeclInstance{class: "Eq"} = inst_decl
    assert length(inst_decl.types) == 1
    assert [%Phi.AST.TypeConstructor{name: "Int"}] = inst_decl.types
    assert length(inst_decl.members) == 1
  end
end
