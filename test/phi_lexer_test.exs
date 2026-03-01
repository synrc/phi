defmodule PhiLexerParserTest do
  use ExUnit.Case

  test "Lexer tokenizes a simple let binding" do
    source = """
    let x = 1
    in x
    """

    assert {:ok,
            [
              {:let, 1, 1},
              {:var_ident, 1, 5, "x"},
              {:=, 1, 7},
              {:number, 1, 9, "1"},
              {:in, 2, 1},
              {:var_ident, 2, 4, "x"}
            ]} == Phi.Lexer.lex(source)
  end

  test "Layout resolves implicit blocks" do
    source = """
    module Main where
    foo = 1
    bar = 2
    """

    {:ok, tokens} = Phi.Lexer.lex(source)
    resolved = Phi.Layout.resolve(tokens)

    assert [
             {:module, 1, 1},
             {:proper_name, 1, 8, "Main"},
             {:where, 1, 13},
             {:left_brace, 2, 1},
             {:var_ident, 2, 1, "foo"},
             {:=, 2, 5},
             {:number, 2, 7, "1"},
             {:semicolon, 3, 1},
             {:var_ident, 3, 1, "bar"},
             {:=, 3, 5},
             {:number, 3, 7, "2"},
             {:right_brace, 3, 7}
           ] == resolved
  end
end
