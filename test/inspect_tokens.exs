defmodule InspectTokens do
  def run do
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
    IO.inspect(resolved, label: "RESOLVED")

    ast = Phi.Parser.parse(resolved)
    IO.inspect(ast, label: "AST")
  end
end
InspectTokens.run()
