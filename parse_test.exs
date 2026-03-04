defmodule Phi.PT do
  def run do
    {:ok, tokens} = Phi.Lexer.lex("module Test where\ninstance (Arbitrary a, Testable b) => Testable (a -> b) where property f = f\n")
    res = Phi.Parser.parse(Phi.Layout.resolve(tokens)) 
    ast = elem(res, 1)
    IO.puts(inspect(ast.declarations |> List.first(), pretty: true))
  end
end
Phi.PT.run()
