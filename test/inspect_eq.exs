source = File.read!("lib/Data/Eq.hm")
{:ok, tokens} = Phi.Lexer.lex(source)
resolved = Phi.Layout.resolve(tokens)
IO.inspect(resolved, limit: :infinity)
