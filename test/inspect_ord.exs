source = File.read!("lib/Data/Ord.hm")
{:ok, tokens} = Phi.Lexer.lex(source)
resolved = Phi.Layout.resolve(tokens)
IO.inspect(resolved, limit: :infinity)
