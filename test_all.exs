defmodule TestAll do
  def run do
    files = Path.wildcard("lib/**/*.hm")
    IO.puts "Found #{length(files)} files."

    Enum.each(files, fn file ->
      source = File.read!(file)
      case Phi.Lexer.lex(source) do
        {:ok, tokens} ->
          resolved = Phi.Layout.resolve(tokens)
          case Phi.Parser.parse(resolved) do
            {:ok, _ast} ->
              IO.puts "Parsed: #{file}"
            {:ok, _ast, rest} ->
               case rest do
                 [tok | _] -> IO.puts "Parsed with LEFTOVER tokens for #{file}: starting at line #{elem(tok, 1)}, col #{elem(tok, 2)} (Token: #{inspect(tok)})"
                 [] -> IO.puts "Parsed: #{file}"
               end
            {:error, reason} ->
              IO.puts "Parse ERROR for #{file}: #{inspect(reason)}"
            {:error, reason, rest} ->
               case rest do
                 [tok | _] ->
                   IO.puts "Parse ERROR for #{file}: #{inspect(reason)} at line #{elem(tok, 1)}, col #{elem(tok, 2)} (Token: #{inspect(tok)})"
                 [] ->
                   IO.puts "Parse ERROR for #{file}: #{inspect(reason)} at EOF"
               end
          end
        {:error, err} ->
          IO.puts "Lex ERROR for #{file}: #{inspect(err)}"
      end
    end)
  end
end

TestAll.run()
