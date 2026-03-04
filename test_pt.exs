defmodule Phi.PT do
  def run do
    env =
      Phi.Typechecker.build_env(
        elem(
          Phi.Parser.parse(
            Phi.Layout.resolve(elem(Phi.Lexer.lex(File.read!("lib/Data/Functor.hm")), 1))
          ),
          1
        ),
        Phi.Typechecker.Env.new()
      )

    {:ok, {mod, scheme}} = Phi.Typechecker.Env.lookup(env, "map")

    {dicts, expected} = split_arity(scheme)
    IO.puts("Dicts: #{dicts}, Expected: #{expected}")
  end

  def split_arity(%Phi.Type.Forall{type: t}), do: do_split_arity(t, 0, 0)
  def split_arity(t), do: do_split_arity(t, 0, 0)
  def do_split_arity(%Phi.Type.Forall{type: t}, c, a), do: do_split_arity(t, c, a)
  def do_split_arity(%Phi.Type.TConstrained{type: t}, c, a), do: do_split_arity(t, c + 1, a)

  def do_split_arity(
        %Phi.Type.TApp{func: %Phi.Type.TApp{func: %Phi.Type.TCon{name: "->"}}, arg: res},
        c,
        a
      ), do: do_split_arity(res, c, a + 1)

  def do_split_arity(_, c, a), do: {c, a}
end

try do
  Phi.PT.run()
rescue
  e -> IO.inspect(e)
end
