import Phi.Compiler

# We will run the Mix Task code up to compile tests!
Code.compiler_options(ignore_module_conflict: true)

IO.puts("Loading stdlib...")
env = Enum.reduce(Path.wildcard("stdlib/**/*.phi"), Phi.Typechecker.Env.new(), fn f, e ->
  case compile_module(File.read!(f), source_path: f, env: e) do
    {:ok, _, _, _, e2} -> e2
    _ -> e
  end
end)
{:ok, _, _, _, env} = compile_module(File.read!("tests/Test.phi"), source_path: "tests/Test.phi", env: env)

with {:ok, tokens} <- Phi.Lexer.lex(File.read!("tests/Test/Control/Process.phi")),
     resolved <- Phi.Layout.resolve(tokens),
     {:ok, ast} <- Phi.Parser.parse(resolved),
     desugared_ast <- Phi.Desugar.desugar(ast) do
  
  IO.puts("Parsed and Desugared...")
  new_env = Phi.Typechecker.build_env(desugared_ast, env)
  IO.puts("Typechecker Built Env...")
  Phi.Typechecker.infer(new_env, desugared_ast)
  IO.puts("Infer executed...")
  Phi.Codegen.generate(desugared_ast, new_env, [])
  IO.puts("Codegen finished!")
end
