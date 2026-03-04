defmodule TR do
  def run do
    # load Curry FFI
    :code.add_path("ebin")
    IO.inspect(:"Test.QuickCheck".runTestGroup(0, {:Exe, [fn n -> IO.puts("hello #{n}"); {:Successed} end]}))
  end
end
TR.run()
