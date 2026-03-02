defmodule Phi.MixProject do
  use Mix.Project
  def project do
      [
        app: :phi,
        version: "0.3.1",
        description: "The Phi Programming Language",
        deps: deps(),
        package: package()
      ]
  end
  def application do [ extra_applications: [ :logger ] ] end
  def deps do
      [
#        {:ex_doc, ">= 0.0.0", only: :dev}
      ]
  end
  def package() do
      [
        files: ["lib", "repl", "LICENSE", "README.md" ],
        licenses: ["ISC"],
        maintainers: ["Namdak Tonpa"],
        name: :phi,
        links: %{"GitHub" => "https://github.com/synrc/phi"}
      ]
  end
end
