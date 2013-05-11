defmodule ElixirHelloWorld.Mixfile do
  use Mix.Project

  def project do
    [ app: :elixir_hello_world,
      version: "0.0.1",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ mod: { ElixirHelloWorld, [] },
      applications: [:cowboy] ]
  end

  defp deps do
    [ {:ranch,  github: "extend/ranch", tag: "0.8.1"},
      {:cowboy, github: "extend/cowboy"} ]
  end
end
