defmodule ElixirWebsocket.Mixfile do
  use Mix.Project

  def project do
    [ app: :elixir_websocket,
      version: "0.0.1",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ mod: { ElixirWebsocket, [] },
      applications: [:crypto, :ranch, :cowboy] ]
  end

  defp deps do
    [ {:ranch,  github: "extend/ranch", tag: "0.6.2"},
      {:cowboy, github: "extend/cowboy"},
      {:mimetypes, github: "spawngrid/mimetypes"} ]
  end
end
