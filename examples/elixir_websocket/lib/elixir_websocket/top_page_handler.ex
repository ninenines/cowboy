# Feel free to use, reuse and abuse the code in this file.

defmodule ElixirWebsocket.TopPageHandler do
  def init(_transport, req, []) do
    {:ok, req, nil}
  end

  def get_html do
    {:ok, cwd} = :file.get_cwd()
    filename = :filename.join([cwd, "priv", "html_ws_client.html"])
    {:ok, binary} = :file.read_file(filename)
    binary
  end

  def handle(req, state) do
  html = get_html()
    {:ok, req} = :cowboy_req.reply(200,
    [{<<"content-type">>, <<"text/html">>}],
      html, req)
    {:ok, req, state}
  end

  def terminate(_reason, _req, _state) do
    :ok
  end
end