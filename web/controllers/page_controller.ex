defmodule ProcessMandara.PageController do
  use ProcessMandara.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
