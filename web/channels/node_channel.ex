defmodule ProcessMandara.NodesChannel do
  use ProcessMandara.Web, :channel
  require Logger
  alias ProcessMandara.Endpoint
  alias ProcessMandara.Tracker

  @self Node.self |> Atom.to_string

  def join("nodes", _auth_msg, socket) do
    {:ok, nodes_msg, socket}
  end

  def handle_in("add", node, socket) do
    ping_result = node |> String.to_atom |> Node.ping
    Logger.debug("Pinging node #{node} returned #{inspect ping_result}")

    Endpoint.broadcast! "nodes", "update", nodes_msg

    {:noreply, socket}
  end

  def handle_in("cleanup", node, socket) when node != @self do
    node |> String.to_atom |> Tracker.cleanup
    Logger.debug("Telling node #{node} to clean up")

    {:noreply, socket}
  end
  def handle_in("cleanup", node, socket), do: {:noreply, socket}


  defp nodes_msg do
    %{nodes: Node.list(:known)}
  end
end
