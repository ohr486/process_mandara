defmodule ProcessMandara.TrackChannel do
  use ProcessMandara.Web, :channel
  alias ProcessMandara.Tracker
  alias ProcessMandara.Endpoint
  alias Phoenix.Socket

  def join("track:" <> node, _auth_msg, socket) do
    node = String.to_atom(node)
    Tracker.send_module(node)
    Tracker.start(node)

    {:ok, Tracker.init_state(node), socket}
  end

  def handle_in("msg_track", pid, %Socket{topic: "track:" <> node} = socket) do
    node |> String.to_atom |> Tracker.msg_track(pid)

    {:noreply, socket}
  end

  def handle_in("stop_msg_track_all", _msg, %Socket{topic: "track:" <> node} = socket) do
    node |> String.to_atom |> Tracker.stop_msg_track_all

    {:noreply, socket}
  end

  def handle_in("cleanup", node, socket), do: {:noreply, socket}

  def announce_spawn(node, pid_map) do
    IO.puts "###### announce_spawn! #######"
    Endpoint.broadcast! "track:#{node}", "spawn", pid_map
  end

  def announce_exit(node, pid) do
    Endpoint.broadcast! "track:#{node}", "exit", %{pid: pid}
  end

  def announce_name(node, pid, name) do
    Endpoint.broadcast! "track:#{node}", "name", %{pid: pid, name: name}
  end

  # a list of links is a list of lists
  # [[pid1, pid2], [pid3, pid4], ...]
  def announce_links(node, links) do
    Endpoint.broadcast! "track:#{node}", "links", %{links: links}
  end

  def announce_link(node, link), do: announce_links(node, [link])

  def announce_unlink(node, link) do
    Endpoint.broadcast! "track:#{node}", "unlink", %{link: link}
  end

  def announce_msg(node, from_pid, to_pid, msg) do
    Endpoint.broadcast! "track:#{node}", "msg", %{from_pid: from_pid,
                                                  to_pid: to_pid,
                                                  msg: inspect(msg)}
  end
end
