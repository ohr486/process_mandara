defmodule ProcessMandara.Tracker do
  use GenServer
  alias ProcessMandara.TrackChannel

  def send_module(node) do
    {module, binary, file} = :code.get_object_code(__MODULE__)
    :rpc.call(node, :code, :load_binary, [module, file, binary])
  end

  def start(node) do
    Node.spawn_link(node, :gen_server, :start, [{:local, __MODULE__}, __MODULE__, [Node.self], []])
  end

  def init_state(node) do
    :rpc.call(node, __MODULE__, :init_state, [])
  end

  def init_state do
    %{
      pids: map_pids_to_info(Process.list),
      ports: map_pids_to_info(:erlang.ports),
      links: all_links
    }
  end

  # --- Tracking ---

  def init([mandara_node]) do
    :erlang.trace(:all, true, [:procs])
    {:ok, mandara_node}
  end

  def handle_info({:trace, _spawner_pid, :spawn, pid, _mfa}, mandara_node) do
    :rpc.call(mandara_node, TrackChannel, :announce_spawn, [:erlang.node, map_pids_to_info([pid])])
    {:noreply, mandara_node}
  end

  def handle_info({:trace, pid, :exit, reason}, mandara_node) do
    :rpc.call(mandara_node, TrackChannel, :announce_exit, [:erlang.node, pid_to_bin(pid)])
    {:noreply, mandara_node}
  end

  def handle_info(msg, state) do
    {:noreply, state}
  end

  # --- Process Data ---

  def map_pids_to_info(pids) do
    pids
    |> Enum.map(&(pinfo(&1)))
    |> Enum.filter(fn {pid, %{type: type}} -> type != :dead end)
    |> Map.new
  end

  def pinfo(pid) when is_pid(pid) do
    {
      pid_to_bin(pid),
      %{
        name: pname(pid),
        type: ptype(pid),
        app: app(pid)
      }
    }
  end
  def pinfo(port) when is_port(port) do
    {
      pid_to_bin(port),
      %{
        name: pname(port),
        type: ptype(port),
        app: app(port)
      }
    }
  end
  def pinfo(_), do: raise "invalid type"

  def name_to_bin(name) when is_atom(name) do
    :erlang.atom_to_binary(name, :utf8)
  end
  def name_to_bin(name) when is_list(name) do
    :erlang.list_to_binary(name)
  end

  def pname(pid) when is_pid(pid) do
    case Process.info(pid, :registered_name) do
      {:registered_name, name} -> name_to_bin(name)
      _ -> nil
    end
  end
  def pname(port) when is_port(port) do
    case :erlang.port_info(port, :name) do
      {:name, name} -> name |> :erlang.list_to_binary
      _             -> nil
    end
  end
  def pname(_), do: raise "invalid type"

  def app(pid) when is_pid(pid) do
    case :application.get_application(pid) do
      :undefined -> nil
      {_pid, app} -> app
    end
  end
  def app(port) when is_port(port), do: nil
  def app(_), do: raise "invalid type"

  def ptype(pid) when is_pid(pid) do
    case Process.info(pid, :dictionary) do
      :undefined -> :dead
      #{_, [{_, _}, "$initial_call": {:supervisor, _, _}]} -> :supervisor
      {_, ["$initial_call": {:supervisor, _, _}, "$ancestors": _]} -> :supervisor
      _ -> :normal
    end
  end
  def ptype(port) when is_port(port), do: :port

  def pid_to_bin(pid) when is_pid(pid) do
    "#PID" <> (pid |> :erlang.pid_to_list |> :erlang.list_to_binary)
  end
  def pid_to_bin(port) when is_port(port) do
    port |> :erlang.port_to_list |> :erlang.list_to_binary
  end
  def pid_to_bin(atom) when is_atom(atom) do
    atom |> :erlang.whereis |> pid_to_bin
  end

  # --- Link Data ---

  def all_links do
    :lists.flatmap(fn pid ->
      links = case :erlang.process_info(pid, :links) do
        {:links, links} -> links
        :undefined      -> []
      end
      :lists.map( &:lists.sort([pid_to_bin(pid), pid_to_bin(&1)]), links )
    end, :erlang.processes)
    |> :lists.usort
  end
end
