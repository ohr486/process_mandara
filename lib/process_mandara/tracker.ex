defmodule Tracker do
  use GenServer

  def start(node) do
    Node.spawn_link(node, :gen_server, :start, [{:local, __MODULE__}, __MODULE__, [Node.self], []])
  end

  def init_state do
    %{
    }
  end

  def pid_to_info(pid) do

  end

  def pid_name(pid) when is_pid(pid) do
    case Process.info(pid, :registered_name) do
      {:registered_name, name} -> name |> :erlang.atom_to_binary(:utf8)
      _ -> nil
    end
  end
  def pid_name(_), do: raise "invalid type"

  def app(pid) when is_pid(pid) do
    case :application.get_application(pid) do
      :undefined -> nil
      {_pid, app} -> app
    end
  end
  def app(_), do: raise "invalid type"

  def pid_to_bin(pid) when is_pid(pid) do
    "#PID#{ pid |> :erlang.pid_to_list |> :erlang.list_to_binary }"
  end
  def pid_to_bin(_), do: raise "invalid type"

  def pid_info(pid) when is_pid(pid) do
    %{
      name: pid_name(pid),
      app: app(pid)
    }
  end

end
