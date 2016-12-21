defmodule ProcessMandara.PageView do
  use ProcessMandara.Web, :view

  def hostname do
    node |> Atom.to_string |> String.split("@") |> List.last
  end
end
