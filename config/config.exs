# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# Configures the endpoint
config :process_mandara, ProcessMandara.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "8tTYPOnfyfDZveLTFWb1bFnRW211SBemXqnL2fd5TK6TfJm0Oe9ZoCApYfZ7rnjU",
  render_errors: [view: ProcessMandara.ErrorView, accepts: ~w(html json)],
  pubsub: [name: ProcessMandara.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
