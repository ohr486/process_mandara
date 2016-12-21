// Brunch automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "brunch-config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".
//import "phoenix_html"

import "deps/phoenix_html/web/static/js/phoenix_html";
import {Socket} from "deps/phoenix/web/static/js/phoenix";

// Import local files
//
// Local files can be imported directly using relative
// paths "./socket" or full ones "web/static/js/socket".

import NodeView from "./node_view";
import NodeSelector from "./node_selector";

class App {
  constructor(node_selector_container) {
    this.node_selector = new NodeSelector(node_selector_container, this.channel);
    $('#stop_msg_tracing').click(e => {
      if (this.node_view)
        this.node_view.stopMsgTraceAll();
    });
  }

  switchToNode(node) {
    if(this.node_view) {
      this.node_view.cleanup();
      this.node_selector.cleanup(this.node_view.node);
    }
    this.node_view = new NodeView(node, $('#graph'), $('#msg_seq'), $('#log'));
  }
}


$( () => {
  window.socket = new Socket("/socket");
  socket.connect();
  window.app = new App($('#node_selector'));
})


//import socket from "./socket"
