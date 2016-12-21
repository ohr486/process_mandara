import Graph from "web/static/js/graph";

export default class {
  constructor(node, graph_container, msg_seq_container, log_container) {
    this.node = node;
    this.channel = socket.channel("track:"+ node, {});
    this.pids = {};

    //graph_container.empty();

    this.graph = new Graph(graph_container, this, this.pids);


    this.channel.on("spawn", msg => {
      console.log("spawn!!!!!!!!!!");
      $.each(msg, (pid, info) => {
        this.addNode(pid, info);
      });
      this.graph.update(true);
    });

    this.channel.join().receive("ok", msg => {
      $.each(msg.pids, (pid, info) => this.addNode(pid, info));
      $.each(msg.ports, (port, info) => this.addNode(port, info));
      msg.links.forEach(link => this.graph.addLink(link[0], link[1]));
      this.graph.update(true);
    })
    .receive("error", resp => { console.log("channel join error", resp) })
  }

  addNode(id, info) {
    let pid = {"id": id,
      links: {},
      name: info.name,
      app: info.app,
      type: info.type,
      msg_traced: info.msg_traced};
    this.pids[id] = pid;
  }

  cleanup() {
    this.channel.leave();
  }
}
