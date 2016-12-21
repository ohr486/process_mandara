export default class {
  constructor(container) {
    console.log("ns-con");

    this.container = container;
    this.channel = socket.channel("nodes", {});

    let updateNodes = msg => {
      this.update(msg.nodes);
    };

    this.channel.join().receive("ok", updateNodes);
    this.channel.on("update", updateNodes);
  }

  update(nodes) {
    console.log("ns-update:" + nodes);
    let node_els = d3.select(this.container.find("ul").get(0)).selectAll("li.node_name").data(nodes);

    let node = node_els.enter().insert("li")
          .attr("class", "node_name")
          .html(n => n)
          .on("click", function(d) {
            $(this).addClass("selected");
            $(this).siblings().removeClass("selected");
            app.switchToNode(d);
          });
    node_els.exit().remove();
  }

  add(node) {
    console.log("ns-add:" + node);
    this.channel.push("add", node);
  }

  cleanup(node) {
    console.log("ns-cleanup:" + node);
    this.channel.push("cleanup", node);
  }
}
