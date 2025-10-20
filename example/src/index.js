"use strict";

import "./style.css";
import * as Viz from "@viz-js/viz";

const SVG_NS = "http://www.w3.org/2000/svg";

const getNode = (x) => x.querySelector("title").childNodes[0].data;

const getSymbol = (x) => x.querySelector("text").childNodes[0].data;

const eqEdge = (x, y) =>
  x && y && x.from == y.from && x.to == y.to && x.symbol == y.symbol;

var Elm = require("./Main.elm").Elm;

var app = Elm.Main.init();

const getEdge = (x) => {
  const [from, to] = x.querySelector("title").childNodes[0].data.split("->");
  return { from: from, to: to, symbol: getSymbol(x) };
};

app.ports.renderDot_.subscribe(function ([graphId, message]) {
  Viz.instance().then((viz) => {
    const svg = viz.renderSVGElement(message);
    const graph = document.getElementById(graphId);
    if (!graph) {
      console.error("Could not find element with id:", graphId);
      return;
    }
    graph.childNodes.forEach((n) => n.remove());
    graph.appendChild(svg);

    svg.querySelectorAll("g.node").forEach((x) => {
      const t = x.querySelector("text");
      const e = x.querySelector("ellipse");
      e.classList.add("outside");
      if (t && x.classList.contains("start")) {
        const el = document.createElementNS(SVG_NS, "text");
        el.setAttribute("x", t.getAttribute("x") * 1 - e.getAttribute("rx"));
        el.setAttribute("y", t.getAttribute("y") * 1 - 16);
        el.setAttribute("font-family", "serif");
        el.setAttribute("font-size", "12px");
        el.innerHTML = "🡾";
        t.parentNode.insertBefore(el, t);
      }
      if (t && x.classList.contains("end")) {
        const el = document.createElementNS(SVG_NS, "ellipse");
        el.classList.add("inside");
        el.setAttribute("cx", e.getAttribute("cx") * 1);
        el.setAttribute("cy", e.getAttribute("cy") * 1);
        el.setAttribute("rx", e.getAttribute("rx") - 2);
        el.setAttribute("ry", e.getAttribute("ry") - 2);
        el.setAttribute("stroke", "black");
        el.setAttribute("stroke-width", "0.6");
        t.parentNode.insertBefore(el, t);
      }

      x.addEventListener("click", () => {
        app.ports.selectState_.send([graphId, getNode(x)]);
      });
      x.addEventListener("mouseenter", () =>
        app.ports.selectState_.send([graphId, getNode(x)])
      );
      x.addEventListener("mouseleave", () =>
        app.ports.selectState_.send([graphId, null])
      );
    });

    svg.querySelectorAll("g.edge").forEach((x) => {
      const t = x.querySelector("text");
      const el = document.createElementNS(SVG_NS, "rect");
      el.setAttribute("x", t.getAttribute("x") * 1 - 6);
      el.setAttribute("y", t.getAttribute("y") * 1 - 14);
      el.setAttribute("width", 12);
      el.setAttribute("height", 18);
      el.setAttribute("rx", "3");
      el.setAttribute("fill", "#ddd8");
      t.parentNode.insertBefore(el, t);

      const p = x.querySelector("path");
      p.classList.add("visual");
      const elP = document.createElementNS(SVG_NS, "path");
      elP.setAttribute("d", p.getAttribute("d"));
      elP.setAttribute("fill", "none");
      elP.setAttribute("stroke", "transparent");
      elP.setAttribute("stroke-width", "10");
      elP.classList.add("event");
      t.parentNode.insertBefore(elP, t);

      x.addEventListener("click", () => {
        app.ports.selectTransition_.send([graphId, getEdge(x)]);
      });
      x.addEventListener("mouseenter", () => {
        app.ports.selectTransition_.send([graphId, getEdge(x)]);
      });
      x.addEventListener("mouseleave", () => {
        app.ports.selectTransition_.send([graphId, null]);
      });
    });

    svg.addEventListener("mouseleave", () => {
      app.ports.selectState_.send([graphId, null]);
      app.ports.selectTransition_.send([graphId, null]);
      app.ports.selectSymbol_.send([graphId, null]);
    });
  });
});

app.ports.selectedState_.subscribe(([graphId, nodeId]) => {
  const graph = document.getElementById(graphId);
  graph.querySelectorAll("g.node").forEach((x) => {
    x.classList.remove("selected");
    if (nodeId != null && getNode(x) == nodeId) {
      x.classList.add("selected");
    }
  });
});

app.ports.selectedTransition_.subscribe(([graphId, edge]) => {
  const graph = document.getElementById(graphId);

  graph.querySelectorAll("g.node").forEach((x) => {
    x.classList.remove("from");
    x.classList.remove("to");
    if (edge && getNode(x) == edge.from) {
      x.classList.add("from");
    }
    if (edge && getNode(x) == edge.to) {
      x.classList.add("to");
    }
  });

  graph.querySelectorAll("g.edge").forEach((x) => {
    x.classList.remove("selected");
    const e = getEdge(x);
    if (
      edge &&
      e.from == edge.from &&
      e.to == edge.to &&
      e.symbol == edge.symbol
    ) {
      x.classList.add("selected");
    }
  });
});

app.ports.selectedSymbol_.subscribe(([graphId, symbol]) => {
  const graph = document.getElementById(graphId);
  graph.querySelectorAll("g.edge").forEach((x) => {
    x.classList.remove("selected-symbol");
    const e = getEdge(x);
    if (symbol && e.label == symbol) {
      x.classList.add("selected-symbol");
    }
  });
});

app.ports.selectedStateStep_.subscribe(([graphId, selection]) => {
  const graph = document.getElementById(graphId);
  var prev, state, next;
  if (selection) {
    [prev, state, next] = selection;
  }
  graph.querySelectorAll("g.node").forEach((x) => {
    x.classList.remove("selected");
    if (selection && getNode(x) == state) {
      x.classList.add("selected");
    }
  });
  graph.querySelectorAll("g.edge").forEach((x) => {
    x.classList.remove("from");
    x.classList.remove("to");
    if (selection && prev && eqEdge(getEdge(x), prev)) {
      x.classList.add("from");
    }
    if (selection && next && eqEdge(getEdge(x), next)) {
      x.classList.add("to");
    }
  });
});
