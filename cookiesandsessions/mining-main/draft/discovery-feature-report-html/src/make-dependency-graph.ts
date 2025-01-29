import { DiscoveryFeatureMatrix } from "./model";
import * as d3 from 'd3';
import { graphviz } from '@hpcc-js/wasm';
import _ from "lodash";

interface Node {
  id: string;
  moduleType: string;
}

interface Edge {
  from: string;
  to: string;
  relationship: string;
}

export function makeDependecyGraph(p: d3.Selection<HTMLParagraphElement, DiscoveryFeatureMatrix[], HTMLElement, unknown>) {
  const svg = p.append('div').attr('class', 'dependencies')
    .each(async function (d) {
      const promise = graphviz.dot(makeDigraph(d), 'svg');
      asyncOperations.push(promise);
      const graph = await promise;
      this.innerHTML = graph;
    });
}

function makeDigraph(data: DiscoveryFeatureMatrix[]) {
  const nodes: Node[] = [];
  const edges: Edge[] = [];
  let counter = 0;

  function getOrCreateNode(moduleType: string) {
    let node = _.find(nodes, n => n.moduleType === moduleType);
    node = { id: `n${++counter}`, moduleType };
    nodes.push(node);
    return node;
  }

  data.forEach(d => {
    if (d.dependencies.length == 0) {
      return;
    }
    const from = getOrCreateNode(d.moduleType);
    d.dependencies.forEach(r => {
      const to = getOrCreateNode(r.moduleType);
      edges.push({ from: from.id, to: to.id, relationship: r.relationship[0] });
    })
  })

  const nodesString = nodes.map(n => `${n.id} [label="${n.moduleType}" shape=box fontsize="11" fontname="Arial"]`);
  const edgesString = edges.map(e => `${e.from} -> ${e.to} [label="${e.relationship}" fontsize="11" fontname="Arial"]`);

  return `digraph { splines=ortho ${nodesString.join(' ')} ${edgesString.join(' ')} }`
}