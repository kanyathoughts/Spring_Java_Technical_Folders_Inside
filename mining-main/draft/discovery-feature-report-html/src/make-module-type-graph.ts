import { DiscoveryFeatureMatrix } from "./model";
import _ from 'lodash';
import * as d3 from 'd3';

interface GraphElement {
  type: string;
  children: GraphElement[];
  shape: 'MISSING' | 'MODULE' | 'SOURCE'
}

export function makeModuleTypeGraph(p: d3.Selection<HTMLParagraphElement, DiscoveryFeatureMatrix[], HTMLElement, unknown>) {

  const svg = p.selectAll('svg').data(d => makeGraphStructure(d))
    .enter().append('svg')
      .attr('width', 300)
      .attr('height', d => 50 * graphDepth(d, 1));

  svg.append('g').attr('transform', 'translate(0, 0)').each(function (d) {
    makeGroup(this, d);
  });
}

function makeGroup(element: SVGGElement, data: GraphElement) {
  const g = d3.select(element);
  if (data.shape === 'SOURCE') {
    g.append('ellipse')
      .attr('cx', 60)
      .attr('rx', 60)
      .attr('cy', 20)
      .attr('ry', 20)
      .attr('fill', 'rgb(255, 255, 255)')
      .attr('stroke', 'rgb(0, 0, 0)');
  } else if (data.shape === 'MODULE') {
    g.append('rect')
      .attr('x', 0)
      .attr('y', 0)
      .attr('width', 120)
      .attr('height', 40)
      .attr('fill', 'rgb(255, 255, 255)')
      .attr('stroke', 'rgb(0, 0, 0)');
  }
  g.append('text')
    .attr('x', 60)
    .attr('y', 22)
    .attr('font-size', '12px')
    .attr('text-anchor', 'middle')
    .text(data.type);
  const childGroup = g.selectAll('g').data(data.children).enter().append('g').attr('transform', (d, i) => `translate(60, ${20 + i * 45})`);
  childGroup.append('path').attr('d', (d, i) => `M 0 ${i === 0 ? 20 : 0} L 0 50 L 20 50`).attr('fill', 'none').attr('stroke', 'rgb(0, 0, 0)');
  childGroup.append('g').attr('transform', 'translate(20, 30)').each(function (d) {
    makeGroup(this, d);
  });
}

function makeGraphStructure(data: DiscoveryFeatureMatrix[]): GraphElement[] {
  const moduleTypeMap = _.keyBy(data, 'moduleType');
  const elements: GraphElement[] = data.map(d => ({ type: d.moduleType, children: [], shape: 'MODULE' }));
  const elementsGrouped: GraphElement[] = [];
  elements.forEach(element => {
    const module = moduleTypeMap[element.type];
    if (module.representation.includes('ROOT_MODULE')) {
      element.children = module.contains.map(contains => ({ type: contains, children: [], shape: 'MODULE' }));
      elementsGrouped.push({type: 'SourceObject', children: [element], shape: 'SOURCE'});
    } else if (module.representation.includes('EXTERNAL_MODULE')) {
      elementsGrouped.push({type: '', children: [element], shape: 'MISSING'});
    }
  });

  return elementsGrouped;
}

function graphDepth(graph: GraphElement, depth: number): number {
  depth = depth + graph.children.length;
  return _.max(graph.children.map(child => graphDepth(child, depth))) ?? depth;
}