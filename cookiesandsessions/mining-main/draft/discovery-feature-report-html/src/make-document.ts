import { JSDOM } from "jsdom";
import _, { Dictionary } from "lodash";
import * as d3 from 'd3';
import { DiscoveryFeatureMatrix } from "./model";
import { makeModuleTypeGraph } from "./make-module-type-graph.js";
import { makeDependecyGraph } from "./make-dependency-graph.js";

import purecss from 'purecss';

export async function makeDocument(model: DiscoveryFeatureMatrix[]): Promise<JSDOM> {
  const baseStyle = purecss.getFile('base-min.css');
  const dom = new JSDOM(`<!DOCTYPE html><html><head><style>${baseStyle}</style></head><body style="margin: 1em"></body></html>`);

  global.HTMLDivElement = dom.window.HTMLDivElement;
  global.SVGGElement = dom.window.SVGSVGElement
  global.DOMParser = dom.window.DOMParser;
  global.Node = dom.window.Node;

  const sortedModel = _.sortBy(model, 'technology', 'type');
  const modelGroupedByTechnology: Dictionary<DiscoveryFeatureMatrix[]> = _.groupBy(sortedModel, 'technology');

  const technologyParagraph = d3.select(dom.window.document.body)
    .selectAll('p')
    .data(Object.values(modelGroupedByTechnology))
    .enter()
    .append('p');

    technologyParagraph.append('h1').text(d => d[0].technology);

    makeParagraph(technologyParagraph);

  return dom;
}

function makeParagraph(paragraph: d3.Selection<HTMLParagraphElement, DiscoveryFeatureMatrix[], HTMLElement, unknown>) {
  paragraph.append('h2').text('Module Types and Structure');

  makeModuleTypeGraph(paragraph);

  paragraph.append('h2').text('Relationships');

  makeDependecyGraph(paragraph);

  paragraph.append('h2').text('Supported Features');

  const featureDiv = paragraph.selectAll('div.features').data(d => d).enter().append('div').attr('class', 'features');
  featureDiv.append('h3').text(d => d.type);

  const featureList = featureDiv.append('ul');
  featureList.selectAll('li').data(d => d.fullySupportedFeatures).enter()
    .append('li').text(d => d);
}