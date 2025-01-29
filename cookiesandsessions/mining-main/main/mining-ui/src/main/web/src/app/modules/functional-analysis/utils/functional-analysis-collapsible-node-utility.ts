import { ShapeNodeShape } from 'yfiles';
import { Logger } from '@app/core';
import { FunctionalAnalysisCollapsibleNodeGraphInfo } from '../models/functional-analysis-collapsible-node-graph-info';
import { FunctionlAnalysisCollapsibleNode } from '../models/functional-analysis-collapsible-node';
import { ControlFlowNode, ControlFlowEdge } from '@innowake/mining-api-angular-client';

const logger = new Logger('ControlFlowUtility');

export class ControlFlowCollapsibleNodeUtility {

  /**
   * Method that parses through the Ast Nodes and returns nodes to be diaplyed in the graph.
   * @param astNodes - Nodes that are received from the back-end
   * @returns - Control Flow GraphInfo that contains all the nodes and edges to be created. Return null if data is corrupted.
   */
  getGraphInfo(astNodes: any[], moduleName: string): FunctionalAnalysisCollapsibleNodeGraphInfo {
    const graphInfo: FunctionalAnalysisCollapsibleNodeGraphInfo = new FunctionalAnalysisCollapsibleNodeGraphInfo();
    const edgeMap: { [fromTo: string]: ControlFlowEdge } = {};
    const nodeMap: { [id: string]: FunctionlAnalysisCollapsibleNode } = {};
    astNodes.forEach((node: ControlFlowNode) => {
      const cfgNode: FunctionlAnalysisCollapsibleNode = new FunctionlAnalysisCollapsibleNode(node, moduleName);
      graphInfo.graphNodes.push(cfgNode);
      nodeMap[node.id] = cfgNode;
    });
    const graphProcessResult = graphInfo.graphNodes.every(graphNode => {
      let result = graphNode.fromLinks.every((fromLink: string) => {
        if (nodeMap[fromLink]) {
          this.processCfgNodeEdge(nodeMap[fromLink], graphNode, edgeMap, graphInfo);
          return true;
        } else {
          logger.warn('Node not found for ID: ', fromLink);
          return false;
        }
      });
      result = result && graphNode.toLinks.every((toLink: string) => {
        if (nodeMap[toLink]) {
          this.processCfgNodeEdge(graphNode, nodeMap[toLink], edgeMap, graphInfo);
          return true;
        } else {
          logger.warn('Node not found for ID: ', toLink);
          return false;
        }
      });
      return result;
    });
    return graphProcessResult ? graphInfo : null;
  }

  /**
   * Method that creates an edge between two given nodes.
   * @param from - Node from where the edge originates
   * @param to - Node to which the edge connects
   * @param edgeMap - A map of all edges that have been created
   * @param graphInfo - Graph information to populate the edges
   */
  processCfgNodeEdge(
    from: FunctionlAnalysisCollapsibleNode,
    to: FunctionlAnalysisCollapsibleNode,
    edgeMap: { [fromTo: string]: ControlFlowEdge },
    graphInfo: FunctionalAnalysisCollapsibleNodeGraphInfo
  ): void {
    const key: string = '<' + from.recordId + ',' + to.recordId + '>';
    if (!edgeMap[key]) {
      const cfgEdge: ControlFlowEdge = {
        fromId: from.recordId,
        toId: to.recordId,
        label: this.getEdgeLabelText(from, to)
      };
      edgeMap[key] = cfgEdge;
      graphInfo.graphEdges.push(cfgEdge);
    }
  }

  /**
   * Method to create the label for an edge.
   * @param fromNode - Node from where edge originates
   * @param toNode - Node where the edge connects
   * @returns string return true false or empty string
   */
  getEdgeLabelText(fromNode: FunctionlAnalysisCollapsibleNode, toNode: FunctionlAnalysisCollapsibleNode): string {
    if (fromNode.isNodeBranch) {
      if (toNode.nodeDescription === 'CobolThenBlock') {
        return 'true';
      } else {
        return 'false';
      }
    } else {
      return '';
    }
  }
}

export const CONTROL_FLOW_NODE_SHAPE_MAPPING: { [type: string]: ShapeNodeShape } = {
  'terminal': ShapeNodeShape.ELLIPSE,
  'decision': ShapeNodeShape.DIAMOND,
  'process': ShapeNodeShape.RECTANGLE
};

export const DEFAULT_NODE_HEIGHT = 100;
export const DEFAULT_NODE_WIDTH = 285;
export const DEFAULT_NON_EXPANDABLE_NODE_WIDTH = 375;
