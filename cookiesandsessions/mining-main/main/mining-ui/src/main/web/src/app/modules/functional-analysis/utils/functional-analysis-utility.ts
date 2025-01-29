import { ShapeNodeShape } from 'yfiles';
import { FunctionalAnalysisGraphInfo } from '../models/functional-analysis-graph-info';
import { FunctionalAnalysisNode } from '../models/functional-analysis-node';
import { Logger } from '@app/core';
import { DEFAULT_NODE_SHAPE } from '../../graph/utils/graph-global-styles';
import { ControlFlowNode, ControlFlowEdge, ControlFlowGraph } from '@innowake/mining-api-angular-client';

const logger = new Logger('ControlFlowUtility');

export const CONTROL_FLOW_NODE_SHAPE_MAPPING: { [type: string]: ShapeNodeShape } = {
  'terminal': ShapeNodeShape.ROUND_RECTANGLE,
  'decision': ShapeNodeShape.HEXAGON,
  'process': DEFAULT_NODE_SHAPE,
  'group': ShapeNodeShape.RECTANGLE
};

export const DEFAULT_NON_EXPANDABLE_NODE_WIDTH = 375;

export const BRANCH_NODE_STROKE_THICKENESS = 2;

export class ControlFlowUtility {

  /**
   * Method that parses through the ControlFlowGraph data received from the REST call and
   * returns the graph information that can be displayed to the user.
   *
   * @param graph - The ControlFlowGraph data received from the REST call
   * @param moduleName - The name of the module the CFG is calculated for
   * @returns - Control Flow GraphInfo that contains all the nodes and edges to be created. Return null if data is corrupted.
   */
  getGraphInfo(graph: ControlFlowGraph, moduleName: string): FunctionalAnalysisGraphInfo {
    const graphInfo: FunctionalAnalysisGraphInfo = new FunctionalAnalysisGraphInfo();
    const edgeMap: { [fromTo: string]: ControlFlowEdge } = {};
    const nodeMap: { [id: string]: FunctionalAnalysisNode } = {};

    /* Build nodes without group information */
    graph.nodes.forEach((node: ControlFlowNode) => {
      /* groupName set to null since groups are not created at this point */
      const cfgNode: FunctionalAnalysisNode = new FunctionalAnalysisNode(node, moduleName, null);
      graphInfo.graphNodes.push(cfgNode);
      nodeMap[node.id] = cfgNode;
      if (node.type === 'CobolCopyStmt') {
      nodeMap[node.id].skippable = true;
      }
    });

    const groupMap: { [id: string]: FunctionalAnalysisNode } = {};
    /* Build groups */
    graph.nodes.forEach((node: ControlFlowNode) => {
      if (node.superTypes != null && Array.from(node.superTypes).includes('CfgCollapsibleNode')) {
        /* create new group */
        const cfgGroup = new FunctionalAnalysisNode(node, moduleName, this.getFirstParentGroup(node, groupMap, nodeMap)?.recordId, true);
        graphInfo.graphGroups.push(cfgGroup);
        groupMap[node.id] = cfgGroup;
        nodeMap[node.id].skippable = true;
      } else if (node.type === 'CobolSizeGuardedStmt') {
        nodeMap[node.id].skippable = true;
      }
    });

    /* Add group information to nodes */
    graph.nodes.forEach((node: ControlFlowNode) => {
      if ( ! nodeMap[node.id]) {
        return;
      }
      const cfgNode = nodeMap[node.id];
      const group = this.getFirstParentGroup(node, groupMap, nodeMap);
      if (groupMap[node.id]) {
        cfgNode.group = node.id;
        cfgNode.group = group ? group.recordId : node.id;
        groupMap[node.id].group = group?.recordId;
      } else {
        const group = this.getFirstParentGroup(node, groupMap, nodeMap);
        group?.children.push(cfgNode.recordId);
        cfgNode.group = group?.recordId;
      }
    });

    const graphProcessResult = graph.edges.every(controlFlowEdge => {
      const fromId = controlFlowEdge.fromId;
      const toId = controlFlowEdge.toId;
      if( ! (nodeMap[fromId] && nodeMap[toId])) {
        logger.warn('Node not found for IDs: ', fromId, toId);
        return false;
      } else if (nodeMap[fromId].skippable) {
        /* If the edge goes out of a node that exists as a group we don't want to draw it */
        return true;
      }
      if (nodeMap[toId].skippable) {
        controlFlowEdge.toId = this.getFirstOutgoingNode(toId, graph, nodeMap, new Set<string>());
      }
      this.processCfgNodeEdge(controlFlowEdge, edgeMap, graphInfo);
      return controlFlowEdge.toId;
    });

    graphInfo.graphNodes = graphInfo.graphNodes.filter(node => ! node.skippable);
    graphInfo.graphGroups = graphInfo.graphGroups.filter(group => group.children?.length > 0);
    return graphProcessResult ? graphInfo : null;
  }

  /**
   * Finds the outgoing edges of a specific node and returns the rid of the node the first outgoing edge points to.
   * @param nodeId id of the node
   * @param graph graph the node is part of
   * @returns record id of node referenced by first outgoing edge or null if no outgoing edges exist
   */
  getFirstOutgoingNode(nodeId: string, graph: ControlFlowGraph, nodeMap: { [id: string]: FunctionalAnalysisNode }, visitedNodes: Set<string>): string {
    const filteredEdges = graph.edges.filter(edge => edge.fromId === nodeId);
    if (filteredEdges.length > 0) {
      const toId = filteredEdges[0].toId;
      const toNode = nodeMap[toId];
      if (toNode.skippable) {
        if (visitedNodes.has(toId)) {
          logger.error('There is a cycle which only contains skippable nodes: ', visitedNodes);
          return null;
        }
        visitedNodes.add(nodeId);
        return this.getFirstOutgoingNode(toId, graph, nodeMap, visitedNodes);
      }
      return filteredEdges[0].toId;
    }
    logger.warn('no outgoing edges found');
    return null;
  }

  /**
   * Method that traverses the parent hierarchy of a node to get a parent that is a groupNode.
   * This is used for transitive group assignments. (Because we only create groups for nodes with certain types)
   *
   * @param node - group node for this is searched
   * @param groupMap - maps ids to existing group nodes
   * @param nodeMap - maps ids to existing nodes
   * @returns - first node in parent hierarchy which is a group or null if no parent group is found
   */
  getFirstParentGroup(
    node: ControlFlowNode,
    groupMap: { [id: string]: FunctionalAnalysisNode },
    nodeMap: { [id: string]: FunctionalAnalysisNode } = {}
  ): FunctionalAnalysisNode {
    const parentGroup = groupMap[node.parent];
    const parentNode: ControlFlowNode = nodeMap[node.parent];
    if (parentGroup) {
      return parentGroup;
    } else if (parentNode) {
      return this.getFirstParentGroup(parentNode, groupMap, nodeMap);
    }
    return null;
  }

  /**
   * Method that validates the ControlFlowEdge and adds it to the graphInfo.
   *
   * @param controlFlowEdge - The edge that must be validated
   * @param edgeMap - A map of all edges that have been created
   * @param graphInfo - Graph information to populate the edges
   */
  processCfgNodeEdge(controlFlowEdge: ControlFlowEdge, edgeMap: { [fromTo: string]: ControlFlowEdge }, graphInfo: FunctionalAnalysisGraphInfo): void {
    const fromId = controlFlowEdge.fromId;
    const toId = controlFlowEdge.toId;
    const label = controlFlowEdge.label;
    const key: string = '<' + fromId + ',' + toId + ',' + label + '>';
    if ( ! edgeMap[key]) {
      edgeMap[key] = controlFlowEdge;
      graphInfo.graphEdges.push(controlFlowEdge);
    }
  }
}
