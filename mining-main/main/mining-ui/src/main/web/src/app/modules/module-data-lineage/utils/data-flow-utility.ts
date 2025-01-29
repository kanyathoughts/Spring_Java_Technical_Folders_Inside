import { DataLineageGraphInfo } from '../models/data-lineage-graph.info';
import { DataLineageEdge } from '../models/data-lineage.edge';
import { DataLineageNode } from '../models/data-lineage.node';
import { DataLineageGroupNode } from '../models/data-lineage.groupnode';
import { DataFlowGraphNode } from '@innowake/mining-api-angular-client';

enum NodeType {
  MODULE = 'MODULE',
  DATA_INTERFACE = 'DATA_INTERFACE',
  INCOMING = 'INCOMING',
  OUTGOING = 'OUTGOING',
  BOTH = 'BOTH'
}

enum GroupType {
  DATA_INTERFACES = 'Data-Interfaces',
  INCOMING = 'Incoming',
  OUTGOING = 'Outgoing'
}
export class DataFlowUtility {

  inputOutputFields: string[] = [];

  /**
   * Method that parses through the DataFlowGraph data received from the REST call and
   * returns the graph information that can be displayed to the user.
   *
   * @param dataFlowGraph - The DataFlowGraph data received from the REST call
   * @returns - Data Lineage GraphInfo that contains all the nodes and edges to be created.
   */
  getDataLineageGraphInfo(nodes: DataFlowGraphNode[]): DataLineageGraphInfo {
    const graphInfo: DataLineageGraphInfo = new DataLineageGraphInfo();
    const groups = {
      'Data-Interfaces': new Set<string>(),
      'Incoming': new Set<string>(),
      'Outgoing': new Set<string>()
    };
    nodes.forEach((node: DataFlowGraphNode) => {
      if ((node.name === 'Write' || node.name === 'Read' || node.name === 'Rewrite') &&
        (Array.from(node.incomings).some(incoming => incoming.includes('field')) ||
        Array.from(node.outgoings).some(outgoing => outgoing.includes('field')))) {
        const filteredIncomings = Array.from(node.incomings).filter(incoming => incoming.includes('field'));
        const filteredOutgoings = Array.from(node.outgoings).filter(outgoing => outgoing.includes('field'));
        this.inputOutputFields.push(...filteredIncomings, ...filteredOutgoings);
      }
      if (node.type === NodeType.MODULE) {
        const moduleGroup: DataLineageGroupNode = {
          id: node.id,
          label: node.name,
          type: node.type
        };
        graphInfo.graphGroups.push(moduleGroup);
      } else if (node.type === 'STATEMENT' && node.statementLabel) {
        let offset;
        let length;
        if (node.sourceLocation) {
          offset = node.sourceLocation.moduleLocation.offset;
          length = node.sourceLocation.moduleLocation.length;
        } else if (node.location) {
          offset = node.location.offset;
          length = node.location.length;
        }
        const statementGroup: DataLineageGroupNode = {
          id: node.id,
          label: node.name,
          type: 'STATEMENT',
          direction: node.direction,
          statementLabel: node.statementLabel.replace(new RegExp(node.name, 'i'), ''),
          parentGroup: node.parentModule,
          offset,
          length
        };
        graphInfo.graphGroups.push(statementGroup);
      } else if (node.type === NodeType.DATA_INTERFACE) {
        this.addNodeAndGroup(node, GroupType.DATA_INTERFACES, graphInfo, groups);
      // Commented out the below code as a part of ticket WMIN-13625 temporarily and Enable once the spike ticket WMIN-13627 is completed.
      // } else if (node.direction && (node.direction === NodeType.BOTH || node.direction === NodeType.INCOMING) && node.name !== fieldName) {
      //   this.addNodeAndGroup(node, GroupType.INCOMING, graphInfo, groups);
      // } else if (node.direction && node.direction === NodeType.OUTGOING) {
      //   this.addNodeAndGroup(node, GroupType.OUTGOING, graphInfo, groups);
      } else {
        const elementNode = new DataLineageNode(node, node.parentModule);
        graphInfo.graphNodes.push(elementNode);
      }
    });

    /* Add edges to graph */
    nodes.forEach((node: DataFlowGraphNode) => {
      node.outgoings.forEach((descendantId: string) => {
        const edge: DataLineageEdge = {};
        edge.fromId = node.id;
        edge.toId = descendantId;
        graphInfo.graphEdges.push(edge);
      });
    });
    return graphInfo;
  }

  private addNodeAndGroup(node: DataFlowGraphNode, label: string, graphInfo: DataLineageGraphInfo, groups: any) {
    const groupId = `${node.parentModule}-${label}`;
    const newNode = new DataLineageNode(node, groupId);
    graphInfo.graphNodes.push(newNode);
    if ( ! groups[label].has(groupId)) {
      const newGroup: DataLineageGroupNode = {
        id: groupId,
        label,
        parentGroup: node.parentModule,
        type: node.type
      };
      graphInfo.graphGroups.push(newGroup);
      groups[label].add(groupId);
    }
  }
}
