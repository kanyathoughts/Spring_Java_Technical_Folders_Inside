import { NodeType, NODE_CONFIG } from '../../utils/node-configurations';
import { LARGE_GRAPH_CUTOFF } from '../../utils/graph-layouts';
import { IGraph } from 'yfiles';
import { YFileGraphInfo, YNode } from '../../models/yfile-graph-info.model';
import { DependencyGraph, ModulePojo, ModuleRelationshipPojo } from '@innowake/mining-api-angular-client';

/**
 * The utility class which contains the static methods for graph operations
 * such as graph create, filter etc.
 */
export class GraphUtility {

    static ARTIFICIAL = 'Artificial';
    static ARTIFICIAL_EDGE = 'Artificial_edge';

    static OUT = 'OUT';
    static IN = 'IN';
    static MAX_GRAPH_NODES = 800;

    private static currClueNodeId = -1;
    private static currClueEdgeId = -1;

    /**
     * Returns the YFilesGraphInfo with cloned and modified dependency graph links.
     * @param dependencyGraph api data describing links and nodes
     * @param rootModules set of modules that will be root nodes of the graph
     */
    public static getYFilesGraphInfo(dependencyGraph: DependencyGraph): YFileGraphInfo {
      const nodes: YNode[] = [];
      const links: ModuleRelationshipPojo[] = [];
      const nodeTypes: NodeType[] = [];
      const relationships: string[] = [];
      const rootModules = new Set<number>(dependencyGraph?.rootModuleIds);
      if (dependencyGraph) {
        dependencyGraph.modules.forEach(module => {
          const nodeImageKey = (module.technology + ' ' + module.type).replace(/_/g, ' ');
          const nodeConfigMap = NODE_CONFIG[nodeImageKey] ? NODE_CONFIG[nodeImageKey] : NODE_CONFIG[NodeType.GENERIC];
          const peerCount = module?.info?.peerCount as unknown as string;
          const moduleClone: YNode = JSON.parse(JSON.stringify(module));
          moduleClone.style = nodeConfigMap;
          moduleClone.peerCount = peerCount;
          nodes.push(moduleClone);
        });

        dependencyGraph.moduleTypes.forEach((nodeType: NodeType) => {
          nodeTypes.push(nodeType);
        });
        dependencyGraph.references.forEach((link: ModuleRelationshipPojo) => {
          links.push(JSON.parse(JSON.stringify(link)) as ModuleRelationshipPojo);
        });
        dependencyGraph.relationshipTypes.forEach((relationship: string) => {
          relationships.push(relationship);
        });
      }
      const yFileGraphInfo = new YFileGraphInfo(nodes, links, nodeTypes, relationships);

      if (rootModules && rootModules.size > 0) {
        yFileGraphInfo.nodes.forEach(n => {
          if (rootModules.has(n.id)) {
            n.isRoot = true;
          }
        });
      } else if (yFileGraphInfo.nodes.length > 0) {
        yFileGraphInfo.nodes[0].isRoot = true;
      }
      return yFileGraphInfo;
    }

    /**
     * Returns if a graph is Large or not.
     * The graph is defined as Large if the number of
     * nodes and edges exceed the CUTOFF value.
     *
     * @param graph - The graph to be tested
     * @returns true if the graph is large. Returns false otherwise
     */
    public static isLarge(graph: IGraph): boolean {
      return (graph.nodes && graph.edges) && (graph.nodes.size >= LARGE_GRAPH_CUTOFF && graph.edges.size >= LARGE_GRAPH_CUTOFF);
    }

    public static enableInfoAlert(graph: IGraph): boolean {
      return graph.nodes.size === GraphUtility.MAX_GRAPH_NODES;
    }

    public static getNewClueNodeId(): number {
      return GraphUtility.currClueNodeId--;
    }

    public static getNewClueEdgeId(): number {
      return GraphUtility.currClueEdgeId--;
    }

    private static getNodeType(module: ModulePojo): NodeType {
      let nodeType: NodeType = NodeType[module.technology + ' ' + module.type.replace(/_/g, ' ')];
        if (! nodeType)  {
          nodeType = NodeType.GENERIC;
        }
      return nodeType;
    }
}
