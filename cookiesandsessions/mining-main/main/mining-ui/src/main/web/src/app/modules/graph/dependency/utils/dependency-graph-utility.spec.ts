import { GraphUtility } from '../utils/dependency-graph-utility';
import { NodeType, NODE_CONFIG } from '../../utils/node-configurations';
import { YFileGraphInfo } from '../../models/yfile-graph-info.model';
import { DependencyGraph, ModulePojo, ModuleRelationshipPojo } from '@innowake/mining-api-angular-client';
describe('DependencyGraphUtility', () => {

  it('should get all details in graph info', () => {
    const firstModule: ModulePojo = {
      uid: '#-1:-1',
      customProperties: {},
      id: 2037,
      name: 'MMRS710J.STEP02.MMRS7101',
      projectId: 3,
      path: null,
      technology: 'JCL',
      type: 'EXEC_PGM',
      storage: 'FILE_SECTION',
      identification: 'IDENTIFIED',
      origin: 'CUSTOM',
    };
    const secondModule: ModulePojo = {
      uid: '#-1:-1',
      customProperties: {},
      id: 2038,
      name: 'MMRS710J.STEP02.MMRS7101',
      projectId: 3,
      path: null,
      technology: 'JCL',
      type: 'EXEC_PGM',
      storage: 'FILE_SECTION',
      identification: 'IDENTIFIED',
      origin: 'CUSTOM',
    };
    const link: ModuleRelationshipPojo = {srcModule: '2037', dstModule: '2038'};
    const nodeTypes: NodeType[] = [NodeType[`EXEC_PGM`]];
    const linkTypes: string[] = ['CALLS'];
    const dependencyGraphLinks: DependencyGraph = {
      modules: [],
      references: [],
      moduleTypes: [],
      relationshipTypes: []
    };
    const graphInfo: YFileGraphInfo = GraphUtility.getYFilesGraphInfo(dependencyGraphLinks);
    graphInfo.graphNodes = [firstModule, secondModule];
    graphInfo.graphlinks = [link];
    graphInfo.graphNodeTypes = nodeTypes;
    graphInfo.graphRelationships = linkTypes;

    expect(graphInfo.graphNodes).toEqual([firstModule, secondModule]);
    expect(graphInfo.graphlinks).toEqual([link]);
    expect(graphInfo.graphNodeTypes).toEqual(nodeTypes);
    expect(graphInfo.graphRelationships).toEqual(linkTypes);
  });

  it('should have image url for module types', () => {
    const graph: DependencyGraph = {
      modules: [
        {technology: 'IMS', type: 'UTILITY', id: 0}, {technology: 'RESOURCE', type: 'RESOURCE' as any, id: 1},
        {technology: 'RESOURCE', type: 'TSQ', id: 2}, {technology: 'RESOURCE', type: 'TDQ', id: 3},
        {technology: 'IMS', type: 'TDFXTRCT', id: 4}, {technology: 'RESOURCE', type: 'LISTCAT', id: 5},
        {technology: 'C', type: 'PROGRAM', id: 6}, {technology: 'C', type: 'HEADER', id: 7},
        {technology: 'UNKNOWN', type: 'UNKNOWN', id: 8}
      ],
      references: [
        {srcModule: '0', dstModule: '1'}, {srcModule: '0', dstModule: '2'}, {srcModule: '0', dstModule: '3'},
        {srcModule: '0', dstModule: '4'}, {srcModule: '0', dstModule: '5'}, {srcModule: '0', dstModule: '6'},
        {srcModule: '0', dstModule: '7'}, {srcModule: '0', dstModule: '8'}
      ],
      moduleTypes: ['IMS UTILITY', 'RESOURCE TSQ', 'RESOURCE RESOURCE', 'IMS TDFXTRCT', 'C PROGRAM', 'RESOURCE LISTCAT', 'C HEADER'],
      relationshipTypes: ['CALLS', 'READS_WRITES']
    };
    const graphInfo = GraphUtility.getYFilesGraphInfo(graph);
    graphInfo.nodes.forEach(node => {
      expect(node.style.imageUrl).toEqual(NODE_CONFIG.GENERIC.imageUrl);
    });
  });

  Object.keys(NodeType).forEach(nodeType => {
    it('should check ' + nodeType + ' icon and colour', () => {
      if (nodeType === 'VISUAL_CLUE') {
        return; /* visual clue nodes don't have a technology so getGraphTest doesn't work for this */
      }
      const graphInfo: YFileGraphInfo = getGraphTest(nodeType);
      expect(graphInfo.nodes[0].style.imageUrl).toEqual(NODE_CONFIG[nodeType].imageUrl);
      expect(graphInfo.nodes[0].style.color.fillColor).toEqual(NODE_CONFIG[nodeType].color.fillColor);
      expect(graphInfo.nodes[0].style.color.strokeColor).toEqual(NODE_CONFIG[nodeType].color.strokeColor);
    });
  });

  it('should check GENERIC Node icon and colour', () => {
   const graphInfo: YFileGraphInfo = getGraphTest('TEST UNDISCOVERED');
    expect(graphInfo.nodes[0].style.imageUrl).toEqual(NODE_CONFIG.GENERIC.imageUrl);
    expect(graphInfo.nodes[0].style.color.fillColor).toEqual(NODE_CONFIG.GENERIC.color.fillColor);
    expect(graphInfo.nodes[0].style.color.strokeColor).toEqual(NODE_CONFIG.GENERIC.color.strokeColor);
  });

  it('browser crashes while filtering a graph with a self referencing module in it', () => {
    // tslint:disable-next-line:max-line-length
    const dependencyGraphLinks: DependencyGraph = JSON.parse('{"modules":[{"recordId":"#-1:-1","customProperties":[],"id":1,"name":"one","projectId":3,"path":null,"technology":"JCL","type":"EXEC_PGM","storage":"FILE_SECTION","identification":"IDENTIFIED","origin":"CUSTOM","info":null,"description":null,"linesOfCode":null,"linesOfComment":null,"complexity":null,"content":null},{"recordId":"#-1:-1","customProperties":[],"id":2,"name":"two","projectId":3,"path":null,"technology":"JCL","type":"EXEC_PGM","storage":"FILE_SECTION","identification":"IDENTIFIED","origin":"CUSTOM","info":null,"description":null,"linesOfCode":null,"linesOfComment":null,"complexity":null,"content":null},{"recordId":"#-1:-1","customProperties":[],"id":3,"name":"three","projectId":3,"path":null,"technology":"JCL","type":"JOB","storage":"FILE","identification":"IDENTIFIED","origin":"CUSTOM","info":null,"description":null,"linesOfCode":null,"linesOfComment":null,"complexity":null,"content":null},{"recordId":"#-1:-1","customProperties":[],"id":4,"name":"four","projectId":3,"path":null,"technology":"JCL","type":"EXEC_PGM","storage":"FILE_SECTION","identification":"IDENTIFIED","origin":"CUSTOM","info":null,"description":null,"linesOfCode":null,"linesOfComment":null,"complexity":null,"content":null},{"recordId":"#-1:-1","customProperties":[],"id":5,"name":"five","projectId":3,"path":null,"technology":"JCL","type":"EXEC_PGM","storage":"FILE_SECTION","identification":"IDENTIFIED","origin":"CUSTOM","info":null,"description":null,"linesOfCode":null,"linesOfComment":null,"complexity":null,"content":null}],"references":[{"recordId":null,"customProperties":[],"id":1,"relationship":"CALLS","fromId":"1","toId":"3","fromName":null,"toName":null,"fromModuleLocation":null,"toModuleLocation":null,"attributes":null,"properties":null},{"recordId":null,"customProperties":[],"id":2,"relationship":"READS_WRITES","fromId":"2","toId":"3","fromName":null,"toName":null,"fromModuleLocation":null,"toModuleLocation":null,"attributes":null,"properties":null},{"recordId":null,"customProperties":[],"id":3,"relationship":"READS_WRITES","fromId":"3","toId":"3","fromName":null,"toName":null,"fromModuleLocation":null,"toModuleLocation":null,"attributes":null,"properties":null},{"recordId":null,"customProperties":[],"id":4,"relationship":"READS_WRITES","fromId":"3","toId":"5","fromName":null,"toName":null,"fromModuleLocation":null,"toModuleLocation":null,"attributes":null,"properties":null}], "moduleTypes": ["JCL EXEC_PGM", "JCL JOB"], "relationshipTypes": ["CALLS", "READS_WRITES"]}');
    GraphUtility.getYFilesGraphInfo(dependencyGraphLinks);
    // no need to assert anything as the passing of this method is enough
    expect(true).toEqual(true);
  });

  it('browser crashes while filtering both the modules in a bi directional relationship', () => {
    // tslint:disable-next-line:max-line-length
    const dependencyGraphLinks: DependencyGraph = JSON.parse('{"modules":[{"recordId":"#-1:-1","customProperties":[],"id":1,"name":"one","projectId":3,"path":null,"technology":"JCL","type":"EXEC_PGM","storage":"FILE_SECTION","identification":"IDENTIFIED","origin":"CUSTOM","info":null,"description":null,"linesOfCode":null,"linesOfComment":null,"complexity":null,"content":null},{"recordId":"#-1:-1","customProperties":[],"id":2,"name":"two","projectId":3,"path":null,"technology":"JCL","type":"PROC","storage":"FILE_SECTION","identification":"IDENTIFIED","origin":"CUSTOM","info":null,"description":null,"linesOfCode":null,"linesOfComment":null,"complexity":null,"content":null},{"recordId":"#-1:-1","customProperties":[],"id":3,"name":"three","projectId":3,"path":null,"technology":"JCL","type":"JOB","storage":"FILE","identification":"IDENTIFIED","origin":"CUSTOM","info":null,"description":null,"linesOfCode":null,"linesOfComment":null,"complexity":null,"content":null}],"references":[{"recordId":null,"customProperties":[],"id":1,"relationship":"CALLS","fromId":"1","toId":"2","fromName":null,"toName":null,"fromModuleLocation":null,"toModuleLocation":null,"attributes":null,"properties":null},{"recordId":null,"customProperties":[],"id":2,"relationship":"READS_WRITES","fromId":"2","toId":"2","fromName":null,"toName":null,"fromModuleLocation":null,"toModuleLocation":null,"attributes":null,"properties":null},{"recordId":null,"customProperties":[],"id":3,"relationship":"READS_WRITES","fromId":"2","toId":"3","fromName":null,"toName":null,"fromModuleLocation":null,"toModuleLocation":null,"attributes":null,"properties":null},{"recordId":null,"customProperties":[],"id":4,"relationship":"READS_WRITES","fromId":"3","toId":"2","fromName":null,"toName":null,"fromModuleLocation":null,"toModuleLocation":null,"attributes":null,"properties":null}],"moduleTypes": ["JCL EXEC_PGM", "JCL PROC"], "relationshipTypes": ["CALLS", "READS_WRITES"] }');
    GraphUtility.getYFilesGraphInfo(dependencyGraphLinks);
    // no need to assert anything as the passing of this method is enough
    expect(true).toEqual(true);
  });

  it('should test moduleTypes and relationshipTypes', () => {
    const graphInfo: YFileGraphInfo = getGraphTest('COBOL COPYBOOK');
    const nodeType = 'COBOL COPYBOOK';
    const nodeTypes: NodeType[] = [nodeType as NodeType];
    expect(graphInfo.nodeTypes).toEqual(nodeTypes);
    expect(graphInfo.relationships).toEqual(['CALLS', 'READS_WRITES']);
  });

  /**
   * Generate a Graph for testing purposes
   * @param nodeTypeEnumValue : a NodeType enum value as a string
   */
  function getGraphTest(nodeTypeEnumValue: string): YFileGraphInfo {
    let nodeTypeEnumSplit: string[] = nodeTypeEnumValue.split(' ');
    const nodeTechnology: ModulePojo.TechnologyEnum = nodeTypeEnumSplit[0] as ModulePojo.TechnologyEnum;
    nodeTypeEnumSplit = nodeTypeEnumSplit.slice(1);
    const nodeType: ModulePojo.TypeEnum = nodeTypeEnumSplit.toString().replace(/,/g, '_') as ModulePojo.TypeEnum;
    const graph: DependencyGraph = {
      modules: [
        {technology: nodeTechnology, type: nodeType, id: 0},
        {technology: nodeTechnology, type: nodeType, id: 1},
      ],
      references: [
        {srcModule: '0', dstModule: '1'}
      ],
      moduleTypes: [`${nodeTechnology} ` + `${nodeType}`],
      relationshipTypes: ['CALLS', 'READS_WRITES']
    };
    return GraphUtility.getYFilesGraphInfo(graph);
  }
});

