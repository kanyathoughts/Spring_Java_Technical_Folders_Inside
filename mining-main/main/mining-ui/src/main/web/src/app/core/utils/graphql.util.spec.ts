import { Usages } from "@app/shared/interfaces/datapoints-labels.interface";
import { getValueAtPath, graphQlQuery, replaceTemplateString, selectDataPointFilterType, buildFilterObject } from "./graphql.util";
import { MiningDataPointDefinitionWithPath } from "@innowake/mining-api-angular-client";

/* a selection of example data point from "Module", including two aliases */
const DATA_POINTS: MiningDataPointDefinitionWithPath[] = [
  {
    "name": "name",
    "parentTypeName": "Module",
    "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.String,
    "referenceTypeName": null,
    "parameters": [],
    "usages": new Set([
      "general.viewMode",
      "miningUi.annotationsTable",
      "general.sortBy",
      "miningUi.modulesTable",
      "general.searchFilter"
    ]),
    "usageAttributes": {
      "general.viewMode": {
        "displayAs": "link",
        "linkTemplate": "#/project-${$projectId}/module-${id}/details/overview",
        "togetherWith": "id"
      },
      "miningUi.annotationsTable": {
        "rsqlFragment": "$in.HasAnnotation.name=='${$query}*'",
        "category": "Base Data",
        "defaultColumnIndex": "0"
      },
      "general.searchFilter": {
        "filterMode": "text"
      },
      "miningUi.modulesTable": {
        "rsqlFragment": "name=='${$query}*'",
        "sortByFieldName": "name",
        "category": "Base Data",
        "defaultColumnIndex": "0"
      }
    },
    "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
    "displayName": "Module Name",
    "description": "The name of the Module",
    "path": "content.name",
    "array": false,
    "id": "Module.name",
    "nullable": true,
    "aliasFor": null,
    "alias": false
  },
  {
    "name": "linesOfCode",
    "parentTypeName": "Module",
    "scalarType": MiningDataPointDefinitionWithPath.ScalarTypeEnum.Int,
    "referenceTypeName": null,
    "parameters": [],
    "usages": new Set([
      "miningUi.modulesTable"
    ]),
    "usageAttributes": {
      "miningUi.modulesTable": {
        "category": "Metrics"
      }
    },
    "providedBy": new Set(["innowake.mining.data.model.springdata.ModuleV2"]),
    "displayName": "Lines of Code",
    "description": "Number of source code lines",
    "path": "content.linesOfCode",
    "aliasFor": null,
    "alias": false,
    "nullable": true,
    "array": false,
    "id": "Module.linesOfCode"
  },
  {
    "name": "inboundDependencyCount",
    "parentTypeName": "Module",
    "scalarType": null,
    "referenceTypeName": null,
    "parameters": [],
    "usages": new Set([
      "miningUi.modulesTable"
    ]),
    "usageAttributes": {
      "miningUi.modulesTable": {
        "category": "Dependencies"
      }
    },
    "providedBy": new Set(["innowake.mining.server.graphql.controller.ModulesGraphQlController"]),
    "displayName": "Number of Inbound Dependencies",
    "description": "Number of all Modules that directly depend on this Module",
    "path": "content.inboundDependencyCount",
    "aliasFor": {
      "aliasFor": "dependencyCount",
      "subSelection": "",
      "parameters": [
        "direction: IN"
      ]
    },
    "alias": true,
    "nullable": false,
    "array": false,
    "id": "Module.inboundDependencyCount"
  },
  {
    "name": "inboundDependencyNames",
    "parentTypeName": "Module",
    "scalarType": null,
    "referenceTypeName": null,
    "parameters": [],
    "usages": new Set([
      "miningUi.modulesTable"
    ]),
    "usageAttributes": {
      "miningUi.modulesTable": {
        "category": "Dependencies"
      }
    },
    "providedBy": new Set(["innowake.mining.server.graphql.controller.ModulesGraphQlController"]),
    "displayName": "Inbound Dependencies",
    "description": "List of all Modules that directly depend on this Module",
    "path": "content.inboundDependencyNames.name",
    "aliasFor": {
      "aliasFor": "dependencies",
      "subSelection": "name",
      "parameters": [
        "direction: IN"
      ]
    },
    "alias": true,
    "nullable": false,
    "array": false,
    "id": "Module.inboundDependencyNames"
  }
];

describe('GraphQlUtil', () => {

  it('should create a GraphQL query from a list of data points with filter object', () => {
    const query = graphQlQuery('modules', { filter: `name=='MMRS0815'` }, DATA_POINTS, [], true);
    expect(query).toEqual(`query($filter: FilterObject_modules) { modules(filter: "name=='MMRS0815'") { content { name id linesOfCode inboundDependencyCount: dependencyCount(direction: IN) inboundDependencyNames: dependencies(direction: IN) { name } } } }`)
  });

  it('should create a GraphQL query from a list of data points without filter object', () => {
    const query = graphQlQuery('modules', { filter: `name=='MMRS0815'` }, DATA_POINTS, []);
    expect(query).toEqual(`query { modules(filter: "name=='MMRS0815'") { content { name id linesOfCode inboundDependencyCount: dependencyCount(direction: IN) inboundDependencyNames: dependencies(direction: IN) { name } } } }`)
  });

  it('should replace template string', () => {
    const template = 'name=="${query}*"';
    const context = { query: ['PRGD'] };
    const value = replaceTemplateString(DATA_POINTS[0], template, context, {});
    expect(value).toBe('name=="*"');
  });

  it('should should return filter type', () => {
    const filterMode = selectDataPointFilterType(DATA_POINTS[0], Usages.MODULETABLE);
    expect(filterMode).toBe('freeText');
    DATA_POINTS[0].usageAttributes['general.searchFilter'].filterMode = 'number';
    const numberFilterMode = selectDataPointFilterType(DATA_POINTS[0], Usages.MODULETABLE);
    expect(numberFilterMode).toBe('number');
    DATA_POINTS[0].usageAttributes['general.searchFilter'].filterMode = 'multiSelect';
    const multiSelectFilterMode = selectDataPointFilterType(DATA_POINTS[0], Usages.MODULETABLE);
    expect(multiSelectFilterMode).toBe('multiSelect');
    const noFilterMode = selectDataPointFilterType(DATA_POINTS[1], Usages.MODULETABLE);
    expect(noFilterMode).toBe(undefined);
  });

  it('should check getValueAtPath', () => {
    const value1 = getValueAtPath({}, 'content.name');
    expect(value1).toBeUndefined();
    const value2 = getValueAtPath({}, '');
    expect(value2).toBeUndefined();
    const value3 = getValueAtPath(null, '');
    expect(value3).toBe(null);
    const value4 = getValueAtPath(['test'], '');
    expect(value4).toBeUndefined;
  });

  it('should test buildFilterObject', () => {
    const content = [{name: 'name', path: 'content.name'}];
    const filterObject: { [key: string]: any } = {
      "content_name": {
          "eq": "MMRS7101"
      }
  };
    const graphQlFilter = buildFilterObject(content, [['MMRS7101']]);
    expect(graphQlFilter).toEqual(filterObject);
  });
});
