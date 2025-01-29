import { FilterType } from '@app/shared/components/mining-table/mining-table-config.interface';

import UsagesEnum = UsagesModel.UsagesEnum;
import SearchFilterAttributesEnum = UsagesModel.SearchFilterAttributesEnum;
import ViewModeAttributesEnum = UsagesModel.ViewModeAttributesEnum;
import { AliasDefinition, MiningDataPointDefinitionWithPath, UsagesModel } from '@innowake/mining-api-angular-client';

const PLACEHOLDER_PATTERN = /\$(')?{([a-zA-Z0-9_$\.]+)}/g;

/**
 * Interface for the graphQl.
 */
interface Selection {
  field: string;
  parameters?: { [key: string]: any };
  subSelections?: Selection[];
  aliasDefinition?: AliasDefinition;
  togetherWith?: string;
}

interface GraphQLFilterValueObject {
  operator: string;
  value: string;
}

/**
 * Method is used to create the requested graphQl query.
 * @param query string variable for which query needs to be generated
 * @param parameters param to be part of the query parameter.
 * @param dataPoints are the list of dataPoints requested in graphQl
 * @param additionalParameters are the list of fixed datPoints.
 * @returns the value of the graphQlQuery.
 */
export const graphQlQuery = (
  query: string,
  parameters: { [key: string]: any },
  dataPoints: MiningDataPointDefinitionWithPath[],
  additionalParameters: string[],
  useFilterObject: boolean = false
): string => {
    const root: Selection = {
      field: query,
      parameters,
      subSelections: []
    };
    for (const dataPoint of dataPoints) {
      const pathSegments = dataPoint.path.split('.');
      const aliasDefinition: AliasDefinition = dataPoint.aliasFor;
      const togetherWith = dataPoint.usageAttributes?.[UsagesEnum.general_viewMode]?.[ViewModeAttributesEnum.togetherWith];
      putSelectionAtPath(root, { field: dataPoint.name, parameters: {}, subSelections: [], aliasDefinition, togetherWith }, pathSegments);
    }
    additionalParameters.forEach((element: string) => {
      root.subSelections.push({ field: element, parameters: {}, subSelections: [] });
    });
    const filterObject = useFilterObject ? {$filter: 'FilterObject_' + query} : {};
    return selectionToString({ field: 'query', parameters: filterObject, subSelections: [root] });
  };

export const selectDataPointFilterType = (dataPoint: MiningDataPointDefinitionWithPath, primaryUsage: UsagesEnum): FilterType => {
  const filterMode = (dataPoint.usageAttributes?.[primaryUsage]?.[SearchFilterAttributesEnum.filterMode]
    ?? dataPoint.usageAttributes?.[UsagesEnum.general_searchFilter]?.[SearchFilterAttributesEnum.filterMode]) as SearchFilterAttributesEnum;
  switch (filterMode) {
    case 'text':
      return FilterType.freeText;
    case 'number':
      return FilterType.numberValue;
    case 'multiSelect':
      return FilterType.multiSelect;
    case 'treeSelect':
      return FilterType.treeSelect;
  }
};

/**
 * Build the filter Object to be send in a GraphQL query
 * @param columnDataPoints Selected Datapoints
 * @param columnFilterValues Selected values per datapoint
 * @returns a GraphQLfilter object
 */
export const buildFilterObject = (
  columnDataPoints: MiningDataPointDefinitionWithPath[],
  columnFilterValues: string[][] | GraphQLFilterValueObject[][]
): { [key: string]: string }  => {
  if (columnDataPoints.length < 1) {
    return {};
  }
  const stringBuilder: any[] = [];
  const filterObject: { [key: string]: any } = {};
  columnDataPoints.forEach((dataPoint, i) => {
    const propertyName = dataPoint.path.replace(/\./g, '_');
    if (columnFilterValues[i].length > 1) {
      const or: any = { [propertyName]: { in: [] } };
      or[propertyName]['in'].push(...columnFilterValues[i]);
      stringBuilder.push(or);
    } else {
      stringBuilder.push({ [propertyName]: buildFilterValueObject(dataPoint.scalarType, columnFilterValues[i][0]) });
    }
  });
  stringBuilder.forEach((obj) => {
    for (const key in obj) {
      if (obj.hasOwnProperty(key)) {
        filterObject[key] ? filterObject[key] =  {...filterObject[key], ...obj[key]} : filterObject[key] = obj[key];
      }
    }
  });
  return filterObject;
};

/**
 * Method is used to replace template string.
 * @param contextDataPoint datapoint where filter is applied.
 * @param template string that contains RSQL Fragment.
 * @param context object contains filter value.
 * @param graphQlData contains graphQL Data.
 * @returns filter string.
 */
export const replaceTemplateString = (
  contextDataPoint: MiningDataPointDefinitionWithPath,
  template: string,
  context: { [key: string]: any },
  graphQlData: Record<any, any>,
  valueIndex?: number,
  returnPartialReplacement = true
): string => {
  let regExpExec: RegExpExecArray;
  const stringBuilder: string[] = [];
  let lastIndex = 0;
  while ((regExpExec = PLACEHOLDER_PATTERN.exec(template)) != null) {
    const match = regExpExec[0];
    const needsQuote = regExpExec[1] !== undefined;
    const key = regExpExec[2];
    const index = regExpExec.index;
    let value: string[] | string;
    if (key.startsWith('$')) {
      value = context[key.substring(1)];
    } else {
      const contextPath = contextDataPoint.path.split('.');
      const relativePath = key.split('.');
      const finalPath = [...contextPath.slice(0, contextPath.length - 1), ...relativePath].join('.');
      value = getValueAtPath(graphQlData, finalPath);
      if (valueIndex >= 0 && value) {
        value = value[valueIndex];
      }
      if (graphQlData.content?.linkHash && contextDataPoint.displayName.toLowerCase() === 'module name') {
        value = graphQlData.content.linkHash;
      }
    }
    const contextValue = (Array.isArray(value) ? value : [value]).map(element => needsQuote ? `'${element}'` : element).join(',');
    stringBuilder.push(template.substring(lastIndex, index));
    stringBuilder.push(contextValue);
    lastIndex = index + match.length;
  }
  stringBuilder.push(template?.substring(lastIndex, template.length));
  if ( ! returnPartialReplacement) {
    return undefined;
  }
  return stringBuilder.join('');
};

/**
 * Method is used to get the value at Path.
 * @param data data obtained through graph.
 * @param path the datapoint path.
 * @returns filter value.
 */
export const getValueAtPath = (data: Record<any, any>, path: string): any => {
  const pathSegments = path.split('.');
  return getValue(data, pathSegments);
};

const getValue = (data: any, path: string[]): any => {
  if (path.length === 0 || data === 'N/A') {
    return data;
  }
  if (data === undefined) {
    return undefined;
  }
  if (data === null) {
    return null;
  }
  if (Array.isArray(data)) {
    return data.map(obj => getValue(obj, path));
  }
  return getValue(data[path[0]], path.slice(1));
};

/**
 * Build the filter value object containing the operator and the value of the filter
 * @param type scalar taype of the datapoint
 * @param filterValue string or object provided as filter value
 * @returns the filter value object
 */
 const buildFilterValueObject = (
  type: MiningDataPointDefinitionWithPath.ScalarTypeEnum,
  filterValue: string | GraphQLFilterValueObject
): {[key: string]: string} => {
  let filter: GraphQLFilterValueObject;
  if (filterValue === null) {
    filter = {operator: 'is', value: null};
  } else if (typeof filterValue === 'object') {
    filter = filterValue;
  } else {
    filter = {
      operator: type?.toLowerCase() === MiningDataPointDefinitionWithPath.ScalarTypeEnum.Boolean.toLowerCase() ? 'is' : 'eq',
      value: filterValue
    };
  }
  return { [filter.operator]: filter.value };
};

/**
 * Method is used to set the path.
 * @param parent string dataPoints for which query needs to be generated
 * @param selection param to be part of the query parameter.
 * @param path are the associated path corresponds to the dataPoints.
 */
const putSelectionAtPath = (parent: Selection, selection: Selection, path: string[]): string => {
  if (path.length === 1) {
    parent.subSelections.push(selection);
    if (selection.togetherWith) {
      const togetherWithPath = selection.togetherWith.split('.');
      putSelectionAtPath(parent, { field: togetherWithPath[togetherWithPath.length - 1], parameters: {}, subSelections: [] }, togetherWithPath);
    }
  } else {
    for (const subSelection of parent.subSelections) {
      if (subSelection.field === path[0]) {
        putSelectionAtPath(subSelection, selection, path.slice(1));
        return;
      }
    }
    const subSelection: Selection = { field: path[0], parameters: {}, subSelections: [] };
    if (selection.aliasDefinition && selection.field === path[0]
      && selection.aliasDefinition.subSelection === path.slice(1).join('.')) {
      /* alias definition must go here (i.e. this is where the 'renaming' of the GraphQL field must happen) */
      subSelection.aliasDefinition = selection.aliasDefinition;
      selection.aliasDefinition = undefined;
      selection.field = path[path.length - 1];
      /* "togetherWith" is also resolved from here */
      subSelection.togetherWith = selection.togetherWith;
      selection.togetherWith = undefined;
    }
    parent.subSelections.push(subSelection);
    if (subSelection.togetherWith) {
      const togetherWithPath = subSelection.togetherWith.split('.');
      putSelectionAtPath(parent, { field: togetherWithPath[togetherWithPath.length - 1], parameters: {}, subSelections: [] }, togetherWithPath);
    }
    putSelectionAtPath(subSelection, selection, path.slice(1));
  }
};

/**
 * Method is used to convert selections to strings.
 * @param selection string dataPoints for which query needs to be generated
 * @returns the value of the string.
 */
const selectionToString = (selection: Selection): string => {
  let graphql: string;
  if (selection.aliasDefinition) {
    graphql = `${selection.field}: ${selection.aliasDefinition.aliasFor}${aliasParametersToString(selection.aliasDefinition.parameters)}`;
  } else {
    graphql = `${selection.field}${parametersToString(selection.parameters)}`;
  }
  if (selection.subSelections.length > 0) {
    graphql += ` { ${selection.subSelections.map(selectionToString).join(' ')} }`;
  }
  return graphql;
};

/**
 * Method is used to create the parameters into query string.
 * @param parameters param to be part of the query.
 * @returns the value of the string.
 */
const parametersToString = (parameters: { [key: string]: any }): string => {
  if (parameters === undefined || Object.keys(parameters).length === 0) {
    return '';
  }

  const paramStrings: string[] = [];
  for (const [key, value] of Object.entries(parameters)) {
    // we need to avoid seting quote on the parameters related to the filter object
    if (typeof value === 'string' && ! key.includes('$filter') && ! value.includes('$filter') && ! key.includes('sortObject')) {
      /* needs quote */
      paramStrings.push(`${key}: "${value}"`);
    } else if (typeof value === 'object') {
      paramStrings.push(`${key}: [${value}]`);
    } else {
      paramStrings.push(`${key}: ${value}`);
    }
  }
  return '(' + paramStrings.join(', ') + ')';
};

/**
 * Method is used to create the parameters
 * @param parameters param to be part of the query.
 * @returns the value of the string.
 */
const aliasParametersToString = (parameters: string[]): string => (parameters === undefined || parameters.length === 0) ? '' : `(${parameters.join(', ')})`;
