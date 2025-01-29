import { Injectable } from '@angular/core';
import { buildFilterObject } from '@app/core/utils/graphql.util';
import { DEFAULT_NUMBER_OF_ROWS } from '@app/shared/components/mining-table/mining-table-config.interface';
import { NzTableFilterValue, NzTableSortOrder } from 'ng-zorro-antd/table';
import { CustomizableTableQueryParameter } from './customizable-table-query-parameters.interface';
import { BehaviorSubject, Observable } from 'rxjs';
import { MiningDataPointDefinitionWithPath } from '@innowake/mining-api-angular-client';
import { Usages } from '@app/shared/interfaces/datapoints-labels.interface';

/**
 * This service is responsible for managing the selected parameters of the cusomisable table
 */
@Injectable({
  providedIn: 'root'
})
export class CustomizableTableParametersService {
  currentTableParameters: CustomizableTableQueryParameter = {
    page: 1,
    filter: [],
    sort: ''
  };
  reloadTableData: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(false);
  reloadTableDataFlag$: Observable<boolean> = this.reloadTableData.asObservable();
  savedFilter: {[key: string]: CustomizableTableQueryParameter} = {};

  private preFilterModuleIdDataPoints: MiningDataPointDefinitionWithPath[] = [];


  /**
   * To set flag to reload table
   * @value boolean to reload data
   */
  setReloadTableDataValue(value: boolean): void {
    this.reloadTableData.next(value);
  }

  /**
   * To set current parameter for the table
   * @filter set the applied filter
   * @page set the current page number
   * @sort set the sorting style
   * @preFilter set the call chain prefilter
   */
  setParameters(filter: Array<{ key: string, value: NzTableFilterValue}>, page: number, sort: string, preFilter?: string, entity?: string): void {
    if (! page) {
      page = 1;
    }
    this.currentTableParameters = { page, sort, filter, preFilter };
    if (entity) {
      this.savedFilter[entity] = {...this.currentTableParameters};
    }
  }

  /**
   * Save the current table parameters into the saved Search object for the given entity
   * @param entity
   */
  saveParameters(entity: string): void {
    this.savedFilter[entity] = {...this.currentTableParameters};
  }

  /**
   * Reset all the table parameter to empty values
   */
  resetCurrentTableParameters(): void {
    this.currentTableParameters = {
      page: 1,
      filter: [],
      sort: ''
    };
  }

  /**
   * Updates current table filter when column is removed.
   * @param selectedDataPoints selected data points in the table.
   * @returns current filter available in the table.
   */
  updateCurrentTableFilter(selectedDataPoints: MiningDataPointDefinitionWithPath[]): Array<{ key: string, value: NzTableFilterValue}> {
    const columns = Object.values(selectedDataPoints).map((param) => param.path.replace('content.', ''));
    this.currentTableParameters.filter = this.currentTableParameters.filter.filter(filter => columns.includes(filter.key));
    return this.currentTableParameters.filter;
  }

  getTableParametersForUrl(): {[key: string]: any} {
    return { ...this.currentTableParameters, filter: JSON.stringify(this.currentTableParameters.filter) };
  }

  /**
   * Build the filter object out of the array of filter values
   * @param datapoints List of datapoints
   * @param preFilterModuleIds Contains Module Ids from Call Chain Prefilter
   * @returns the filters object to use with GraphQl queries
   */
  handleFilters(datapoints: MiningDataPointDefinitionWithPath[], preFilterModuleIds?: string[]): { [key: string]: string } {
    const filteredDatapoint: MiningDataPointDefinitionWithPath[] = [];
    const filterStringArray: string[][] = [];
    this.currentTableParameters.filter?.forEach((filterElement: { key: string, value: NzTableFilterValue}) => {
      if (filterElement.value.length > 0) {
        filteredDatapoint.push(...(datapoints.filter(x => x.path.replace('content.', '') === filterElement.key) ?? []));
        filterStringArray.push(filterElement.value as string[]);
      }
    });
    if (preFilterModuleIds?.length) {
      if ( ! this.preFilterModuleIdDataPoints.length) {
        this.preFilterModuleIdDataPoints = datapoints.filter(x => x.name === 'id' && x.path === 'content.id');
      }
      filterStringArray.push(preFilterModuleIds);
      filteredDatapoint.push(...this.preFilterModuleIdDataPoints);
    }
    return buildFilterObject(filteredDatapoint, filterStringArray);
  }

  /**
   * Check if the datapoint passed as parameter is part of the currently applied filters
   * @param dataPoint Datapoint to check
   * @returns boolean value representig whether the datapoint is filtered and the list of values used for the filtering
   */
  checkActiveFilter(dataPoint: MiningDataPointDefinitionWithPath): { isFilterActive: boolean, value: any } {
    let isFilterActive = false;
    let value: any;
    this.currentTableParameters?.filter.forEach((element: { key: string, value: string[] }) => {
      if (element.key === dataPoint.path.replace('content.', '') && element.value.length) {
        isFilterActive = true;
        value = element.value;
      }
    });
    return { isFilterActive, value };
  }

  /**
   * Build the graphql parameter object based on the table parameters.
   * @param projectId contains the project id.
   * @param additionalParameters additional GraphQL parameters to be added in plus than the table parameters.
   * @returns request param used for graphQl.
   */
  getGraphQlParam(projectId: number, additionalParameters?: { [key: string]: any }, usage?: string): { [key: string]: any } {
    const preventFilterObject: string[] = [Usages.SCHEDULERIMPORTTABLE];
    // Since filterObject is not supported for SchedulerImportTable, we need to remove it from the requestParam
    const requestParam: { [key: string]: any } = {
      projectId,
      page: this.currentTableParameters.page - 1,
      size: DEFAULT_NUMBER_OF_ROWS,
      ...((preventFilterObject.indexOf(usage) === -1)  && { filterObject: '$filter' })
    };
    if (this.currentTableParameters.sort) {
      requestParam.sortObject = this.currentTableParameters.sort;
    }
    return { ...requestParam, ...additionalParameters };
  }

  /**
   * Restore the previous table parameter for the given entity
   * @param entity the current entity from where the table is instanciated
   */
  restoreFromSavedFilter(entity: string): void {
    if (this.savedFilter[entity]) {
      this.currentTableParameters = {... this.savedFilter[entity]};
    } else {
      this.resetCurrentTableParameters();
    }
  }

  /**
   * Handle the sorting by findinf the corresponding value in the datapoint to build the sort string
   * @param sort sortong object coming from the nz-table
   * @param dataPoints Datapoints coresponding to the currently selected datapoints
   * @returns sort string
   */
  handleSorting(sort: Array<{ key: string; value: NzTableSortOrder }>, dataPoints: MiningDataPointDefinitionWithPath[]): string {
    const sortedColumn = sort.find(sortValue => (sortValue.value));
    if (! sortedColumn) {
      return '';
    }
    const dataPoint = dataPoints.find(x => x.path.replace('content.', '') === sortedColumn.key);
    if (dataPoint) {
      const sortField = dataPoint.path.split('.').join('_');
      const sortOrder = sortedColumn.value === 'ascend' ? 'ASC' : 'DESC';
      return '{' + sortField + ': ' + sortOrder + '}';
    } else {
      return '{}';
    }
  }

  /**
   * Reset all the saved searches related to a module Id (i.e: reset all the tables in the Module Details tabs)
   * @param moduleId Id of the module
   */
  resetSavedSearchForModuleId(moduleId: number): void {
    Object.keys(this.savedFilter).forEach((entity) => {
      if (entity.includes(moduleId.toString())) {
        this.savedFilter[entity] = {
          page: 1,
          filter: [],
          sort: ''
        };
      }
    });
  }

  /**
   * Get the sort object.
   * @returns the sort object.
   */
  getSortObject(): string {
    return this.currentTableParameters?.sort;
  }
}
