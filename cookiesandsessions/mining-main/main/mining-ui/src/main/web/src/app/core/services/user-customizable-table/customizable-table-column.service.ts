import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable, of } from 'rxjs';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { map } from 'rxjs/operators';
import { Column, FilterList, FilterType, ViewMode } from '@app/shared/components/mining-table/mining-table-config.interface';
import { getFieldType } from '@app/shared/components/mining-table/scalartype-fieldtype.mapping';
import { selectDataPointFilterType, replaceTemplateString } from '@app/core/utils/graphql.util';
import UsagesEnum = UsagesModel.UsagesEnum;
import SearchFilterAttributesEnum = UsagesModel.SearchFilterAttributesEnum;
import { TranslateService } from '@ngx-translate/core';
import { LinkType } from '@app/shared/components/mining-table/mining-table-action.interface';
import { LabelMappingService } from '../label-mapping.service';
import { CustomizableTableParametersService } from './customizable-table-parameters.service';
import { Logger } from '@app/core';
import { CustomPropertiesService } from '../custom-properties/custom-properties.service';
import { Router } from '@angular/router';
import {
  AggregationRequestAnnotationFieldName,
  AggregationRequestDataDictionaryFieldName,
  AggregationRequestModuleFieldName,
  AggregationRequestRelationshipFieldName,
  AggregationRequestTaxonomyFieldName,
  AnnotationControllerService,
  DataDictionaryControllerService,
  EntityId,
  JobInformation,
  MiningDataPointDefinitionWithPath,
  ModuleControllerService,
  ModulePojo,
  ReferenceControllerService,
  TaxonomyControllerService,
  FunctionalBlockControllerService,
  UsagesModel,
  TaxonomyPojo
} from '@innowake/mining-api-angular-client';
import { Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { JobManagerService } from '../job-manager/job-manager.service';
import { NzModalService } from 'ng-zorro-antd/modal';
import { BadgeCountUpdateOperation } from '@app/modules/module-details/models/module-complexity-details';
import { ModuleBadgeUpdateService } from '../module-details/module-badge-update.service';
import { taxonomyReduceList } from '@app/core/utils/taxonomy.utils';

const log = new Logger('CustomizableTableColumnService');
const moduleName = 'Module Name';
const pathTaxonomy = 'Path Taxonomy';
const dataDictionaryUsage = 'miningUi.dataDictionaryTable';
const annotationUsage = 'miningUi.annotationsTable';
const miningUsage = 'miningUi.modulesTable';
const reachabilityUsage = 'miningUi.reachabilityTable';
const dependencyUsage = 'miningUi.dependenciesTable';

/**
 * This service is responsible for managing the selected Columns/Datapoints of the cusomisable table
 */
@Injectable({
  providedIn: 'root'
})
export class CustomizableTableColumnService {

  fullTreeOptions: NzTreeNodeOptions[] = [];
  dataPointsList: MiningDataPointDefinitionWithPath[] = [];
  usage: string;
  selectedColumns: string[];
  defaultColumnsToShow: string[] = [];
  retainedDataPointId: MiningDataPointDefinitionWithPath[] = [];
  clearRowSelection: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(false);
  private persistentColumns: string[];
  private selectedDataPoints = new BehaviorSubject<MiningDataPointDefinitionWithPath[]>([]);
  private entity: string;
  private savedSelection: {[key: string]: string[]} = {};

  constructor(
    private moduleControllerService: ModuleControllerService,
    private annotationControllerService: AnnotationControllerService,
    private customPropertiesService: CustomPropertiesService,
    private taxonomyControllerService: TaxonomyControllerService,
    private dataDictionaryControllerService: DataDictionaryControllerService,
    private translateService: TranslateService,
    private labelMappingService: LabelMappingService,
    private parametersService: CustomizableTableParametersService,
    private referenceControllerService: ReferenceControllerService,
    private router: Router,
    private jobManagerService: JobManagerService,
    private modalService: NzModalService,
    private moduleBadgeUpdateService: ModuleBadgeUpdateService,
    private functionalBlockControllerService: FunctionalBlockControllerService
  ) { }

  /**
   * method to set the column id which we will be shown as checked
   * @param columns: final selected column
   */
  setCheckedDataPoint(columns: MiningDataPointDefinitionWithPath[]): void {
    this.retainedDataPointId = [...columns];
  }

  /**
   * method to get the current table column
   */
  getCheckedDataPoint(): MiningDataPointDefinitionWithPath[] {
    return this.retainedDataPointId;
  }

  /**
   * method to reset the checked id
   */
  resetCheckPoints(): void {
    this.retainedDataPointId = [];
  }

  /**
   * method to set the ids and for setting the columns
   */
  setColumnIdsSelection(columnIds: string[]): void {
    this.selectedColumns = columnIds;
    if (this.dataPointsList && this.dataPointsList.length > 0 && columnIds) {
      this.buildTreeData();
    }
  }

  /**
   * Method to update mining table columns on selection of tree node.
   * @param data contains user selected columns.
   * @param ignoreHidden to ignore hidden columns.
   */
  updateSelectedColumns(data: NzTreeNodeOptions[], ignoreHidden: boolean): void {
    let dataPoints = data.map(node => this.dataPointsList.find(datapoint => datapoint.displayName === node.title && datapoint.id === node.id));
    // This is to allow non selected columns to have proper index defined in the reachability table Or ErrorMarker table
    if (ignoreHidden && (this.usage === Usages.REACHABILITYTABLE || this.usage === Usages.MODULERRORTABLE)) {
      dataPoints = dataPoints.filter(dp => dp.usageAttributes[this.usage]?.hiddenByDefault !== 'true');
    }
    this.selectedColumns = data.map(node => node.id);
    if (this.entity) {
      this.savedSelection[this.entity] = [...this.selectedColumns];
    }
    const dataPointPaths = dataPoints.map(dataPoint => dataPoint.path.replace('content.', ''));
    this.parametersService.currentTableParameters.filter = this.parametersService.currentTableParameters.filter
      .filter(filter => dataPointPaths.includes(filter.key));
    this.updateSelectedDataPoints(dataPoints);
  }

  /**
   * Method to update mining table columns with new selected Datapoints (i.e.: when selecting a svaed search).
   * @param data contains user selected columns.
   */
  updateSelectedDataPoints(data: MiningDataPointDefinitionWithPath[]): void {
    this.selectedDataPoints.next(this.sortFixedColumn(data) as MiningDataPointDefinitionWithPath[]);
  }

  /**
   * Sets the column data as tree nodes for customizable table.
   * @param dataPoints contains columns.
   * @param usage to check for modules or annotations.
   * @param persistentColumns contains columns which cannot be edited by user
   */
  setDataPointList(dataPoints: MiningDataPointDefinitionWithPath[], defaultColumns: string[],
    usage: string, persistentColumns: string[]): void {
    this.dataPointsList = dataPoints;
    this.usage = usage;
    this.persistentColumns = persistentColumns;
    this.defaultColumnsToShow = defaultColumns;
    this.buildTreeData();
  }

  /**
   * Get the column data for mining table.
   * @returns The column data.
   */
  getColumnData(): NzTreeNodeOptions[] {
    return this.fullTreeOptions;
  }

  /**
   * Get the Datapoints corresponding to the Columns selected by the user
   * @returns Array of selected DataPoints.
   */
  getSelectedDataPoints(): Observable<MiningDataPointDefinitionWithPath[]> {
    return this.selectedDataPoints;
  }

  /**
   * Updates the table config with the data
   * @param selectedDatapoint contains datapoints.
   * @param projectId current project id
   * @returns Object of key-value pair for columns.
   */
  updateTableConfig(selectedDatapoint: MiningDataPointDefinitionWithPath[], projectId: number): { [key: string]: Column; } {
    const sortedArr = this.sortFixedColumn(selectedDatapoint);
    const columns: { [key: string]: Column; } = {};
    sortedArr.forEach((dataPoint: MiningDataPointDefinitionWithPath) => {
      columns[dataPoint.id] = {
        header: dataPoint.displayName,
        field: dataPoint.path.replace('content.', '') + (dataPoint.aliasFor?.jsonPath ? '.' + dataPoint.aliasFor.jsonPath : ''),
        fieldType: getFieldType(dataPoint.scalarType),
        filterProperties: {
          filterType: selectDataPointFilterType(dataPoint, this.usage as UsagesEnum)
        },
        displayAs: dataPoint.usageAttributes[UsagesEnum.general_viewMode]?.[UsagesModel.ViewModeAttributesEnum.displayAs],
        sortFn: Array.from(dataPoint.usages).findIndex(usage => usage === 'general.sortBy') !== -1 ? true : false
      };
      if (dataPoint.displayName === moduleName) {
        columns[dataPoint.id].hasWarning = (data: any) => this.returnRequiresReview(data);
        columns[dataPoint.id].warningMessage = this.translateService.instant('warningMessage');
      }
      if (this.usage === dependencyUsage && dataPoint.displayName === 'Target Name') {
        columns[dataPoint.id].errorField = (data: any) => this.returnErrorField(data);
      }

      if (this.usage === reachabilityUsage && dataPoint.displayName === pathTaxonomy) {
        // provided tool tip for column when tool tip is different from column display name
        columns[dataPoint.id].headerToolTip = this.translateService.instant('reachability.pathTaxonomyTableHeaderToolTipMessage');
      }
      if (dataPoint.usageAttributes[UsagesEnum.general_viewMode]?.[UsagesModel.ViewModeAttributesEnum.displayAs] === ViewMode.LINK) {
        const viewMode = dataPoint?.usageAttributes[UsagesEnum?.general_viewMode];
        if (viewMode.linkTemplate != null)  {
          const linkTemplate = viewMode.linkTemplate.replace('#', '');
          columns[dataPoint.id].columnAction = {
            type: linkTemplate.includes('null') ? LinkType.STRING : LinkType.HYPERLINK,
            resolveURL: (data: ModulePojo, index: number) => {
              const resolvedLink = linkTemplate.includes('null') ? replaceTemplateString(dataPoint, linkTemplate, { projectId }, { content: data }, index,
                false) : replaceTemplateString(dataPoint, linkTemplate, { projectId }, { content: data }, index, true);
              if (resolvedLink === undefined) {
                return null;
              }
              return resolvedLink;
            }
          };
        };
      }
      if (dataPoint.usageAttributes[UsagesEnum.general_viewMode]?.labelMapping) {
        columns[dataPoint.id].getLabel =
          (value: string) => this.labelMappingService.mapLabel(dataPoint.usageAttributes[UsagesEnum.general_viewMode]?.labelMapping, value);
      }
      const activefilter = this.parametersService.checkActiveFilter(dataPoint);
      columns[dataPoint.id].filterProperties.isFilterActive = activefilter.isFilterActive;
      if (dataPoint.usageAttributes[this.usage]?.[UsagesModel.SearchFilterAttributesEnum.fixedValues]) {
        const multiFixedValues = dataPoint.usageAttributes[this.usage]?.[UsagesModel.SearchFilterAttributesEnum.fixedValues].split(',')
          .map((fixedValue: string) => fixedValue.trim());
        const filtersData = multiFixedValues.map((fixedValue: string) => {
          const text = fixedValue;
          return {
            text,
            value: fixedValue,
            byDefault: false
          };
        });
        const filters = this.setMultiselectFilterValue(activefilter, filtersData);
        columns[dataPoint.id].filterProperties.listOfFilter = filters;
        columns[dataPoint.id].filterProperties.loadingValues = false;
      };
      if (columns[dataPoint.id].filterProperties.filterType === FilterType.multiSelect &&
        ! dataPoint.usageAttributes[this.usage]?.[UsagesModel.SearchFilterAttributesEnum.fixedValues]) {
        columns[dataPoint.id].filterProperties.listOfFilter = [];
        const field = dataPoint.usageAttributes[this.usage]?.[UsagesModel.SearchFilterAttributesEnum.multiSelectValueRetrievalFieldName];
        const retrievalMode = dataPoint.usageAttributes[this.usage]?.[UsagesModel.SearchFilterAttributesEnum.multiSelectValueRetrievalMode];
        const retrievalKey = dataPoint.usageAttributes[this.usage]?.[UsagesModel.SearchFilterAttributesEnum.multiSelectValueRetrievalKeyField];
        const retrievalFilter = dataPoint.usageAttributes[this.usage]?.[UsagesModel.SearchFilterAttributesEnum.multiSelectValueRetrievalFilter];
        columns[dataPoint.id].filterProperties.loadingValues = true;
        this.invokeSelectDistinctService(field, retrievalKey, retrievalFilter, retrievalMode, projectId).subscribe(
          (filters: any[]) => {
            if (
              (dataPoint.usageAttributes['general.searchFilter'] && dataPoint.usageAttributes['general.searchFilter'].showNoneOption === 'true') ||
              (dataPoint.usageAttributes[annotationUsage] && dataPoint.usageAttributes[annotationUsage].showNoneOption === 'true') ||
              (dataPoint.usageAttributes[dataDictionaryUsage] && dataPoint.usageAttributes[dataDictionaryUsage].showNoneOption === 'true')
              || (dataPoint.usageAttributes[miningUsage] && dataPoint.usageAttributes[miningUsage].showNoneOption === 'true')
            ) {
              filters.push({ text: 'None', value: null });
            }
            filters = this.setMultiselectFilterValue(activefilter, filters);
            columns[dataPoint.id].filterProperties.listOfFilter = filters;
            columns[dataPoint.id].filterProperties.loadingValues = false;
          },
          (error) => {
            log.error(error);
            columns[dataPoint.id].filterProperties.loadingValues = false;
          }
        );
      } else if (columns[dataPoint.id].filterProperties.filterType === FilterType.treeSelect) {
        columns[dataPoint.id].filterProperties.listOfFilter = [];
        columns[dataPoint.id].filterProperties.loadingValues = true;
        this.invokeTreeDataService(projectId).subscribe((taxonomyTreeData: TaxonomyPojo[]) => {
          const filters = this.setMultiselectFilterValue(activefilter, this.buildFilterListForTreeSelect(taxonomyTreeData));
          const defaultKeys = filters.filter(filter => filter.byDefault).map(filter => filter.key);
          columns[dataPoint.id].filterProperties.listOfFilter = [...filters];
          columns[dataPoint.id].filterProperties.defaultKeys = defaultKeys;
          columns[dataPoint.id].filterProperties.taxonomyData = this.buildTreeDataForTaxonomy(activefilter, taxonomyTreeData);
          columns[dataPoint.id].filterProperties.loadingValues = false;
        },
        (error) => {
          log.error(error);
          columns[dataPoint.id].filterProperties.loadingValues = false;
        });
      } else {
        columns[dataPoint.id].filterProperties.filterValue = activefilter.value;
      }
    });
    const regexExp = /{|}|\s/ig;
    const sortedColumn = this.parametersService.currentTableParameters.sort;
    if (sortedColumn && sortedColumn !== '') {
      const sortSplit = sortedColumn.replace(regexExp, '').split(':');
      // As alias is removed for RB table, we need to replace alias with the actual path.
      const dataPoint = selectedDatapoint.find(dataPoint => dataPoint.path === sortSplit[0].split('_').join('.'));
      if (dataPoint) {
        columns[dataPoint?.id].sortOrder = sortSplit[1] === 'ASC' ? 'ascend' : 'descend';
      } else {
        this.parametersService.currentTableParameters.sort = '';
      }
    }
    return columns;
  }

  /**
   * Reset the table to defaul values defined by the datapoints
   */
  resetTable(): void {
    this.selectedColumns = [];
    this.buildTreeData();
  }

  /**
   * Reset the table to default values and default datapoints
   */
  resetTableColumnAndDataPoints(): void {
    this.selectedColumns = [];
    this.dataPointsList = [];
    this.entity = '';
  }

  /**
   * Restore the selected columns for the  given entity
   * @param entity currentity from here the table is instanciated
   */
  restoreFromSavedSelection(entity: string): void {
    this.entity = entity;
    this.dataPointsList = [];
    if (this.savedSelection[entity]) {
      this.selectedColumns = [...this.savedSelection[entity]];
    } else {
      this.selectedColumns = [];
    }
  }

  /**
   * checks selected columns of the datapoints
   * @param dataPoint contains datapoints.
   * @param usage check for modules or annotation
   * @returns boolean
   */
  checkSelectedColumns(dataPoint: MiningDataPointDefinitionWithPath, usage: string, persistent?: boolean): boolean {
    if (this.selectedColumns && this.selectedColumns.length && this.defaultColumnsToShow.length === 0) {
      return this.selectedColumns.findIndex((x: string) => x === dataPoint.id) > -1 || persistent;
    } else if (this.defaultColumnsToShow && this.defaultColumnsToShow.length > 0) {
      return this.defaultColumnsToShow.includes(dataPoint?.displayName) ? true : false;
    } else if (dataPoint.usageAttributes?.[usage]?.defaultColumnIndex) {
      return true;
    } else {
      return false;
    }
  }

  /**
   * Sets default sort object string.
   * @param usage usage of the table.
   * @param dataPoint contains datapoints.
   * @returns default sort by string.
   */
  getDefaultSortBy(usage: string, dataPoints: MiningDataPointDefinitionWithPath[], defaultColumn?: string): string {
    if (usage === dataDictionaryUsage) {
      return '{' + this.getKeyFromDataPoint(dataPoints, 'Field Name') + ': ' + 'ASC}';
    } else if(usage === reachabilityUsage) {
      defaultColumn = defaultColumn ? defaultColumn : 'Upper Bound';
      return '{' + this.getKeyFromDataPoint(dataPoints, defaultColumn) + ': ' + 'ASC}';
    } else if(usage === annotationUsage && this.router.url.includes('details/annotations')) {
      return '{content_annotationOffset: ASC}';
    }
    const moduleNameFilterString = this.getKeyFromDataPoint(dataPoints, moduleName);
    if (moduleNameFilterString) {
      return '{' + moduleNameFilterString + ': ' + 'ASC}';
    } else {
      return '';
    }
  }

  /**
   * Bulk deletes annotation or DD entries based on entity type.
   * @param ids Array of entry IDs.
   * @param entityType string that mentions the entity type- either DD/ annotation.
   * @param component component from where the method is called- to refresh the table.
   */
  bulkDelete(ids: number[], entityType: string, component: {projectId: EntityId, refreshCoreTable: boolean}): void {
    component.refreshCoreTable = false;
    this.modalService.warning({
      nzTitle: this.translateService.instant('bulkDeletionModal.title', { entry: entityType }),
      nzContent: this.translateService.instant('bulkDeletionModal.description', { entry: entityType }),
      nzOkText: this.translateService.instant('btnLabel.delete'),
      nzOkDanger: true,
      nzIconType: 'warning',
      nzOnOk: () => this.deleteEntries(ids, entityType, component),
      nzCancelText: this.translateService.instant('annotationReporting.no'),
    });
  }

  /**
   * Bulk generation of Functional block based on passes data dictionary ids
   * @param ids Array of entry IDs.
   * @param entityType string that mentions the entity type- either DD/ annotation.
   * @param component component from where the method is called- to refresh the table.
   */
  bulkFbGeneration(ids: EntityId[], entityType: string, component: {projectId: EntityId, refreshCoreTable: boolean}): void {
    component.refreshCoreTable = false;
    this.generateFB(ids, component);
  }

  /**
   * method to pass the flag to clear row selection.
   * @param clearSelection boolean flag to clear row selection.
   */
  updateRowSelectionClearanceFlag(clearSelection: boolean): void {
    this.clearRowSelection.next(clearSelection);
  }

  /**
   * method to get row selection clearance flag.
   * @returns Subject string
   */
  getRowSelectionClearanceFlag(): BehaviorSubject<boolean> {
    return this.clearRowSelection;
  }

  private deleteEntries(entityIds: number[], entityType: string, component: {projectId: EntityId, refreshCoreTable: boolean}) {
    this.annotationControllerService.bulkDeletion(component.projectId, entityType, entityIds).subscribe((response)=> {
      this.jobManagerService.register({ jobId: response + '', foreground: true, cancellable: true }).status$.subscribe((result: string) => {
        if (result === JobInformation.StatusEnum.SUCCESS) {
          const operation = entityType === 'Annotation' ? BadgeCountUpdateOperation.ANNOTATION_DELETED : BadgeCountUpdateOperation.DATA_DICTIONARY_DELETED;
          this.moduleBadgeUpdateService.updateAnnotationDataDictionary({operation, count: Array.from(entityIds).length});
          component.refreshCoreTable = true;
          this.updateRowSelectionClearanceFlag(true);
        }
      });
    }, (error) => {
      log.error(error);
    });
  }

  private generateFB(entityIds: EntityId[], component: {projectId: EntityId, refreshCoreTable: boolean}) {
    this.functionalBlockControllerService.generateFunctionalBlocksUsingDataLineage(component.projectId, entityIds).subscribe((response)=> {
      this.jobManagerService.register({ jobId: response + '', foreground: true, cancellable: true }).status$.subscribe((result: string) => {
        if (result === JobInformation.StatusEnum.SUCCESS) {
          log.info('Functional Block Generated Successfully');
        }
      });
    } , (error) => {
      log.error(error);
    } );
  }

  private setMultiselectFilterValue(activeFilter: { isFilterActive: boolean, value: any[] }, filterList: any[]): any[] {
    filterList = filterList.filter((filter: any) => filter.value === false || filter.value || (filter.text === 'None' && filter.value === null));
    filterList.sort((a: any, b: any) => String(a.text).localeCompare(String(b.text)));
    if (activeFilter.isFilterActive) {
      activeFilter.value.forEach(selectedFilter => {
        filterList?.forEach((filterValue: any) => {
          if (filterValue.value === selectedFilter) {
            filterValue.byDefault = true;
          }
        });
      });
    }
    return filterList;
  }

  private invokeTreeDataService (projectId: number): Observable<TaxonomyPojo[]> {
    return this.taxonomyControllerService.findAllTaxonomies(projectId);
  }

  private buildTreeDataForTaxonomy(activeFilter: { isFilterActive: boolean, value: any[] }, taxonomies: TaxonomyPojo[]): NzTreeNodeOptions[] {
    const taxonomyIdPerChild: number[] = [];
    const groupTaxonomyIds: number[] = [];
    const modifiedArray: NzTreeNodeOptions[] = [];
    const groupedTaxonomy = taxonomies.reduce((newList: TaxonomyPojo, currentValue: TaxonomyPojo) => taxonomyReduceList(newList, currentValue), {});
    Object.keys(groupedTaxonomy).forEach((category: string) => {
      const tree: NzTreeNodeOptions = {
        title: category,
        key: category,
        expanded: true,
        children: [],
        isLeaf: false,
        selectable: false
      };
      Object.keys(groupedTaxonomy[category] as object).forEach((type: string) => {
        const tree_children: NzTreeNodeOptions = {
          title: type,
          key: `${category}:${type}`,
          expanded: false,
          children: [],
          isLeaf: false,
          selectable: false
        };
        groupedTaxonomy[category][type].forEach((childNode: NzTreeNodeOptions) => {
          const isActive = activeFilter.isFilterActive && activeFilter.value.includes(childNode.id);
          tree_children.children.push({
            title: childNode.name,
            key: `${childNode['name']}_${childNode['id']}`,
            id: childNode.id,
            isLeaf: true,
            checked: isActive
          });
        });
        tree_children.children.sort((a, b) => a.title.toLowerCase().localeCompare(b.title.toLowerCase()));
        tree.children.push(tree_children);
      });
      groupTaxonomyIds.length = 0;
      tree.children.forEach((treeChildrenItem: NzTreeNodeOptions) => {
        taxonomyIdPerChild.length = 0;
        treeChildrenItem.children.forEach((children: NzTreeNodeOptions) => {
          const taxonomyIds = +(children.key.substring(children.key.lastIndexOf('_') + 1));
          taxonomyIdPerChild.push(taxonomyIds);
          groupTaxonomyIds.push(taxonomyIds);
        });
        treeChildrenItem.key = `${treeChildrenItem.key}_${taxonomyIdPerChild}`;
      });
      tree.key = `${tree.key}_${groupTaxonomyIds}`;
      tree.children.sort((a, b) => a.title.toLowerCase().localeCompare(b.title.toLowerCase()));
      modifiedArray.push(tree);
    });
    modifiedArray.sort((a, b) => a.title.localeCompare(b.title));
    return modifiedArray;
  }

  private buildFilterListForTreeSelect(taxonomies: TaxonomyPojo[]): FilterList[] {
    return taxonomies.map((taxonomy: TaxonomyPojo) => ({
      text: taxonomy['name'],
      value: taxonomy['id'],
      key: `${taxonomy['name']}_${taxonomy['id']}`,
      byDefault: false
    })
    );
  }

  private invokeSelectDistinctService(
    field: string,
    keyField: string,
    filterObject: string,
    retrievalMode: string,
    projectId: number
  ): Observable<any> {
    const requestString = {
      filterObject: filterObject? JSON.parse(filterObject): {},
      groupBy: keyField ? new Set([keyField, field]) : new Set([field]),
      fields: {
        ID: 'COUNT'
      }
    };
    let observable: Observable<any> = new Observable();
    switch (retrievalMode) {
      case SearchFilterAttributesEnum.moduleControllerDistinctFieldValues:
        observable = this.moduleControllerService.getAggregatedValues2(projectId, requestString as AggregationRequestModuleFieldName);
        break;
      case SearchFilterAttributesEnum.annotationControllerAggregatedValues:
        observable = this.annotationControllerService.getAggregatedValues4(projectId, requestString as AggregationRequestAnnotationFieldName);
        break;
      case SearchFilterAttributesEnum.taxonomyControllerAggregatedValues:
        observable = this.taxonomyControllerService.getAggregatedValues(projectId, requestString as AggregationRequestTaxonomyFieldName);
        break;
      case SearchFilterAttributesEnum.dataDictionaryControllerAggregatedValues:
        observable = this.dataDictionaryControllerService.getAggregatedValues3(projectId, requestString as AggregationRequestDataDictionaryFieldName);
        break;
      case SearchFilterAttributesEnum.customPropertyTag:
        observable = this.customPropertiesService.getAutoCompletionValues(projectId).pipe(
          map((autocompletionMap: { [key: string]: string[]}) => autocompletionMap[field])
        );
        break;
      case SearchFilterAttributesEnum.referenceControllerAggregatedValues:
        observable = this.referenceControllerService.getAggregatedValues1(projectId, requestString as AggregationRequestRelationshipFieldName);
        break;
      default:
        observable = of([]);
        break;
    }
    return observable.pipe(
      map((distinctValues) =>
        distinctValues.map((value: any | string) => {
          const text = typeof value === 'string' ? value : value['group'][field];
          return {
            text,
            value: keyField ? value['group'][keyField] : text,
            byDefault: false
          };
        })
      )
    );
  }

  private buildTreeData(): void {
    const groupedDataPoints = this.dataPointsList.reduce((newList: MiningDataPointDefinitionWithPath, currentValue: MiningDataPointDefinitionWithPath) => {
      if (currentValue.usageAttributes[this.usage]) {
        newList[currentValue.usageAttributes[this.usage].category] = newList[currentValue.usageAttributes[this.usage].category] || [];
        newList[currentValue.usageAttributes[this.usage].category].push(currentValue);
      }
      return newList;
    }, {});
    const fullTreeOption: NzTreeNodeOptions[] = [];
    Object.keys(groupedDataPoints).forEach((element: string) => {
      const tree: NzTreeNodeOptions = {
        title: element,
        key: element,
        expanded: true,
        children: [],
        isLeaf: false
      };
      groupedDataPoints[element].forEach((childNode: MiningDataPointDefinitionWithPath) => {
        const persistent = this.persistentColumns.indexOf(childNode['id']) !== -1;
        if (this.defaultColumnsToShow.length === 0) {
          const defaultChecked = this.checkSelectedColumns(childNode, this.usage, persistent);
          tree.children.push({
            title: childNode.displayName,
            name: childNode.name,
            key: childNode.name,
            fieldType: childNode.scalarType,
            checked: defaultChecked,
            disableCheckbox: persistent,
            id: childNode.id,
            isLeaf: true,
            columnIndex: childNode.usageAttributes[this.usage]?.defaultColumnIndex || null
          });
        } else {
          tree.children.push({
            title: childNode.displayName,
            name: childNode.name,
            key: childNode.name,
            fieldType: childNode.scalarType,
            checked: this.defaultColumnsToShow.includes(childNode.displayName) ? true : false,
            disableCheckbox: persistent,
            id: childNode.id,
            isLeaf: true
          });
        }
      });
      fullTreeOption.push(tree);
    });
    const miningColumnData: NzTreeNodeOptions[] = [];
    fullTreeOption.sort((a, b) => a.title.localeCompare(b.title));
    fullTreeOption.forEach((element: NzTreeNodeOptions, index: number) => {
      element.key = index.toString();
      element.children.sort((a, b) => a.columnIndex - b.columnIndex ||
        a.title.localeCompare(b.title));
      element.children.forEach((childNode: NzTreeNodeOptions, childIndex: number) => {
        childNode.key = element.key.concat(childIndex.toString());
        if (childNode.checked) {
          miningColumnData.push(childNode);
        }
      });
    });
    this.fullTreeOptions = fullTreeOption;
    this.updateSelectedColumns(miningColumnData, true);
  }

  private returnRequiresReview(value: any): boolean {
    return value.requiresReview;
  }

  private returnErrorField(value: any): string {
    return value.target.identification;
  }

  private sortFixedColumn(selectedDatapoint: MiningDataPointDefinitionWithPath[]) {
    selectedDatapoint.sort((a, b) => +a.usageAttributes[this.usage]?.defaultColumnIndex - +b.usageAttributes[this.usage]?.defaultColumnIndex);
    if(this.usage === Usages.REACHABILITYTABLE || this.usage === Usages.MODULERRORTABLE) {
      this.setCheckedDataPoint(selectedDatapoint);
    }

    return selectedDatapoint.reduce((acc, element) => {
      if (element.displayName === moduleName && element.usageAttributes[this.usage]?.defaultColumnIndex === '0') {
        return [element, ...acc];
      }
      return [...acc, element];
    }, []);
  }

  private getKeyFromDataPoint(dataPoints: MiningDataPointDefinitionWithPath[], displayName: string): string {
    const moduleNameDatapoint = dataPoints.find(dataPoint => dataPoint.displayName === displayName);
    if (moduleNameDatapoint) {
      return moduleNameDatapoint.path.split('.').join('_');
    } else {
      return '';
    }
  }
}
