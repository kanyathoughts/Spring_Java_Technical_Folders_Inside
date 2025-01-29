import { Component, Input, OnInit, Output, ViewChild, EventEmitter, SimpleChanges, OnChanges, TemplateRef, AfterViewInit, OnDestroy} from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalService } from 'ng-zorro-antd/modal';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { Observable, Subscription } from 'rxjs';
import { debounceTime } from 'rxjs/operators';
import {
  Column, DEFAULT_NUMBER_OF_ROWS, FieldTypeEnum, MiningDropDown, MiningTableConfig, MiningTableRow,
  OptionItem
} from '../mining-table/mining-table-config.interface';
import { graphQlQuery, buildFilterObject } from '@app/core/utils/graphql.util';
import { dateFormatter } from '@app/core/utils/date-formatter.utils';
import { Logger } from '@app/core';
import { MiningTableComponent } from '../mining-table/mining-table.component';
import { NzTableQueryParams, NzTableSize } from 'ng-zorro-antd/table';
import { MiningTableOptionSelected } from '../mining-table/mining-table-option-selected.interface';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TableFilter } from '@app/shared/interfaces/type-based-filter.interface';
import { getValueAtPath } from '@app/core/utils/graphql.util';
import { AllowedTableActions, MiningTableAction, StatesType } from '../mining-table/mining-table-action.interface';
import { DataPointControllerService, EntityId, MiningDataPointDefinitionWithPath, ModulePojo } from '@innowake/mining-api-angular-client';
import { ActivatedRoute } from '@angular/router';
import { ModuleBadgeUpdateService } from '@app/core/services/module-details/module-badge-update.service';
import { BadgeCountUpdateOperation } from '@app/modules/module-details/models/module-complexity-details';

const log = new Logger('DataDictionaryComponent');
const idPath = 'content.id';
const errorTablePath = 'content.severity';
const moduleIdPath = 'content.module.id';

@Component({
  selector: 'mn-customizable-table-core-component',
  templateUrl: './customizable-table-core.component.html'
})
export class CustomizableTableCoreComponent implements OnInit, OnChanges, AfterViewInit, OnDestroy {
  @ViewChild(MiningTableComponent) miningTable: MiningTableComponent;
  @ViewChild('messageTemplate') messageTemplate: TemplateRef<any>;
  @Input() usage: string; // Usage of the table
  @Input() graphQlType: string; // Type of graphQl query to be generated.
  @Input() projectId: number;
  @Input() moduleIdForPreFiltering: number[];
  @Input() pageType: string;
  @Input() dataPointsToHide: string[] = [];
  @Input() rowActions: OptionItem[][]; // Actions of the table
  @Input() internalDataPoints: MiningDataPointDefinitionWithPath[] = [];
  @Input() refreshCoreTable = false; // To initialize the core table
  @Input() toolTipTemplate: TemplateRef<any>;
  @Input() dataChangeEvent: Observable<MiningTableAction>;
  @Input() allowSelectDeselectRows: boolean;
  @Input() disableSortHandling: boolean;
  @Input() defaultDataPointsToShow: string[] = [];
  @Input() entity = '';
  @Input() additionalGraphQLParams: { [key: string]: any } = {};
  @Input() additionalGraphQLPreFilter: { [key: string]: any } = {};
  @Input() preFilterDetails: string[];
  @Input() additionalTableConfig: MiningTableConfig; // Additional properties to be added to the table config
  @Input() enableOrderedBusinessRuleCsv: boolean;
  @Input() isSavedSearchSelected: boolean;
  @Input() filterResult: (args: MiningTableRow) => void;
  @Input() mergeRowsByProperty?: string;
  @Input() switchColumns?: { id: string, column: string };
  @Input() columnForRowSelection?: string;
  @Input() preSelectedModules: number[] = [];
  @Input() selectAllOnLoad = false;
  @Output() optionSelected: EventEmitter<any> = new EventEmitter();
  @Output() selectedRows: EventEmitter<number[]> = new EventEmitter();
  @Output() performAction: EventEmitter<string> = new EventEmitter();
  @Output() collectRowData: EventEmitter<any> = new EventEmitter();

  tableConfig: MiningTableConfig = {
    columnMap: {},
    paginator: true,
    rows: DEFAULT_NUMBER_OF_ROWS,
    showTotalCount: true,
    serverSidePagination: true,
    actionsWidth: '100px',
    isCustomizable: true,
    isExportVisible: true,
    projectId: 0
  };
  totalRecords = 0;
  value: MiningTableRow[] = [];
  tableSize: NzTableSize = 'small';
  columns: Column[];
  childValues: MiningTableRow = {};
  clientProjectSubscription: Subscription;
  expandedRows: EntityId[] = [];
  setOfSelectedId = new Set<number>();
  currentEditingColumnId: string;
  tooltip: any;
  sortDirections: string[] = ['descend', 'ascend', null];
  datChangeEventsSubscriptions = new Subscription();
  selectedState: string = StatesType.NONE;
  deselectCurrent = false;
  getValueAtPath = getValueAtPath;
  dataPointSubscription = new Subscription();
  multiRowSelectionOptions: MiningDropDown[] = [];
  preFiltersObject: { [key: string]: string | string[]; };
  moduleId: number;
  initialDataPoints: MiningDataPointDefinitionWithPath[] = [];
  switchedDataPoints: MiningDataPointDefinitionWithPath[] = [];
  clearFlagSubscription: Subscription;
  selectedReachabilityRow: string[] = [];
  errorMesgId: string;

  public modifiedDataPoints: MiningDataPointDefinitionWithPath[] = [];
  private eventsSubscriptions: Subscription;

  constructor(
    public messageService: NzMessageService,
    public relationshipService: ClientProjectRelationshipService,
    public userCustomizableTableColumnService: CustomizableTableColumnService,
    public graphQlControllerService: GraphQlControllerService,
    public dataPointControllerService: DataPointControllerService,
    public translateService: TranslateService,
    public parametersService: CustomizableTableParametersService,
    public modalService: NzModalService,
    public notification: NzNotificationService,
    private moduleBadgeUpdateService: ModuleBadgeUpdateService,
    private route: ActivatedRoute,
    private numberFormatter: NumberFormatter
  ) { }

  ngOnInit(): void {
    this.relationshipService.getClientProjectObservable().subscribe((currentClient: ClientProjectRelationship) => {
      this.projectId = this.projectId ? this.projectId : currentClient?.getProjectId();
      this.tableConfig.projectId = this.projectId;
    });
    // Additional parameters must be provided to the CSV export as well
    if (this.additionalGraphQLParams) {
      if (this.usage === Usages.REACHABILITYTABLE) {
        this.tableConfig.exportParameters = this.updateExportParameters();
      } else {
        this.tableConfig.exportParameters = this.additionalGraphQLParams;
      }
    }
    /*
      Note: Fetching the module ID from the route, this will only work when we have the component being
      used inside any route which is using the Module Resolver to resolve data.
    */
    this.route.data.subscribe((data: { module: ModulePojo }) => {
      this.moduleId = data.module?.id;
    });
    this.initialize(this.pageType);
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.refreshCoreTable && changes.refreshCoreTable.currentValue) {
      this.serverSidePaginationCall(this.preFilterDetails);
      this.clearFlagSubscription?.unsubscribe();
      this.clearFlagSubscription = this.userCustomizableTableColumnService.getRowSelectionClearanceFlag().subscribe((clearSelection: boolean) => {
        if (clearSelection) {
          this.clearSelectedIds();
          this.userCustomizableTableColumnService.updateRowSelectionClearanceFlag(false);
        }
      });
      this.refreshCheckedStatus();
      this.serverSidePaginationCall(this.preFilterDetails);
    }
    if (changes.totalRecords) {
      this.multiRowSelectionOptionsDropdown();
    }
    if (changes.rowActions && changes.rowActions.currentValue) {
      this.tableConfig.actions = this.rowActions && this.rowActions.length > 0 ? this.rowActions : [];
    }
    if (changes.additionalTableConfig && changes.additionalTableConfig.currentValue) {
      this.tableConfig = {...this.tableConfig, ... this.additionalTableConfig};
    }
  }

  ngAfterViewInit(): void {
    if (this.dataChangeEvent) {
      this.datChangeEventsSubscriptions.add(this.dataChangeEvent.subscribe((data: MiningTableAction) => this.changeTableData(data)));
    }
  }

  /**
   * Initialize the Customizable table page by fetching the needed Data (data points, Saved Searches, ...)
   * @param typeName type to use to fetch the data points and for GraphQl queries.
   * @returns the client project observable so the parent component can subscribe to it.
   */
  initialize(typeName: string): void {
    if (this.entity) {
      this.parametersService.restoreFromSavedFilter(this.entity);
      this.userCustomizableTableColumnService.restoreFromSavedSelection(this.entity);
    }
    this.tableConfig.exportType = this.graphQlType;
    this.tableConfig.isCustomizable = true;
    this.dataPointControllerService.getDataPointsForType(this.projectId, typeName, [this.usage])
      ?.subscribe((dataPoints: MiningDataPointDefinitionWithPath[]) => {
      if (this.switchColumns) {
        this.initialDataPoints = this.switchedDataPoints;
        this.parametersService.currentTableParameters.sort =
          this.userCustomizableTableColumnService.getDefaultSortBy(this.usage, dataPoints, this.switchColumns.column);
      } else {
        this.initialDataPoints = dataPoints;
      }
      const finalDataPoints = this.initialDataPoints.filter((datPoint) => ! this.dataPointsToHide.includes(datPoint?.displayName));
      if ( ! this.isSavedSearchSelected && ! this.parametersService.currentTableParameters.sort) {
        this.parametersService.currentTableParameters.sort =
          this.userCustomizableTableColumnService.getDefaultSortBy(this.usage, dataPoints);
      }
      this.userCustomizableTableColumnService.setDataPointList(finalDataPoints, this.defaultDataPointsToShow, this.usage,
        this.getPersistentColumnsBasedOnusage(this.usage));
      this.fetchTableDataAsPerDataPoints();
    });
  }

  /**
   * Subscribe to the data point selection and fetch table data on update
   */
  fetchTableDataAsPerDataPoints(): void {
    /** Get user selected column data */
    this.dataPointSubscription = this.userCustomizableTableColumnService.getSelectedDataPoints().pipe(debounceTime(100))
      .subscribe((selectedDataPoint: MiningDataPointDefinitionWithPath[]) => {
        if (selectedDataPoint.length > 0) {
          this.modifiedDataPoints = [...selectedDataPoint];
          this.internalDataPoints.forEach((dataPoint) => {
            if (this.modifiedDataPoints.findIndex(dp => dp.path === dataPoint.path) === -1) {
              this.modifiedDataPoints.push(dataPoint);
            }
          });
          if(this.tableConfig?.bulkSelectionDataPoints && this.tableConfig.bulkSelectionDataPoints.length) {
            // setting the collection of ids to null on column change
            this.clearSelectedIds();
            this.selectedState = StatesType.NONE;
            this.selectedReachabilityRow.length = 0;
            const currentColumn: MiningDataPointDefinitionWithPath[] = this.userCustomizableTableColumnService.getCheckedDataPoint();
            const currentColumnName = currentColumn.map((item: MiningDataPointDefinitionWithPath) =>  item.name);
            this.tableConfig.bulkSelectionDataPoints = [];
            this.tableConfig.bulkSelectPoints.forEach((item: any) => {
              if(currentColumnName.indexOf(item.name as string)  > -1) {
                this.tableConfig.bulkSelectionDataPoints.push({ name: 'id', path: item.path});
              }
            });
            this.tableConfig.bulkSelectionDataPoints.forEach((dataPoint)=> {
              if (this.modifiedDataPoints.findIndex(dp => dp.path === dataPoint.path) === -1) {
                this.modifiedDataPoints.push(dataPoint);
              }
            });
          }
          // This is because incase of intermediate module, export parameters needs to be updated with the status of button.
          if (this.usage === Usages.REACHABILITYTABLE) {
            this.tableConfig.exportParameters = this.updateExportParameters();
          }

          /*
            Below logic will select all rows for the first time only when the table is loaded as per the selectOnLoad flag.
            Otherwise it checks if there are any preSelectedModules and selects them only.
          */
          if (this.selectAllOnLoad) {
            this.selectedState = StatesType.NONE;
            this.selectDeselectAll();
            this.selectAllOnLoad = false;
          } else if (this.preSelectedModules.length) {
            this.updateCheckedSet(this.preSelectedModules , true);
          }

          this.serverSidePaginationCall(this.preFilterDetails);
          this.performAction.emit('fetchTable');
          this.updateTableConfig(selectedDataPoint);
        }
      });
  }

  /**
   * cancel the ongoing network  call
   */
  cancelDataLoading(): void {
    this.tableConfig.loading = false;
    this.eventsSubscriptions?.unsubscribe();
    this.datChangeEventsSubscriptions?.unsubscribe();
  }

  /**
   * Fetch the data for the table
   * @param moduleIdsFromCallChain contains module ids from preFilter in Call Chain
   */
  serverSidePaginationCall(moduleIdsFromCallChain?: string[]): void {
    const preventFilterObject: string[] = [Usages.SCHEDULERIMPORTTABLE];
    if (this.modifiedDataPoints.length < 1) {
      return null;
    }
    const additionalProperties: string[] = ['totalElements', 'size'];
    const getGraphQlParam = this.parametersService.getGraphQlParam(this.projectId, this.additionalGraphQLParams, this.usage);
    const baseFilterObj = this.parametersService.handleFilters(this.modifiedDataPoints, moduleIdsFromCallChain);
    let finalFilterObject: { [key: string]: string | string[]; } = baseFilterObj;
    let idDataPoint: MiningDataPointDefinitionWithPath;
    if (this.usage === Usages.ANNOTATIONTABLE) {
      idDataPoint = this.modifiedDataPoints.find((dataPoint) => dataPoint.path === moduleIdPath);
    } else if (this.usage === Usages.DATADICTIONARYTABLE) {
      idDataPoint = this.modifiedDataPoints.find((dataPoint) => dataPoint.path === moduleIdPath);
    } else if (this.usage === Usages.MODULETABLE) {
      idDataPoint = this.modifiedDataPoints.find((dataPoint) => dataPoint.path === idPath);
    } else if (this.usage === Usages.DNATABLE) {
      idDataPoint = this.modifiedDataPoints.find((dataPoint) => dataPoint.path === moduleIdPath);
    } else if (this.usage === Usages.SCHEDULERIMPORTTABLE) {
      idDataPoint = this.modifiedDataPoints.find((dataPoint) => dataPoint.path === idPath);
    } else if (this.usage === Usages.MODULERRORTABLE) {
      idDataPoint = this.modifiedDataPoints.find((dataPoint) => dataPoint.path === errorTablePath);
    }
    if (this.moduleIdForPreFiltering && this.moduleIdForPreFiltering.length) {
      const idSelectionFilterObj = buildFilterObject([idDataPoint], [this.moduleIdForPreFiltering.map(id => id.toString())]);
      finalFilterObject = (baseFilterObj && Object.keys(baseFilterObj).length) ?
      {...baseFilterObj, ...idSelectionFilterObj} : idSelectionFilterObj;
    }
    const content = [
      { name: 'id', path: 'content.id' }
    ];
    let filter: { [key: string]: any } = { };
    filter = this.parametersService.handleFilters(this.modifiedDataPoints, moduleIdsFromCallChain);
    if ( moduleIdsFromCallChain && moduleIdsFromCallChain.length ) {
      filter = buildFilterObject([content[0]], [moduleIdsFromCallChain]);
    }
    const requestQuery: { [key: string]: any } = {
      'query': graphQlQuery(
        this.graphQlType,
        getGraphQlParam,
        this.modifiedDataPoints,
        additionalProperties,
        preventFilterObject.indexOf(this.usage) > -1  ? false : true),
      'variables': {
        filter
      }
    };
    const preFilterGraphQl: { [key: string]: any } = this.additionalGraphQLPreFilter;
    if (preFilterGraphQl && Object.keys(preFilterGraphQl).length) {
      finalFilterObject = { ...finalFilterObject, ...preFilterGraphQl };
    }

    if (finalFilterObject && Object.keys(finalFilterObject).length) {
        requestQuery.variables = { filter: finalFilterObject };
    }
    this.preFiltersObject = finalFilterObject;
    this.tableConfig.loading = true;
    this.eventsSubscriptions?.unsubscribe();
    this.eventsSubscriptions = this.graphQlControllerService.graphQl(requestQuery, this.projectId).subscribe((response: { [key: string]: any }) => {
      if (response.data && response.data[this.graphQlType] && response.data[this.graphQlType].content) {
        this.value = response.data[this.graphQlType].content;
        this.value = typeof this.filterResult === 'function' ? this.value.filter(DNAValue => this.filterResult(DNAValue)) : this.value;
        const lengthOfFilteredValues = response.data[this.graphQlType].content.length - this.value.length;
        this.totalRecords = typeof this.filterResult === 'function' ? response.data[this.graphQlType].totalElements - lengthOfFilteredValues
          : response.data[this.graphQlType].totalElements;
        this.value.forEach((element: any, index: number) => {
          const currentPage = this.parametersService.currentTableParameters.page;
          if(this.usage === Usages.REACHABILITYTABLE && this.tableConfig.bulkSelectionDataPoints.length) {
            element['id'] = currentPage * (DEFAULT_NUMBER_OF_ROWS) + '_' + index + '_' + element[this.columnForRowSelection]?.id;
          }
          if (element['id'] === null || element['id'] === undefined) {
            element['id'] = index;
          }
          element['metricsDate'] = element.metricsDate ? dateFormatter(element.metricsDate as Date) : this.translateService.instant('notAvailable');
          // Set a fake id if not present to avoid row duplication issue
          if (element['id'] === null || element['id'] === undefined) {
            element['id'] = index;
          }
        });
        this.closeErrorMessage();
        if (this.setOfSelectedId.size > 0) {
          this.checkCurrentPageRecord();
          this.refreshCheckedStatus();
        }
        this.tableConfig.loading = false;
      } else {
        this.closeErrorMessage();
        this.errorMesgId = this.messageService.create('error', this.messageTemplate, { nzDuration: 0}).messageId;
        this.tableConfig.scroll = {};
        this.tableConfig.loading = false;
      }
      this.setChildrenValues();
      this.performAction.emit('makeServerSideCall');
    }, error => {
      log.error(error);
      this.tableConfig.loading = false;
    });
    this.performAction.emit('setRoute');
  }

  /**
   * Close the error message.
   */
  closeErrorMessage(): void {
    if (this.errorMesgId) {
      this.messageService?.remove(this.errorMesgId);
    }
  }

  /**
   * Gets the rowSpan value for a given table row and column.
   * @param item table row
   * @param column table column
   * @returns value to span/merge the given row.
   */
  getRowSpanValue(item: MiningTableRow, column: Column): number {
    return item?.rowspan ? item.rowspan[column.field.split('.')[0]] ? item.rowspan[column.field.split('.')[0]] : 1 : 1;
  }

  /**
   * Callback method to capture the page change event in pagination
   * @param page current selected page in pagination
   */
  onPageChange(page: number): void {
    if (page !== this.parametersService.currentTableParameters.page) {
      this.parametersService.currentTableParameters.page = page;
      this.parametersService.saveParameters(this.entity);
      this.serverSidePaginationCall(this.preFilterDetails);
    }
  }

  /**
   * emits the selected row data to parent component
   * @param event get the rowData
   */
  handleTableActions?(rowDataToBePerformedAction: MiningTableOptionSelected): void {
    this.optionSelected.emit(rowDataToBePerformedAction);
  };

  /**
   * Method return the Alignment on the basis of String or Number.
   *
   * @param column field column
   * @param item table row
   * @returns string value right or left .
   */
  // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
  getAlign(column?: Column, item?: MiningTableRow): 'left' | 'right' | 'center' {
    return column?.fieldType === FieldTypeEnum.NUMBER && ! Array.isArray(item?.[column?.field?.split('.')[0]]) ? 'right' : 'left';
  }

  /**
   * Method to calculate the indent for a row that cannot be expanded
   * in case the row does not have any children under it.
   *
   * @param item The data being displayed in the row
   */
  calculateIndent(item: MiningTableRow): number {
    let indent = (item.level ? item.level : 0) * 24;
    if (item.nonExpandableRowIndent && ! item.children) {
      indent++;
    }
    return indent;
  }

  /**
   * Collapses the row in the tree-table view.
   *
   * @param parentArray Array of the parent row.
   * @param data data of the selected row.
   * @param $event boolean to determine to collapse or not.
   */
  collapse(parentArray: MiningTableRow[], data: MiningTableRow, $event: boolean): void {
    this.tableConfig.disableAllActions = false;
    this.maintainExpandedRows($event, data.id);
    /* calls the callback method for fetching child data*/
    if ($event && ! data.children.length) {
      const info = { optionValue: 'children', data };
      this.handleTableActions(info);
    }

    if (! $event && data.children) {
      data.children.forEach((child: MiningTableRow) => {
        const target = parentArray.find((parent) => parent.id === child.id && parent.expand);
        if (target) {
          target.expand = false;
          this.collapse(parentArray, target, false);
        }
      });
    }
  }

  /**
   * method to handle both client and server side filtering
   * @param  selectedFilterDetails value of selected filter
   * @param  columnField column name on which filter is applying
   */
  handleSelectedFilter(selectedFilterDetails: TableFilter[], columnField: string): void {
    const newFilters = this.parametersService.currentTableParameters.filter.filter(item => item.key !== columnField);
    if (selectedFilterDetails.length > 0) {
      newFilters.push({
        key: columnField,
        value: selectedFilterDetails.map(detail => {
          if (detail.operator) {
            return { value: detail.value, operator: detail.operator };
          } else {
            return detail.value;
          }
        })
      });
      this.parametersService.currentTableParameters.page = 1;
    }
    if (JSON.stringify(this.parametersService.currentTableParameters.filter) !== JSON.stringify(newFilters)) {
      this.parametersService.currentTableParameters.filter = newFilters;
      this.parametersService.saveParameters(this.entity);
      if (this.usage === Usages.REACHABILITYTABLE) {
        this.selectedState = StatesType.ALL;
        this.selectDeselectAll();
      } else if (this.setOfSelectedId.size > 0 && newFilters.length > 0) {
        this.filterSelectionWithCurrentFilter();
      }
      this.serverSidePaginationCall(this.preFilterDetails);
    }
  }

  /**
   * Handle the nzQueryParams event of the nz-table
   * Important: filters should be handled by handleSelectedFilter only to avoid multiple calls
   * @param event event emitted by the nzQueryParams param of the nz-table
   */
  handleSortChange(event: NzTableQueryParams): void {
    if(event.sort.length && ! this.disableSortHandling) {
      const newSort = this.parametersService.handleSorting(event.sort, this.modifiedDataPoints);
      if (newSort !== this.parametersService.currentTableParameters.sort) {
        this.parametersService.currentTableParameters.sort = newSort;
        this.parametersService.saveParameters(this.entity);
        this.serverSidePaginationCall(this.preFilterDetails);
      }
    }
  }

  /**
   * Check the requiresReview flag and return boolean to display warning image.
   *
   * @param data contain the details of the Content.
   * @param column contain the Column property of the table config.
   * @returns the boolean to display the warning message.
   */
  resolveFieldWarning(data: MiningTableRow, column: Column): boolean {
    return (column?.hasWarning) ? column?.hasWarning(data) : false;
  }

  /**
   * Check the errorField flag and return boolean to display warning/error image.
   * @param data contain the details of the Content.
   * @param column contain the Column property of the table config.
   * @returns the boolean to display the warning message.
   */
  errorWarningIcon(data: MiningTableRow, column: Column): boolean {
    return column?.errorField ? (column?.errorField(data) === 'IDENTIFIED' && data.errorCount === 0) ? false : true : false;
  }

  /**
   * method for adding the warning/error icon to the table in case of module error table
   * @param column:Column
   * @returns boolean if to show icon or not
   */
  errorWarningIconForMarker(column: Column): boolean {
    return (this.usage === Usages.MODULERRORTABLE && column.field === 'severity');
  }

  /**
   * method to show the code for source code column
   * @param column:Column
   */
  isCodeIconVisible(column: Column, data: MiningTableRow): boolean {
    return (this.usage === Usages.MODULERRORTABLE && column.field === 'errorText' && data.errorText !== 'No Data');
  }

  /**
   * Check if the dependency identified with errors or not.
   * @param data contain the details of the Content.
   * @returns the boolean to display the warning message.
   */
  isIdentified(data: MiningTableRow): boolean {
    return data.target.identification === 'IDENTIFIED' && data.errorCount > 0;
  }

  /**
   * Determines the icon and its corresponding CSS class based on the severity of a given MiningTableRow object.
   * @param data - The MiningTableRow object whose severity is to be checked.
   * @returns An object containing 'nzType' and 'ngClass'.
   * If the severity is 'ERROR', 'nzType' is 'close-circle' and 'ngClass' is 'ant-helper__error-icon'.
   * Otherwise, 'nzType' is 'exclamation-circle' and 'ngClass' is 'ant-helper__warning-icon'.
   */
  getSeverityIcon(data: MiningTableRow): { nzType: string, ngClass: string } {
    return data.severity === 'ERROR'
      ? { nzType: 'close-circle', ngClass: 'ant-helper__error-icon' }
      : { nzType: 'exclamation-circle', ngClass: 'ant-helper__warning-icon' };
  }

  /**
   * Check if specific action is available or not in the specified array
   * @param actions data at the row
   * @param action action at the row
   * @returns boolean flag to disable
   */
  isActionAvailable(actions: string[], action: string): boolean {
    if (actions) {
      return actions.includes(action);
    }
    return false;
  }

  /**
   * Emits the event for the callback function defined in the Parent components.
   *
   * @param optionValue value of the option selected.
   * @param data table row data.
   */
  callOptionCallback(optionValue: string, data: MiningTableRow): void {
    this.handleTableActions({ optionValue, data });
  }

  /**
   * Resolves the fields that are functions or '.' separators.
   *
   * @param data data send by the callbacks.
   * @param field field send by the callbacks.
   */
  resolveFieldData(data: MiningTableRow, field: string): any | null {
    if (! data || ! field) {
      return null;
    }
    if (field.indexOf('.') === -1) {
      return data[field] || data[field] === 0 ? data[field] : null;
    }
    const fields: string[] = field.split('.');
    let value = data;
    for (let i = 0, len = fields.length; i < len; ++i) {
      if (value === null) {
        return null;
      }
      value = value[fields[i]];
    }
    return value;
  }

  /**
   * Flattens the parent array with children for rendering purpose.
   *
   * @param root Actual array list.
   */
  convertTreeToList(root: MiningTableRow): any[] {
    const stack: MiningTableRow[] = [];
    const array: MiningTableRow[] = [];
    const hashMap = {};
    /**
     * We are adding the key for the identifying the visited node as both parent
     * and child can have same id if they are different entities. Eg: Client/project.
     */
    const expand = root?.expand || this.expandedRows.includes(root.id);
    const edgeRefernces =  this.translateService.instant('edgeReferences');
    const toolTip = edgeRefernces[root.relationship?.toLowerCase()]?.toolTip;
    stack.push({ ...root, level: 0, expand, key: 'parent-' + root.id, toolTip });

    while (stack.length !== 0) {
      const node = stack.pop();
      this.visitNode(node, hashMap, array);
      if (node.children) {
        for (let i = node.children.length - 1; i >= 0; i--) {
          stack.push({
            ...node.children[i],
            level: node.level + 1,
            expand: node.children[i]?.expand || this.expandedRows.includes(node.children[i].id),
            parent: node,
            key: 'child-' + node.children[i].id,
          });
        }
      }
    }
    return array;
  }

  /**
   * used to toggle b/w select all and deselect all
   * from the drop down
   */
  selectDeselectAll(): void {
    if (this.selectedState === StatesType.ALL) {
      this.selectedState = StatesType.NONE;
      this.clearSelectedIds();
      this.selectedReachabilityRow = [];
    } else {
      this.selectedState = StatesType.ALL;
      if (this.tableConfig.bulkSelectionDataPoints?.length) {
        this.selectedReachabilityRow = [];
        this.tableConfig.selectAllCallback();
      } else {
        this.filterSelectionWithCurrentFilter();
      }
    }
    this.multiRowSelectionOptionsDropdown();
    this.getSelectedRowsById();
  }

  /**
   * on select of single check box
   * @param id selected id
   * @param checked boolean of selected id
   */
  onItemChecked(id: number, checked: boolean, row?: MiningTableRow): void {
    let ids: number[] = [];
    if( ! row) {
      ids = [id];
    } else {
      ids.push(id);
      for(const item in row) {
        if(row[item] && row[item]?.id) {
          ids.push(row[item]?.id as number);
        } else if(row[item] && Array.isArray(row[item])) {
          row[item].forEach((item: any) => {
            if(item.id) {
              ids.push(item.id as number);
            }
          });
        }
      }
    }
    this.updateCheckedSet(ids, checked);
    this.refreshCheckedStatus();
    this.getSelectedRowsById();
  }

  /**
   * get selected rows by Ids
   */
  getSelectedRowsById(): void {
    const rowIds: number[] = [];
    for (const id of this.setOfSelectedId) {
      rowIds.push(id);
    }
    this.selectedRows.emit(rowIds);
  }


  /**
   * to set the show tick box and indeterminate in a checkbox
   */
  refreshCheckedStatus(): void {
    if (this.totalRecords) {
      if (this.setOfSelectedId.size === this.totalRecords || this.selectedReachabilityRow.length === this.totalRecords) {
        this.selectedState = StatesType.ALL;
        this.multiRowSelectionOptionsDropdown();
      } else if (this.setOfSelectedId.size > 0 && (this.setOfSelectedId.size < this.totalRecords || this.selectedReachabilityRow.length < this.totalRecords)) {
        this.selectedState = StatesType.SOME;
        this.multiRowSelectionOptionsDropdown();
      } else {
        if( ! this.setOfSelectedId.size) {
          this.selectedState = StatesType.NONE;
        }
      }
    } else {
      this.selectedState = StatesType.NONE;
    }
    this.getSelectedRowsById();
  }

  /**
   * on select or change in checkbox selection
   * @param id selected id
   * @param checked boolean of selected id
   */
  updateCheckedSet(ids: number[], checked: boolean): void {
    if (checked) {
      if (ids.length && this.usage === Usages.REACHABILITYTABLE) {
        this.selectedReachabilityRow.push(ids[0].toString());
      }
      ids.forEach((id) => {
        this.setOfSelectedId.add(id);
      });
    } else {
      ids.forEach((id) => {
        this.setOfSelectedId.delete(id);
      });
      if (this.usage === Usages.REACHABILITYTABLE && this.selectedReachabilityRow.length) {
        this.selectedReachabilityRow = this.selectedReachabilityRow.filter((item: string) => item !== ids[0].toString());
      }
    }
    this.checkCurrentPageRecord();
  }

  /**
   * To check in the current page, selection of records and
   * turn the flag as true or false based on the number of selected records
   */
  checkCurrentPageRecord(): void {
    this.deselectCurrent = ! this.value?.some((element: MiningTableRow) => !this.setOfSelectedId.has(element.id));
  }

  /**
   * get all the ids present and get all filtered ids
   * based on selection of filters
   * @param selectAll when selectAll from dropdown is selected
   */
  filterSelectionWithCurrentFilter(): void {
    const baseFilterObj = this.parametersService.handleFilters(this.modifiedDataPoints, this.preFilterDetails);
    let finalFilterObject: { [key: string]: string | string[]; } = baseFilterObj;
    if (this.selectedState !== StatesType.ALL) {
      const idDataPoint = this.modifiedDataPoints.find((datapoint) => datapoint.path === idPath);
      const idSelectionFilterObj = buildFilterObject([idDataPoint], [[...this.setOfSelectedId].map(id => id.toString())]);
      finalFilterObject = (baseFilterObj && Object.keys(baseFilterObj).length) ?
      {...baseFilterObj, ...idSelectionFilterObj} : idSelectionFilterObj;
    }
    let dataPointOfId: MiningDataPointDefinitionWithPath;
    if (this.usage === Usages.ANNOTATIONTABLE) {
      dataPointOfId = this.modifiedDataPoints.find((dataPoint) => dataPoint.path === moduleIdPath);
    } else if (this.usage === Usages.DATADICTIONARYTABLE) {
      dataPointOfId = this.modifiedDataPoints.find((dataPoint) => dataPoint.path === moduleIdPath);
    } else if (this.usage === Usages.MODULETABLE) {
      dataPointOfId = this.modifiedDataPoints.find((dataPoint) => dataPoint.path === idPath);;
    }
    if (this.moduleIdForPreFiltering) {
      const idSelectionFilterObj = buildFilterObject([dataPointOfId], [this.moduleIdForPreFiltering.map(id => id.toString())]);
      finalFilterObject = (baseFilterObj && Object.keys(baseFilterObj).length) ?
      {...baseFilterObj, ...idSelectionFilterObj} : idSelectionFilterObj;
    }
    const requestQuery: { [key: string]: any } = {
      'query': graphQlQuery(
        this.tableConfig.exportType,
        {
          projectId: this.tableConfig.projectId,
          filterObject: '$filter'
        },
        [{ name: 'id', path: idPath }],
        [],
        true
      )
    };
    const preFilterGraphQl: { [key: string]: any } = this.additionalGraphQLPreFilter;
    if (preFilterGraphQl && Object.keys(preFilterGraphQl).length) {
      finalFilterObject = { ...finalFilterObject, ...preFilterGraphQl };
    }

    this.preFiltersObject = finalFilterObject;
    if (finalFilterObject && Object.keys(finalFilterObject).length) {
      requestQuery.variables = { filter: finalFilterObject };
    }
    const filteredIds = new Set<number>();
    this.graphQlControllerService.graphQl(requestQuery, this.projectId).subscribe((response: { [key: string]: any }) => {

      if (this.usage === Usages.ANNOTATIONTABLE && Object.keys(preFilterGraphQl).length) {
        if (response.data.annotations.content.length < this.setOfSelectedId?.size) {
          this.messageService.info(this.translateService.instant('miningTable.countReducedMessage') as string);
        }
        response.data.annotations.content.forEach((element: any) => {
          filteredIds.add(element.id as number);
        });
      } else {
        if (response.data[this.graphQlType].content.length < this.setOfSelectedId?.size) {
          this.messageService.info(this.translateService.instant('miningTable.countReducedMessage') as string);
        }
        response.data[this.graphQlType].content.forEach((element: any) => {
          filteredIds.add(element.id as number);
        });
      }
      this.setOfSelectedId = new Set([...filteredIds]);
      this.checkCurrentPageRecord();
      this.refreshCheckedStatus();
    });
  }


  /**
   * handles the event of selected BusinessVariablesReferenced.
   *@param selectedOptionValue value of the option selected.
   * @param selectedRowData table row data.
   */
  handleShowBusinessVariablesReferenced(selectedOptionValue: string, selectedRowData: MiningTableRow): void {
    this.optionSelected.emit({ optionValue: selectedOptionValue, data: selectedRowData?.data });
  }

  /**
   * Checked if the Module is preselected or not.
   * @param id id of the Module
   * @param totalRecords total number of records
   *
   * @returns boolean value to check if the module is preselected or not.
   */
  isPreSelected(id: number, totalRecords?: number): boolean {
    if (this.preSelectedModules.length) {
      if (totalRecords) {
        return this.preSelectedModules.length === totalRecords;
      }
      return this.preSelectedModules.includes(id);
    }
    return false;
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.dataPointSubscription?.unsubscribe();
    this.datChangeEventsSubscriptions?.unsubscribe();
    this.eventsSubscriptions?.unsubscribe();
    this.clearFlagSubscription?.unsubscribe();
    this.additionalGraphQLParams = {};
    this.pageType = '';
    this.graphQlType = '';
    this.entity = '';
    this.messageService?.remove();
  }

  private updateExportParameters(): { [key: string]: any } {
    // this is needed because of string formatting not really only for the reachability analyses
    // The issue is that for the the Graphql query strings should be stringilied but not for the export
    const additionalParams = { ...this.additionalGraphQLParams };
    Object.keys(this.additionalGraphQLParams).forEach((key: string) => {
      if (key === 'functionalBlocks') {
        const uuIds: string[] = additionalParams[key].map((uid: string) => JSON.parse(uid));
        additionalParams[key] = [].concat(...uuIds);
      }
    });
    return additionalParams;
  }

  /**
   * Change the table data as per the data received from the callback functions.
   * @param resp data send by the callbacks.
   */
  private changeTableData(resp: MiningTableAction) {
    let reloadData = true;
    let index: number;
    switch (resp.action) {
      case AllowedTableActions.DELETE:
        this.value = this.value.filter((item) => item.id !== resp.id);
        break;

      case AllowedTableActions.TOOLTIP:
        this.tooltip = resp.data;
        reloadData = false;
        break;

      case AllowedTableActions.UPDATE:
        index = this.value.findIndex((item) => item.id === resp.id);
        this.value[index] = resp.data;
        break;

      case AllowedTableActions.EXPAND_CHILD:
        this.setChildrenValues();
        const parentIndex = this.childValues[resp.id].findIndex((item: MiningTableRow) => ! item.hasOwnProperty('parent'));

        /* Need to have the timeout as we have to wait for the rendering changes to complete before expanding row */
        setTimeout(() => {
          this.childValues[resp.id][parentIndex].expand = true;
        }, 0);
        break;

      case AllowedTableActions.ADD_CHILD:
        const ids = resp.data.id.split('__');
        if (ids.length) {
          let element: MiningTableRow = null;
          ids.forEach((item: any) => {
            const records = element ? element.children : this.value;
            element = records.find((rec: MiningTableRow) => rec.name === item);
          });
          element.expand = true;
          element.children = resp.data.children;
          if (! this.expandedRows.includes(element.id)) {
            this.expandedRows.push(element.id);
          }
        } else {
          reloadData = false;
        }
        break;

      case AllowedTableActions.TOGGLE_ACTIONS:
        this.tableConfig.disableAllActions = resp.data;
        this.currentEditingColumnId = null;
        reloadData = false;
        break;

      case AllowedTableActions.RESTRICT_EDITING:
        this.tableConfig.disableAllActions = true;
        this.currentEditingColumnId = resp.data.id;
        reloadData = false;
    }

    if (reloadData) {
      this.setChildrenValues();
    }
  }

  /**
   * Sets the child values.
   */
  private setChildrenValues() {
    this.childValues = [];
    if (this.value) {
      this.value.forEach((item: MiningTableRow) => {
        this.childValues[item.id] = this.convertTreeToList(item);
      });
    }
    this.multiRowSelectionOptionsDropdown();
  }

  private updateTableConfig(selectedDataPoint: MiningDataPointDefinitionWithPath[]): void {
    const columns = this.userCustomizableTableColumnService.updateTableConfig(selectedDataPoint, this.projectId);
    this.tableConfig.columnMap = columns;
    this.moduleBadgeUpdateService.getModuleToBeReviewed().subscribe(response => {
      if (response === BadgeCountUpdateOperation.DELETE_ALL_WARNINGS) {
        this.tableConfig.columnMap['Module Name'].hasWarning = () => false;
      }
    });
    this.columns = Object.values(this.tableConfig.columnMap);

    const occupiedSpace = this.tableConfig.occupiedSpace ? this.tableConfig.occupiedSpace : 235;
    this.tableConfig.scroll = { y: 'calc(100vh - ' + occupiedSpace + 'px)' };
    /** Apply horizontal scroll if columns exceeds default length. */
    if (selectedDataPoint.length * 150 > 600) {
      this.tableConfig.scroll = { x: `${selectedDataPoint.length * 150}px`, y: 'calc(100vh - ' + occupiedSpace + 'px)' };
    } else {
      this.tableConfig.scroll.x = '';
    }
    /* shallow-clone the tableConfig object to update the table. */
  }

  private maintainExpandedRows(status: boolean, rowId: EntityId) {
    if (status) {
      this.expandedRows.push(rowId);
    } else {
      const index = this.expandedRows.indexOf(rowId);
      if (index > -1) {
        this.expandedRows.splice(index, 1);
      }
    }
  }

  private visitNode(node: any, hashMap: { [key: string]: boolean }, array: any[]): void {
    if (! hashMap[node.key]) {
      hashMap[node.key] = true;
      array.push(node);
    }
  }

  private multiRowSelectionOptionsDropdown() {
    const translationKey = this.selectedState === StatesType.ALL ? 'miningTable.deselectAll' : 'miningTable.selectAll';
    this.multiRowSelectionOptions = [
      {
        text: this.translateService.instant(translationKey, { totalRecords: this.numberFormatter.transform(this.totalRecords) }),
        onSelect: () => this.selectDeselectAll()
      },
      {
        text: this.showCurrentDropDownText(),
        onSelect: () => this.selectDeselectCurrentIds()
      }
    ];
  }

  /**
   * shows the text and count from the dropdown for current page
   * @returns as string for dropdown
   */
  private showCurrentDropDownText(): string {
    if (this.deselectCurrent && this.setOfSelectedId.size) {
      this.deselectCurrent = true;
      return this.translateService.instant('miningTable.deselectCurrent', { length: Object.keys(this.childValues).length });
    } else {
      this.deselectCurrent = false;
      return this.translateService.instant('miningTable.currentPage', { length: Object.keys(this.childValues).length });
    }
  }

  /**
   * used to toggle b/w select current page  and deselect current page
   * from the drop down
   */
  private selectDeselectCurrentIds(): void {
    this.deselectCurrent = ! this.deselectCurrent;
    this.onAllChecked(this.deselectCurrent);
    this.multiRowSelectionOptionsDropdown();
  }

  private onAllChecked(value: boolean): void {
    Object.keys(this.childValues).forEach((item: string) => {
      // this is needed because incase of RB table view, id will be in the format '30_0_201' and parseInt will return 30.
      if (this.usage === Usages.REACHABILITYTABLE) {
        this.updateCheckedSet([item] as unknown as number[], value);
      } else {
        this.updateCheckedSet([parseInt(item, 10)], value);
      }
    });
    this.refreshCheckedStatus();
  }

  private getPersistentColumnsBasedOnusage(usage: string) {
    const displayId = 'Module.name';
    switch(usage) {
      case Usages.DEPENDENCYTABLE:
        return ['DependencyInformation.targetName'];
      case Usages.REACHABILITYTABLE:
        return [this.switchColumns ? this.switchColumns.id : 'ReachabilityData.upperBoundModuleName'];
      case Usages.MODULETABLE:
        return [displayId, 'Module.type', 'Module.technology'];
      case Usages.DATADICTIONARYTABLE:
        return ['DataDictionaryEntry.name'];
      default:
        return [displayId];
    }
  }

  private clearSelectedIds() {
    this.setOfSelectedId.clear();
    if (this.preSelectedModules && this.moduleIdForPreFiltering?.length) {
      this.setOfSelectedId = new Set(this.preSelectedModules);
    }
  }
}
