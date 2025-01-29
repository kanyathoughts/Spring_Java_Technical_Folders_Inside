import {
  Component,
  Input,
  Output,
  EventEmitter,
  AfterViewInit,
  OnDestroy,
  TemplateRef,
  SimpleChanges,
  OnChanges
} from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { NzTableQueryParams, NzTableSortOrder, NzTableSize, NzTableFilterValue } from 'ng-zorro-antd/table';
import { Observable, Subscription } from 'rxjs';
import { AllowedTableActions, MiningTableAction, StatesType } from './mining-table-action.interface';
import { Column, FieldTypeEnum, FilterType, MiningDropDown, MiningTableConfig, MiningTableRow } from './mining-table-config.interface';
import { getValueAtPath } from '@app/core/utils/graphql.util';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TableFilter } from '@app/shared/interfaces/type-based-filter.interface';
import { Operators } from '../type-based-filter/type-based-filter.component';
import { EntityId } from '@innowake/mining-api-angular-client';

const CANCEL_BUTTON_DELAY = 3000;

/**
 * This component provides the table component for the Application which is based on the Ant-design
 * table and should be used to show the table everywhere in the App.
 */
@Component({
  selector: 'mn-table',
  templateUrl: './mining-table.component.html'
})
export class MiningTableComponent implements AfterViewInit, OnDestroy, OnChanges {

  @Input() totalRecords: number;
  @Input() pageIndex: number;
  @Input() dataChangeEvent: Observable<MiningTableAction>;
  @Input() toolTipTemplate: TemplateRef<any>;
  @Input() loading: boolean;
  @Input() tableStructureChange: boolean;
  @Input() isBulkInsertEnabled = false;
  @Input() tableSize: NzTableSize = 'small';
  @Input() afterFilteredId: Set<number>;

  @Output() selectAllModule: EventEmitter<boolean> = new EventEmitter();
  @Output() queryParamChanged: EventEmitter<any> = new EventEmitter();
  @Output() loadingCancel: EventEmitter<any> = new EventEmitter();
  @Output() filteredIds: EventEmitter<Set<number>> = new EventEmitter();
  @Output() private optionSelected: EventEmitter<any> = new EventEmitter();
  @Output() private optionHover: EventEmitter<any> = new EventEmitter();

  columns: Column[];
  tableConfig: MiningTableConfig;
  childValues: MiningTableRow = {};
  queryParams: NzTableQueryParams = {
    pageIndex: 0,
    pageSize: 50,
    sort: [],
    filter: [],
  };
  values: MiningTableRow[];
  valuesToDisplay: MiningTableRow[];
  eventsSubscriptions = new Subscription();
  tooltip: any;
  deselectCurrent = false;
  sortDirections: string[] = ['descend', 'ascend', null];
  error = '';
  filterDisabled = true;
  cancelLoading: boolean;
  getValueAtPath = getValueAtPath;
  currentEditingColumnId: string;
  expandedRows: EntityId[] = [];
  selectedState: string = StatesType.NONE;
  setOfCheckedId = new Set<number>();
  bulkSelectionOptions: MiningDropDown[] = [];
  isTreeTable = false;

  constructor(
    private translateService: TranslateService,
    private numberFormatter: NumberFormatter
  ) { }

  @Input() set config(config: MiningTableConfig) {
    this.tableConfig = config;
    this.columns = Object.values(this.tableConfig.columnMap);
    if (!this.tableConfig.serverSidePagination) {
      this.setColumnSorting();
    }
    setTimeout(() => {
      this.cancelLoading = this.tableConfig.contentLoading;
    }, CANCEL_BUTTON_DELAY);
    if (this.tableConfig.contentLoading) {
      this.valuesToDisplay = [];
    }
  }

  @Input() set value(values: MiningTableRow[]) {
    this.values = values ? values : [];
    this.checkCurrentPageRecord();
    this.valuesToDisplay = [ ...values];
    this.setChildrenValues();
    if (!this.tableConfig.serverSidePagination) {
      this.setColumnFilterList();
    }
    this.orderColumnFilterList();
  }

  parserNumber = (value: string): string => value.replace(/\D/g, '');

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.afterFilteredId?.currentValue) {
      this.setOfCheckedId = this.afterFilteredId;
      this.checkCurrentPageRecord();
      this.refreshCheckedStatus();
    }
    if (changes.totalRecords) {
      this.bulkSelectionOptionsDropdown();
    }
    if (this.tableStructureChange) {
      this.setColumnFilterList();
    }
    this.orderColumnFilterList();
    if (changes.loading) {
      setTimeout(() => {
        this.cancelLoading = this.loading;
      }, CANCEL_BUTTON_DELAY);
    }
  }

  ngAfterViewInit(): void {
    if (this.dataChangeEvent) {
      this.eventsSubscriptions.add(this.dataChangeEvent.subscribe((data: MiningTableAction) => this.changeTableData(data)));
    }
  }

  /**
   * To check in the current page, selection of records and
   * turn the flag as true or false based on the number of selected records
   */
  checkCurrentPageRecord(): void {
    this.deselectCurrent = ! this.values?.some((element: MiningTableRow) => ! this.setOfCheckedId.has(element.id));
  }

  /**
   * on select or change in checkbox selection
   * @param id selected id
   * @param checked boolean of selected id
   */
  updateCheckedSet(id: number, checked: boolean): void {
    if (checked) {
      this.setOfCheckedId.add(id);
    } else {
      this.setOfCheckedId.delete(id);
    }
    this.checkCurrentPageRecord();
  }

  /**
   * on select of single check box
   * @param id selected id
   * @param checked boolean of selected id
   */
  onItemChecked(id: number, checked: boolean): void {
    this.updateCheckedSet(id, checked);
    this.refreshCheckedStatus();
  }

  /**
   * select the passed number of ids to be selected
   * i.e for select all or current page
   * @param value boolean value
   */
  onAllChecked(value: boolean): void {
    Object.keys(this.childValues).forEach((item: string) => {
      this.updateCheckedSet(parseInt(item, 10), value);
    });
    this.refreshCheckedStatus();
  }

  /**
   * used to toggle b/w select current page  and deselect current page
   * from the drop down
   */
  selectDeselectCurrentIds(): void {
    this.deselectCurrent = ! this.deselectCurrent;
    this.onAllChecked(this.deselectCurrent);
    this.bulkSelectionOptionsDropdown();
  }

  /**
   * used to toggle b/w select all and deselect all
   * from the drop down
   */
  selectDeselectAll(): void {
    if (this.selectedState === StatesType.ALL) {
      this.selectedState = StatesType.NONE;
      this.setOfCheckedId.clear();
    } else {
      this.selectedState = StatesType.ALL;
      this.selectAllModule.emit(true);
    }
    this.bulkSelectionOptionsDropdown();
  }

  /**
   * to set the show tick box and indeterminate in a checkbox
   */
  refreshCheckedStatus(): void {
    if (this.totalRecords) {
      if (this.setOfCheckedId.size === this.totalRecords) {
        this.selectedState = StatesType.ALL;
        this.bulkSelectionOptionsDropdown();
      } else if (this.setOfCheckedId.size > 0 && (this.setOfCheckedId.size < this.totalRecords)) {
        this.selectedState = StatesType.SOME;
        this.bulkSelectionOptionsDropdown();
      } else {
        this.selectedState = StatesType.NONE;
      }
    } else {
      this.selectedState = StatesType.NONE;
    }
  }

  /**
   * Performs the validation for checking if input is a number and within range
   * For Client side validation only
   *
   * @param column column filtered for.
   */
  validateFilterInput(column: Column): void {
    if (! this.tableConfig.isCustomizable) {
      this.error = 'success';
      const filterText = column.filterProperties.filterValue.value;
      this.filterDisabled = true;
      if (filterText !== '' && filterText !== null) {
        if (
          !(
            this.getMinValueForFilter(column.field) <= column.filterProperties.filterValue[0].value &&
            column.filterProperties.filterValue[0].value <= this.getMaxValueForFilter(column.field)
          )
        ) {
          this.error = 'error';
          this.filterDisabled = true;
          column.filterProperties.filterValue = 0;
        } else {
          this.filterDisabled = false;
        }
      }
    }
  }

  /**
   * Calculates the min value for the numeric filter for a given field.
   *
   * @param columnField field value calculated for.
   * @returns min value for the numeric filter option.
   */
  getMinValueForFilter(columnField: string): number {
    return Math.min(...this.values.map((rowValue: MiningTableRow) => this.resolveFieldData(rowValue, columnField)) as number[]);
  }

  /**
   * Calculates the max value for the numeric filter for a given field.
   *
   * @param columnField field value calculated for.
   * @returns max value for the numeric filter option.
   */
  getMaxValueForFilter(columnField: string): number {
    return Math.max(...this.values.map((rowValue: MiningTableRow) => this.resolveFieldData(rowValue, columnField)) as number[]);
  }

  /**
   * Emits the event for the callback function defined in the Parent components.
   *
   * @param optionValue value of the option selected.
   * @param data table row data.
   */
  callOptionCallback(optionValue: string, data: MiningTableRow): void {
    this.optionSelected.emit({ optionValue, data });
  }

  /**
   * handles the event of selected BusinessVariablesReferenced.
   *@param optionValue value of the option selected.
   * @param data table row data.
   */
  handleShowBusinessVariablesReferenced(selectedOptionValue: string, selectedRowData: MiningTableRow): void {
    this.optionSelected.emit({ optionValue: selectedOptionValue, data: selectedRowData?.data });
  }

  /**
   * Emit the event to get the tooltip data.
   *
   * @param toolTipField field for the tooltip.
   * @param data row data.
   */
  getToolTipData(toolTipField: string, data: MiningTableRow): void {
    const info = { optionValue: 'tooltip', data: { data, toolTipField } };
    this.optionHover.emit(info);
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
    if ($event && !data.children.length) {
      const info = { optionValue: 'children', data };
      this.optionSelected.emit(info);
    }

    if ( ! $event && data.children) {
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
    stack.push({ ...root, level: 0, expand, key: 'parent-' + root.id });

    while (stack.length !== 0) {
      const node = stack.pop();
      this.visitNode(node, hashMap, array);
      if (node.children) {
        this.isTreeTable = true;
        for (let i = node.children.length - 1; i >= 0; i--) {
          stack.push({
            ...node.children[i],
            level: node.level + 1,
            expand: node.children[i]?.expand || this.expandedRows.includes(node.children[i]?.id),
            parent: node,
            key: 'child-' + node.children[i]?.id,
          });
        }
      }
    }
    return array;
  }

  /**
   * Life cycle hook for pagination filtering and sorting changes in the table.
   *
   * @param params data send by the query param change callbacks on table.
   */
  onQueryParamsChange(params: NzTableQueryParams): void {
    if (this.tableConfig.serverSidePagination) {
      this.queryParams.pageSize = params.pageSize;
      this.queryParams.pageIndex = params.pageIndex;
      this.queryParams.sort = params.sort;
      this.queryParamChanged.emit(this.queryParams);
    }
  }

  /**
   * Method is to reset the table filters when switching between the savedsearch filters.
   */
  resetTablefilters(): void {
    this.queryParams.filter = [];
  }

  /**
   * Set the current table filter, can be used for filter initialisation or external reset
   * @param filters filters currently applied to the filter (not TO APPLY)
   */
  setCurrentTableFilters(filters: Array<{ key: string, value: NzTableFilterValue }>): void {
    this.queryParams.filter = filters;
  }

  /**
   * Resolves the fields that are functions or '.' separators.
   *
   * @param data data send by the callbacks.
   * @param field field send by the callbacks.
   */
  resolveFieldData(data: MiningTableRow, field: string): any | null {
    if (!data || !field) {
      return null;
    }
    if (field.indexOf('.') === -1) {
      return data[field] || data[field] === 0 ? data[field] : null;
    }
    const fields: string[] = field.split('.');
    let value = data;
    for (let i = 0, len = fields.length; i < len; ++i) {
      if (value == null) {
        return null;
      }
      value = value[fields[i]];
    }
    return value;
  }

  /**
   * Resolves the fields that are functions or '.' separators.
   *
   * @param data data send by the callbacks.
   * @param field field send by the callbacks.
   * @returns value in the form of string array or string.
   */
  filterArray(data: Record<any, any>, field: string): string {
    return getValueAtPath(data, field);
  }

  /**
   * Method to calculate the indent for a row that cannot be expanded
   * in case the row does not have any children under it.
   *
   * @param item The data being displayed in the row
   */
  calculateIndent(item: MiningTableRow): number {
    let indent = (item.level ? item.level : 0) * 24;
    if (item.nonExpandableRowIndent && !item.children) {
      indent++;
    }
    return indent;
  }

  /**
   * Method return the Alignment on the basis of String or Number.
   *
   * @param fieldType type of the field
   * @returns string value right or left .
   */
  getAlign(fieldType?: FieldTypeEnum): 'left' | 'right' | 'center' {
    return fieldType === FieldTypeEnum.NUMBER ? 'right' : 'left';
  }

  /**
   * method to  convert in to thousand formatter
   * @param   totalRecords total table  data length
   * @param   tableRecords total data length if any filter applied
   * @returns  formatted number in to thousand
   */
  totalNumberRecords(totalRecords: number, tableRecords: number): number {
    return totalRecords ? totalRecords : tableRecords;
  }

  /** method to sort the table column
   * a defines the column of the table
   * b defines the next  column of the table
   * sortOrder order in which to sort the data.
   * @param  column defines the column property of the table
   */
  sortingFunction(a: MiningTableRow, b: MiningTableRow, column: Column, sortOrder: NzTableSortOrder): number {
    let valueA: string | number = `${this.resolveFieldData(a, column.field)}`;
    let valueB: string | number = `${this.resolveFieldData(b, column.field)}`;
    // To keep the null values always at the end.
    if (sortOrder === 'ascend') {
      if (valueA === 'null' || valueA === 'Unassigned') {
        return 1;
      } else if (valueB === 'null' || valueB === 'Unassigned') {
        return -1;
      }
    } else if (sortOrder === 'descend') {
      if (valueA === 'null' || valueA === 'Unassigned') {
        return -1;
      } else if (valueB === 'null' || valueB === 'Unassigned') {
        return 1;
      }
    }

    if (column.fieldType !== FieldTypeEnum.NUMBER) {
      return valueA.localeCompare(valueB, undefined, { numeric: true, sensitivity: 'base' });
    }
    valueA = valueA !== 'N/A' ? Number(valueA?.split(',')?.join('')) : 0;
    valueB = valueB !== 'N/A' ? Number(valueB?.split(',')?.join('')) : 0;
    return valueA - valueB;
  }

  /**
   * method to handle both client and server side filtering
   * @param  selectedFilterDetails value of selected filter
   * @param  columnField column name on which filter is applying
   */
  handleSelectedFilter(selectedFilterDetails: TableFilter[], columnField: string): void {
    this.queryParams.filter = this.queryParams.filter.filter(item => item.key !== columnField);
    if (selectedFilterDetails.length > 0) {
      this.queryParams.filter.push({
        key: columnField,
        value: selectedFilterDetails.map(detail => {
          if (detail.operator) {
            return { value: detail.value, operator: detail.operator };
          } else {
            return detail.value;
          }
        })
      });
    } else {
      this.error = '';
    }
    if (this.tableConfig.serverSidePagination) {
      if (this.setOfCheckedId.size > 0) {
        this.filteredIds.emit(this.setOfCheckedId);
      }
      this.refreshCheckedStatus();
      this.onQueryParamsChange(this.queryParams);
    } else {
      this.clientSideFiltering();
    }
  }

  /**
   * Gets the placeholder and errortip text using translate service.
   *
   * @param columnField the column for which the text should be retrieved.
   * @param inputString the type of text eg-placeholder or errortip.
   * @returns the text to be displayed.
   */
  getMessage(columnField: string, inputString: string): string {
    const min = this.getMinValueForFilter(columnField);
    const max = this.getMaxValueForFilter(columnField);
    return this.translateService.instant(inputString, { min, max });
  }

  /**
   * Closes the loading spinner along with cancle button.
   */
  cancelCurrentLoading(): void {
    this.cancelLoading = false;
    this.loading = false;
    this.tableConfig.contentLoading = false;
    this.loadingCancel.emit();
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
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.eventsSubscriptions.unsubscribe();
  }

  /**
   * Determine if the table title should be shown
   * @returns boolean value to show the title bar
   */
  showTitleBar(): boolean {
    return this.tableConfig.showTotalCount || this.tableConfig.isExportVisible || this.tableConfig.isCustomizable;
  }

  /**
   * shows the text and count from the dropdown for current page
   * @returns as string for dropdown
   */
  showCurrentDropDownText(): string {
    if (this.deselectCurrent && this.setOfCheckedId.size) {
      this.deselectCurrent = true;
      return this.translateService.instant('miningTable.deselectCurrent', { length: Object.keys(this.childValues).length });
    } else {
       this.deselectCurrent = false;
      return this.translateService.instant('miningTable.currentPage', { length: Object.keys(this.childValues).length });
    }
  }

  /**
   * Sets the sort function for the columns.
   */
  private setColumnSorting() {
    this.columns.forEach((column) => {
      if (column.sortFn === undefined) {
        column.sortFn = (a: MiningTableRow, b: MiningTableRow, sortOrder: NzTableSortOrder) => this.sortingFunction(a, b, column, sortOrder);
      }
    });
  }

  /**
   * Generates the filter list for the table columns.
   */
  private setColumnFilterList() {
    this.columns.forEach((column) => {
      const filterProperties = column.filterProperties;
      if (filterProperties && filterProperties.filterType === FilterType.multiSelect && ! filterProperties.listOfFilter) {
        filterProperties.listOfFilter = [];
        const valuesEntered: any = [];
        this.values.forEach((rowValue) => {
          const cellValue = rowValue[column.field];
          if (cellValue && valuesEntered.indexOf(cellValue) === -1) {
            filterProperties.listOfFilter.push({ text: cellValue, value: cellValue });
            valuesEntered.push(cellValue);
          }
        });
      }
    });
  }

  /**
   * Sorting the column filter list alphabetically
   */
  private orderColumnFilterList() {
    this.columns.forEach((column) => {
      const filterProperties = column.filterProperties;
      if (filterProperties?.filterType === FilterType.multiSelect && filterProperties.listOfFilter?.length > 0) {
        filterProperties.listOfFilter.sort((a: any, b: any) => {
          if (a.text === 'None') {
            return 1;
          } else if (b.text === 'None') {
            return -1;
          } else if (typeof a.text === 'string') {
            return a.text.toLowerCase().localeCompare(b.text?.toLowerCase());
          }
          return true;
        });
      }
    });
  }

  /**
   * Sets the child values.
   */
  private setChildrenValues() {
    this.childValues = [];
    if (this.valuesToDisplay) {
      this.valuesToDisplay.forEach((item) => {
        this.childValues[item.id] = this.convertTreeToList(item);
      });
    }
    this.bulkSelectionOptionsDropdown();
  }

  /**
   * shows the text and count from the dropdown
   */
  private bulkSelectionOptionsDropdown() {
    const translationKey = this.selectedState === StatesType.ALL ? 'miningTable.deselectAll' : 'miningTable.selectAll';
    this.bulkSelectionOptions = [
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

  private visitNode(node: any, hashMap: { [key: string]: boolean }, array: any[]): void {
    if (!hashMap[node.key]) {
      hashMap[node.key] = true;
      array.push(node);
    }
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
        this.values = this.values.filter((item) => item.id !== resp.id);
        break;

      case AllowedTableActions.TOOLTIP:
        this.tooltip = resp.data;
        reloadData = false;
        break;

      case AllowedTableActions.UPDATE:
        index = this.values.findIndex((item) => item.id === resp.id);
        this.values[index] = resp.data;
        break;

      case AllowedTableActions.EXPAND_CHILD:
        this.reloadTableData();
        const parentIndex = this.childValues[resp.id].findIndex((item: MiningTableRow) => !item.hasOwnProperty('parent'));

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
            const records = element ? element.children : this.values;
            element = records.find((rec: MiningTableRow) => rec.name === item);
          });
          element.expand = true;
          element.children = resp.data.children;
          if ( ! this.expandedRows.includes(element.id)) {
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
      this.reloadTableData();
    }
  }

  /* Reloads the Table data */
  private reloadTableData() {
    this.valuesToDisplay = [ ...this.values ];
    this.setChildrenValues();
  }

  private clientSideFiltering(): void {
    this.valuesToDisplay = [...this.values];
    if (this.queryParams.filter.length) {
      this.queryParams.filter.forEach((filter: { key: string, value: any[] }) => {
        const currentData = JSON.parse(JSON.stringify(this.valuesToDisplay)); // Deep Copy
        const filteredData: any[] = [];
        const filterType = this.columns.find(column => column.field === filter.key).filterProperties.filterType;
        if (filterType === FilterType.numberValue) {
          const numberFilter: TableFilter = filter.value[0]; // number filters only have one value
          if (numberFilter.operator === Operators.EQUALS) {
            this.valuesToDisplay = this.valuesToDisplay.filter((row: Record<any, any>) => +(numberFilter.value) === +(this.getValueAtPath(row, filter.key)));
          } else if (numberFilter.operator === Operators.GTE) {
            this.valuesToDisplay = this.valuesToDisplay.filter((row: Record<any, any>) => +(numberFilter.value) <= +(this.getValueAtPath(row, filter.key)));
          } else {
            this.valuesToDisplay = this.valuesToDisplay.filter((row: Record<any, any>) => +(numberFilter.value) >= +(this.getValueAtPath(row, filter.key)));
          }
        } else {
          currentData.forEach((row: Record<any, any>) => {
            if (this.filterData(filter, row)) {
              filteredData.push(this.filterData(filter, row));
            }
          });
          this.valuesToDisplay = [...filteredData];
        }
      });
    }
    // in case of nested data only
    if (this.valuesToDisplay[0]?.children) {
      this.setChildrenValues();
    }
  }

  private filterData(filter: { key: string, value: string[] }, row: Record<any, any>): Record<any, any> {
    const val: string = this.getValueAtPath(row, filter.key);
    if (filter.value.indexOf(val) > -1 || val?.toLocaleLowerCase().includes(filter.value?.toString().toLocaleLowerCase())) {
      return row;
    } else if (row.children) {
      const children: any[] = [];
      row.children.forEach((row: Record<any, any>) => {
        if (this.filterData(filter, row)) {
          children.push(this.filterData(filter, row));
        }
      });

      if (children.length) {
        row.children = children;
        row['expand'] = true;
        return row;
      }
    }
    return null;
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
}
