import { NzTableFilterList, NzTableSortFn, NzTableSortOrder } from 'ng-zorro-antd/table';
import { Observable } from 'rxjs';
import { ColumnAction, ColumnEditOption, LinkType } from './mining-table-action.interface';
import { MiningDataPointDefinitionWithPath } from '@innowake/mining-api-angular-client';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';

/**
 * Interface defining the Menu option item and cell options.
 */
export interface OptionItem {
  label?: string;
  value?: any;
  styleClass?: string;
  icon?: string;
  title?: string;
  disabled?: boolean;
  type?: LinkType;
  prop?: {
    type?: string,
    size?: string,
    isDelete?: boolean
  }
  id?: number;
  options?: OptionItem[],
  optionToolTip?(data: MiningTableRow): string,
  disableItem?(data: MiningTableRow): boolean
  linkType?(data: MiningTableRow): LinkType
}

/**
 * Interface defining the properties used at row level
 */
export interface MiningTableRow {
  [key: string]: any,
  id?: number,
  parent?: any,
  children?: MiningTableRow[],
  expand?: boolean,
  removeActions?: any[],
  buttonToolTip?: any;
  rowClassName?: string;
}

/**
 * Type of filters for the table component.
 */
export enum FilterType {
  freeText = 'freeText',
  multiSelect = 'multiSelect',
  numberValue = 'number',
  loading = 'loading',
  treeSelect = 'treeSelect'
}

/**
 * Interface defining the Column properties of the table config.
 */
export interface Column {
  path?: string;
  header: string;
  headerToolTip?: string // for header tool tip
  field: string;
  options?: OptionItem[];
  widthColumn?: string;
  filterProperties?: FilterProperties;
  sortOrder?: NzTableSortOrder;
  sortFn?: NzTableSortFn | boolean;
  toolTipField?: string; // Field which will shown as tooltip.
  columnAction?: ColumnAction;
  hasWarning?: (item: any) => boolean;
  getLabel?: (value: string) => string;
  warningMessage?: string;
  fieldType?: FieldTypeEnum;

  /**
   * Determines how the mining table cell will be displayed.
   *
   * In the case where both the {@link displayAs} and {@link displayAsCallback}
   * are both defined, the callback takes precedence.
   */
  displayAs?: string;

  isEditable?: boolean;
  editOption?: ColumnEditOption;
  showIcon?: boolean;
  errorField?: (item: any) => string;
  /**
   * A function determining how the mining table cell will be displayed.
   *
   * In the case where both the {@link displayAs} and {@link displayAsCallback}
   * are both defined, the callback takes precedence.
   * @param value The {@link MiningTableRow} that is being displayed.
   */
  displayAsCallback?(row: MiningTableRow): ViewMode;
}

/**
 * Interface defining the properties used to filter on a column
 */
export interface FilterProperties {
  filterType: FilterType;
  filterValue?: any;
  min?: number;
  max?: number;
  step?: number;
  isFilterActive?: boolean
  listOfFilter?: NzTableFilterList;
  taxonomyData?: NzTreeNodeOptions[];
  loadingValues?: boolean;
  defaultKeys?: string[];
}

/**
 * Interface defining the scroll property of the table config.
 */
export interface TableScroll {
  x?: string;
  y?: string;
}

export interface FilterList {
  text: string;
  value: number;
  key: string;
  byDefault?: boolean;
}

/**
 * Interface defining the table configuration.
 */
export interface MiningTableConfig {
  columnMap: { [key: string]: Column };
  paginator?: boolean;
  rows?: number;
  scroll?: TableScroll;
  occupiedSpace?: number; // this is to specify the space alreacy occupied and will be used to calculate the remaining space
  actions?: OptionItem[][];
  actionsWidth?: string;
  /**
   * Full Table loading state (including header, pagination, ...)
   */
  loading?: boolean;
  /**
   * Content only loading, user can still interact with table functionalities
   */
  contentLoading?: boolean;
  serverSidePagination?: boolean;
  showTotalCount?: boolean;
  projectId?: number;
  exportType?: string;
  filters?: string;
  isExportVisible?: boolean;
  exportParameters?: { [key: string]: any }
  tableBorder?: boolean;
  isCustomizable?: boolean;
  disableAllActions?: boolean;
  showHeaderExtraButton?: boolean; // property to extra button in header
  externalExportCallback?: () => Observable<any>;
  bulkSelectionDataPoints?: MiningDataPointDefinitionWithPath[];
  isClientSideExport?: boolean;
  bulkActions?: BulkAction[];
  tableActions?: BulkAction[];
  bulkSelectPoints?: string[];
  isImportVisible?: boolean;
  importToolTipText?: string;
  importAction?(): void;
  selectAllCallback?(): void; // callback method for select All functionality for RB table.
}

export interface MiningDropDown {
  text: string;
  onSelect: () => void;
}

export interface BulkAction {
  label: string;
  tooltip?: string;
  icon?: string;
  isDanger?: boolean;
  class?: string; // this is to apply custom class to the button
  subActions?: BulkAction[]; // this should be populated for the action group only
  id?: string; // only for identifying the buttons when testing
  isBulkSelection?: boolean; // this is to identify if action is bulk selection
  action?(data?: any): void; // this is mainly for actions but could eventually be used for action-group, i.e: to fetch the sub-action only on click
}

export enum FieldTypeEnum {
  STRING = 'STRING',
  NUMBER = 'NUMBER'
}

export enum ViewMode {
  TAG = 'tag',
  LINK = 'link',
  DATE = 'date',
  ICON ='ICON',
  HTML = 'HTML',
  LINKOPENMODAL = 'linkOpenModal',

  /**
   * The link will be displayed as a simple label.
   */
  DISABLEDLINK = 'disabledlink',

  /**
   * The link will not apply any routing information to the url and open in a new browser tab.
   */
  EXTERNALLINK = 'externalLink'
}

export const DEFAULT_NUMBER_OF_ROWS = 30;

