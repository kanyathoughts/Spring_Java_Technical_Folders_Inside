import { Validators } from '@angular/forms';
import { Params } from '@angular/router';

/**
 * Interface for the updating column inside the table.
 */
 export interface ColumnEditOption {
  enableEditing: boolean;
  validations?: Validators[];
  validationMessages?: Record<string, string>;
  // inputType?: string; // not needed right now as we are just focusing on the text fields only.
  onSubmit?(data: any): void;
  onCancel?(data: any): void;
  onEditingStart?(data: any): void;
}

/**
 * Interface for the Menu option item and cell options.
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
  options?: OptionItem[]
}

/**
 * Interface for the Response expected from the callback methods to the Mining table
 * for manipulating the display data.
 */
export interface MiningTableAction {
    id?: number;
    action: AllowedTableActions;
    data?: any;
}

/**
 * Enum for defining the allowed Actions for the Mining table.
 */
export enum AllowedTableActions {
    ADD = 'add',
    UPDATE = 'update',
    DELETE = 'delete',
    TOOLTIP = 'tooltip',
    EXPAND_CHILD = 'expandChild',
    ADD_CHILD = 'addChild',
    CANCEL = 'cancel',
    TOGGLE_ACTIONS = 'toggleActions',
    RESTRICT_EDITING = 'restrictEditing'
}

/**
 * Type of links for the table component.
 */
 export enum LinkType {
  BUTTON = 'button', HYPERLINK = 'hyperlink', DROPDOWN = 'dropdown', EMPTY = 'empty', STRING = 'string'
}

/**
 * Type of States for checkbox the states of table.
 */
export enum StatesType {
  ALL = 'ALL',
  SOME = 'SOME',
  NONE = 'NONE'
}

/**
 * Interface for the column level action.
 */
 export interface ColumnAction {
  type: LinkType;
  onClick?(data: any): void;
  resolveURL?(data: any, index?: number): string;
  resolveURLParams?(data: any, index?: number): Params;
}
