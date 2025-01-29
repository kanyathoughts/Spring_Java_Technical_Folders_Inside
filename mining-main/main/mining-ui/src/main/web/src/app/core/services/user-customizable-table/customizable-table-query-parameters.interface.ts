import { NzTableFilterValue } from 'ng-zorro-antd/table';

/**
 * Interface for Customizable Table Query Parameters.
 */
export interface CustomizableTableQueryParameter {
  page: number;
  filter: Array<{ key: string, value: NzTableFilterValue}>;
  sort?: string;
  preFilter?: string;
}
