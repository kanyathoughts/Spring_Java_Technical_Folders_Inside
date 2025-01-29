import { NzSafeAny } from 'ng-zorro-antd/core/types';
import { Operators } from '../components/type-based-filter/type-based-filter.component';

/**
 * Interface for the Table filter.
 */
export interface TableFilter {
    text: string;
    value: NzSafeAny;
    byDefault?: boolean;
    operator?: Operators;
    columnField?: string;
}
