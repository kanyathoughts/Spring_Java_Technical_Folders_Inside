import { AxiosResponse } from 'axios';
import { withDefaultErrorHandling } from '../default-error-handling';

export function standardAction<T extends any[], R>(
    actionFn: (...args: T) => Promise<AxiosResponse<R>>,
    resultFn?: (data: R) => any
): (...args: T) => Promise<void> {
    return withDefaultErrorHandling(async (...args: T) => {
        const result: R = (await actionFn(... args)).data;
        await resultFn?.(result);
    });
}
