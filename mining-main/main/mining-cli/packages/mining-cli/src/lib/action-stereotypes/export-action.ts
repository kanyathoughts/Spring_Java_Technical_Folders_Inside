import { AxiosResponse } from 'axios';
import byteSize from 'byte-size';
import { writeFile } from 'fs-extra';
import { withDefaultErrorHandling } from '../default-error-handling';

export function exportAction<T extends any[], R>(
    actionFn: (...args: T) => Promise<AxiosResponse<any>>,
    resultFn?: () => any
): (...args: T) => Promise<void> {
    return withDefaultErrorHandling(async (...args: T) => {
        const response = await actionFn(... args);
        const data = Buffer.from(response.data);
        const dispo: string = response.headers['content-disposition'];
        const filename = dispo.substring(dispo.lastIndexOf('filename=') + 'filename='.length);
        await writeFile(filename, data);
        console.log(`Downloaded ${filename} (${byteSize(data.length)})`);
        await resultFn?.();
    });
}