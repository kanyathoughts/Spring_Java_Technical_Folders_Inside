import { AxiosResponse } from 'axios';
import { terminal } from 'terminal-kit';
import { isPage, Page } from '../../types/page';
import { withDefaultErrorHandling } from '../default-error-handling';
import readline from 'readline';

const PAGE_SIZE = 20;

export interface BrowseActionOptions {
    heading?: string;
    headers?: string[];
    width?: number;
    hasBorder?: boolean;
    interactive?: boolean;
}

export function browseSelectAction<T extends any[], R>(
    options: BrowseActionOptions,
    browseFn: (page: number, size: number, ...args: T) => Promise<AxiosResponse<R[] | Page<R>>>,
    tableRowFn: (row: R, index?: number) => string[],
    selectFn?: (data: R, index?: number) => any
): (...args: T) => Promise<void> {
    return withDefaultErrorHandling(async (...args: T) => {
        await requestPage(options, browseFn, tableRowFn, selectFn, 0, PAGE_SIZE, ...args);
    });
}

async function requestPage<T, R>(
        options: BrowseActionOptions,
        browseFn: (page: number, size: number, ...args: T[]) => Promise<AxiosResponse<R[] | Page<R>>>,
        tableRowFn: (row: R, index?: number) => string[],
        selectFn: ((data: R, index?: number) => any) | undefined,
        page: number, size: number, ...args: T[]) {

    const result: R[] | Page<R> = (await browseFn(page, size, ...args)).data;

    if (options.heading) {
        console.log(options.heading);
    }

    let hasMorePages: boolean;
    let count: number;
    if (isPage(result)) {
        hasMorePages = (result.lastElement ?? 0) < (result.totalElements ?? 0);
        count = result.content?.length ?? 0;
        displayPage(result.content ?? [], tableRowFn, options);
    } else {
        hasMorePages = false;
        count = result?.length ?? 0;
        displayPage(result ?? [], tableRowFn, options);
    }

    const allowInteractive = options.interactive ?? true;

    console.log();
    if (isPage(result) && hasMorePages) {
        if (allowInteractive) {
            console.log('Total:', result.totalElements);
            pageInteraction(options, result.content || [], page, (result.totalElements ?? 0) / size || 1, browseFn, tableRowFn, selectFn, ...args);
        } else {
            console.log('Showing the first', count, 'elements of total', result.totalElements);
        }
    } else {
        console.log('Total:', count);
    }
}

function displayPage<R>(data: R[], tableRowFn: (row: R, index?: number) => string[], options: BrowseActionOptions) {
    const table: string[][] = [
        ... data.map(tableRowFn)
    ];
    if (options.headers && options.headers.length > 0) {
        table.unshift(options.headers);
    }
    terminal.table(table, {
        firstRowTextAttr: { bold: true },
        hasBorder: options.hasBorder ?? false,
        width: options.width ?? 80
    });
}

async function pageInteraction<R, T>(
        options: BrowseActionOptions,
        items: R[],
        currentPage: number,
        totalPages: number,
        browseFn: (page: number, size: number, ...args: T[]) => Promise<AxiosResponse<R[] | Page<R>>>,
        tableRowFn: (row: R, index?: number) => string[],
        selectFn?: (data: R, index?: number) => any, ...args: T[]) {

    console.log(`(Enter page number or 'n' - next, 'p' - previous, 'c' - cancel ${selectFn ? `'#<row number>' - select entry` : ``})`);
    const rl = readline.createInterface(process.stdin, process.stdout);
    const input = await new Promise<string>(resolve => rl.question(`Page ${currentPage + 1}/${totalPages}: `, resolve));
    rl.close();

    if (input === '' || input === 'c') {
        return;
    }

    if (selectFn && input.startsWith('#') && ! isNaN(+input.substring(1))) {
        const item = +input.substring(1);
        selectFn(items[item], item);
        return;
    }

    let nextPage: number | undefined;
    if (input === 'n') {
        nextPage = Math.min(totalPages - 1, currentPage + 1);
    } else if (input === 'p') {
        nextPage = Math.max(0, currentPage - 1);
    } else if ( ! isNaN(+input)) {
        nextPage = Math.max(0, Math.min(totalPages -1, +input - 1));
    }
    if (nextPage !== undefined) {
        /* erase the current table and draw over it */
        terminal.move(0, -items.length - 6 - (options.heading ? 1 : 0));
        terminal.eraseDisplayBelow();
        console.log();
        requestPage(options, browseFn, tableRowFn, selectFn, nextPage, PAGE_SIZE, ...args);
        return;
    }

    pageInteraction(options, items, currentPage, totalPages, browseFn, tableRowFn, selectFn, ...args);
}
