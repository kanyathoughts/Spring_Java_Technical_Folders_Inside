
export interface Page<T> {

    content?: T[];

    firstElement?: number;

    lastElement?: number;

    limit?: number;

    offset?: number;
    
    size?: number;

    totalElements?: number;

}

export function isPage(page: any): page is Page<any> {
    const maybePage = page as Page<any>;
    return maybePage.content !== undefined
            && maybePage.offset !== undefined
            && maybePage.size !== undefined;
}