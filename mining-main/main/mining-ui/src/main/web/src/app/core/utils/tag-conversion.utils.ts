/**
 * method to convert to tag for entered values
 * @param  list parameter entered
 */
export const convertToTag = (list: string): void | string[] => {
    const quotesCheck = /["]/;
    let breakIntoTags: string[] = [];
    let splitedString: string[] = [];
    if (!quotesCheck.test(list)) {
        breakIntoTags = splitIntoTagsWithoutQuotes(list as string | string[]);
    } else {
        if (Array.isArray(list)) {
            if (list.length > 1) {
                const arrayValue = list[list.length - 1].split(/["]/);
                list.pop();
                splitedString = [...list, ...arrayValue];
            } else {
                splitedString = list[0].split(/["]/);
            }
        } else {
            splitedString = list.split(/["]/);
        }
        const filterString = splitedString.filter((x: string) => x !== '');
        filterString.forEach((y: string) => {
            y = y?.trim();
            if (y.startsWith(',') || y.endsWith(',') || y.endsWith(', ')) {
                let arr2 = y.split(',');
                arr2 = arr2.filter(ele => ele !== '');
                arr2 = arr2.filter(ele => ele !== ' ');
                breakIntoTags.push(...arr2);
            } else {
                breakIntoTags.push(y);
            }
        });
    }
    return removeDuplicates(breakIntoTags);
};

const splitIntoTagsWithoutQuotes = (str: string | string[]): string[] => {
    const breakIntoTags: string[] = [];
    let splittedTags: any;
    if (Array.isArray(str)) {
        if (str.length > 1) {
            let fullString = '';
            str.forEach(element => {
                fullString = fullString + ',' + element;
            });
            splittedTags = fullString.split(',').filter(ele => ele !== '');
        } else {
            splittedTags = str[0]?.trim().split(',');
        }
    } else {
        splittedTags = str?.trim().split(',');
    }
    splittedTags?.forEach((element: string) => {
        breakIntoTags.push(element?.trim());
    });
    return breakIntoTags;
};
const removeDuplicates = (arr: string[]): string[] => arr.filter((item: string, index: number) => arr.indexOf(item) === index);
