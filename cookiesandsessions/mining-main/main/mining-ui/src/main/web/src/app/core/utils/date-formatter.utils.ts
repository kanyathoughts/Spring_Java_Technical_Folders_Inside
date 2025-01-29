/**
 * Returns the date in the format YYYY-MM-DD HH:MM.
 *
 * @param inputDate : Date which needs to be formatted
 * @returns date which is formatted in the format YYYY-MM-DD HH:MM
 */
export const  dateFormatter = (inputDate: Date | string): string => {
    const date = new Date(inputDate);
    let month: string | number = date.getUTCMonth() + 1;
    month = month < 10 ? '0' + month : month;
    const day = date.getUTCDate() < 10 ? '0' + date.getUTCDate() : date.getUTCDate();
    const year = date.getUTCFullYear();
    const hours = date.getHours() < 10 ? '0' + date.getHours() : date.getHours();
    const minutes = date.getMinutes() < 10 ? '0' + date.getMinutes() : date.getMinutes();
    return year + '-' + month + '-' + day + ' ' + hours + ':' + minutes;
};
