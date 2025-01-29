/**
 * Method is used to sort the array naturally.
 * @param list array to be sorted.
 * @param sortByProperty property to sort by.
 * @returns naturally sorted array.
 */
export const sortNaturally = (list: Array<{[key: string]: any}>, sortByProperty: string): Array<{[key: string]: any}> =>
  list.sort((listItemA, listItemB) =>
    listItemA[sortByProperty]?.toLowerCase().localeCompare(listItemB[sortByProperty]?.toLowerCase(),'en', { numeric: true })
  );

  /** method to sort a given array of object based on the key and direction
   * @param  sortingArray which needs to be sorted
   * @param  key on which its needs to be sorted
   * @param  direction 'ASC' or 'DSC'
   * @param  type sorting for string or number.
   * @returns sorted array.
   */
export const sortArrayBasedOnKeyAndDirection = (sortingArray: any[], key: string, direction: string, type: string): any[] => {
  const sortedArray = [...sortingArray];
  if(type === 'string') {
    sortedArray.sort((a: string, b: string) => direction === 'ASC' ? a[key].localeCompare(b[key]) : b[key].localeCompare(a[key]));
  } else {
    sortedArray.sort((a: any, b: any) =>  direction === 'ASC' ?  a[key] - b[key] : b[key] - a[key]);
  }
  return sortedArray;
};
