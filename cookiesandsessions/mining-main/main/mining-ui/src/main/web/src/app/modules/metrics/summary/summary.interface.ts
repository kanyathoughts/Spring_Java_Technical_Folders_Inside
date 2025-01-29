/**
 * Interface summary Table object.
 */
export interface SummaryTableObj {
  linesOfCode?: string | number;
  lineOfComment?: string | number;
  lineOfDeadCode?: string | number;
  modules?: string | number;
  physicalFiles?: string | number;
}

/**
 * Interface for  summary Table data.
 */
export interface SummaryTableData extends SummaryTableObj {
  modules: string | number;
  id: number;
}

/**
 * Interface for  custom Sorting Method for Summary Table.
 */
export interface SummaryTableSorting {
  linesOfCode: string;
  lineOfComment: string;
  lineOfDeadCode: string;
  modules: string;
}
