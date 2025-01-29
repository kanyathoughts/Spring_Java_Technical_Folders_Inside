/**
 * Interface for the Metrics card KPI.
 */
export interface Kpi {
  title: string;
  value: string | number;
  tooltipTitle?: string;
}

/**
 * Enum for the chart type in metrics-card.
 */
export enum ChartType {
  COLUMN = 'column',
  PIE = 'pie',
  LINE = 'line',
  BAR = 'bar',
}

/**
 * Enum for the chart events in metrics-card.
 */
 export enum ChartEvent {
  ELEMENT = 'element',
  LEGEND = 'legend',
  CHART = 'chart',
}

/**
 * Enum for the description position in metrics-card.
 */
export enum DescriptionPosition {
  Top = 'top',
  Bottom = 'bottom',
}
/**
 * Interface for the metrics card
 */
export interface MetricsCard {
  title: string,
  description?: string,
  kpiList?: Kpi[],
  chartType?: ChartType,
  // eslint-disable-next-line @typescript-eslint/ban-types
  chartConfig?: {},
  descriptionPosition?: DescriptionPosition,
  position?: number,
  showEclipseHint?: boolean,
  chartFilterData?: ChartFilterData,
  tableType?: MetricsTableType
}

/* interface for chart Filter Data */
export interface ChartFilterData {
  showExportButton?: boolean,
  showChartDetails?: boolean,
  tableType: MetricsTableType,
  filterArgs: string[],
  queryFilterBuilder: (val: Record<string, string>,position?: number,isSqlChart?: MetricsTableType) => ChartFilters,
  dataFilter?: (value: any, filterVal?: Array<Record<string, string | number>>) => any
}

export interface ChartFilters {
  /**
   * Filter labels and values to display
   */
  filters: Record<string, string>
  /**
   * Filter Object used in data requests
   */
  filterObject?: {
    [key: string]: {
        [key: string]: object;
    };
  }
}

export enum MetricsTableType {
  ModuleDetailsTable = 'moduleDetailsTable',
  SqlDetailsTable = 'sqlDetailsTable',
  UtilitiesTable = 'utilitiesTable',
  InterfacesTable = 'interfacesTable',
  IMSTable = 'imsTable',
  DNATable = 'dnaTable'
}

export interface ChartDataInterface  {
  key: string;
  value: number;
  position?: number,
  title?: string
}

/**
 * Interface for data created for IMS Data visualization
 */
 export interface IMSFilterData {
  id: number;
  moduleName: string;
  method: string;
  databaseName: string;
  segmentName: string;
  statement: string;
  moduleId: number
}
