import { ModelAlgorithmOption } from '@innowake/mining-api-angular-client';
import { ChartFilters } from '../metrics/shared/components/metrics-card/metrics-card.interface';

/**
 * Interface for DNA Card List
 */
export interface DnaCard {
  chartData: ChartData[];
  options: ModelAlgorithmOption[];
  title: string;
  clusterModuleCount: number;
  clustersLength: number;
  chartFilterData?: DnaChartFilterData,
  assignedModuleCount: number,
  selectedIndex?: number,
  uuid?: string,
  description?: string
}

/**
 * Interface for DNA chart data
 */
export interface ChartData {
  key: string;
  value: number;
  index?: number;
  description: string
  title: string;
  uuid: string
}

/* interface for chart Filter Data for DNA page */
export interface DnaChartFilterData {
  filterArgs: string[],
  queryFilterBuilder: (val: Record<string, number>) => ChartFilters,
  dataFilter?: (value: any, filterVal?: Array<Record<string, string | number>>) => any
}
