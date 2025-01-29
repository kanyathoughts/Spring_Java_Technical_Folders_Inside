import { Component, OnDestroy, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';
import { ChartFilters, ChartType, DescriptionPosition, MetricsCard, MetricsTableType } from '../shared/components/metrics-card/metrics-card.interface';
import { ChartGlobalStyles } from '../shared/utils/chart-global-styles.utils';
import { AggregationRequestModuleFieldName, AggregationResultModuleFieldName, ModuleControllerService } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-utilities',
  templateUrl: './utilities.component.html'
})
export class UtilitiesComponent implements OnInit, OnDestroy {

  metricCardList: MetricsCard[] = [];
  loadState: LoaderState;
  loadStateCount = 0;
  filterIds: number[];

  clientProjectSubscription: Subscription;
  chartDataSubscriptions: Subscription[] = [];
  projectId: number;
  taxonomyTitles: string[];

  constructor(
    private moduleControllerService: ModuleControllerService,
    private translationService: TranslateService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private chartGlobalStyles: ChartGlobalStyles) { }

  ngOnInit(): void {
    // setting both the filterIds and taxonomyTitles as empty temporarly will set it back once taxonomy filter available for it.
    this.filterIds = [];
    this.taxonomyTitles = [];
    this.metricCardList.length = 0;
    this.loadState = LoaderState.loading;
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.chartDataSubscriptions?.forEach((chartDataSubscription: Subscription) => chartDataSubscription.unsubscribe());
      this.chartDataSubscriptions.length = 0;
      this.projectId = response.getProjectId();
      this.fetchAggregatedUtilityValues(response.getProjectId());
    });
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
    this.chartDataSubscriptions?.forEach((chartDataSubscription: Subscription) => chartDataSubscription.unsubscribe());
  }

  private fetchAggregatedUtilityValues(projectId: number): void {
    let filterObject = {};
    if (this.filterIds.length) {
      filterObject = { 'TAXONOMY_ID': { 'in': this.filterIds }};
    }
    this.loadStateCount = 2;
    const requestCategoryAndInvocations: AggregationRequestModuleFieldName = {
      filterObject,
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.CATEGORIES]),
      fields: {
        [AggregationRequestModuleFieldName.GroupByEnum.ID]: 'COUNT'
      }
    };
    const requestCategory: AggregationRequestModuleFieldName = {
      filterObject,
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.CATEGORIES]),
      fields: {
        [AggregationRequestModuleFieldName.GroupByEnum.ID]: 'COUNT_DISTINCT'
      }
    };

    const aggregateData = this.moduleControllerService.getAggregatedUtilityValues(projectId, requestCategoryAndInvocations)
      .subscribe((results: AggregationResultModuleFieldName[]) => {
        if (results && results.length) {
          let chartData: Array<{ key: string, value: number }> = results.map(
            (aggreg) => ({
              key: this.labelCategories(aggreg),
              value: aggreg.fields[AggregationRequestModuleFieldName.GroupByEnum.ID] as any
            })
          );
          chartData = this.mergeCategories(chartData);
          this.metricCardList.push({
            title: this.translationService.instant('metrics.utilities.utilityInvocation'),
            description: this.translationService.instant('metrics.utilities.utilityInvocationDescription'),
            descriptionPosition: DescriptionPosition.Top,
            chartType: ChartType.COLUMN,
            chartConfig: this.chartGlobalStyles.getColumnConfig(chartData, 'key', 'value', 'classTheme', false, '', 'defaultLabel', true),
            position: 0,
            chartFilterData: {
              showChartDetails: true,
              tableType: MetricsTableType.UtilitiesTable,
              filterArgs: ['key', 'value'],
              queryFilterBuilder: (chartValues?: Record<string, string>): ChartFilters => this.generateFilterQuery(chartValues),
              dataFilter: (category: any, filterVal?: Array<Record<string, string | number>>) => this.filterUtilityData(category,filterVal)
            }
          });
        }
        this.setLoadState();
      }, () => {
        this.setLoadState();
      });
    this.chartDataSubscriptions.push(aggregateData);
    const aggregateUtilityChartData = this.moduleControllerService.getAggregatedUtilityValues(projectId, requestCategory)
      .subscribe((results: AggregationResultModuleFieldName[]) => {
        if (results && results.length) {
          let chartData: Array<{ key: string; value: number; }> = results.map(
            (aggreg) => ({
              key: this.labelCategories(aggreg).toString(),
              value: +aggreg.fields[AggregationRequestModuleFieldName.GroupByEnum.ID]
            })
          );
          chartData = this.mergeCategories(chartData);
          this.metricCardList.push({
            title: this.translationService.instant('metrics.utilities.utilityCategories'),
            chartType: ChartType.COLUMN,
            chartConfig: this.chartGlobalStyles.getColumnConfig(chartData, 'key', 'value', 'classTheme', false, '', 'defaultLabel', true),
            position: 1,
            chartFilterData: {
              showChartDetails: true,
              tableType: MetricsTableType.UtilitiesTable,
              filterArgs: ['key', 'value'],
              queryFilterBuilder: (chartValues?: Record<string, string>): ChartFilters => this.generateFilterQuery(chartValues),
              dataFilter: (category: any, filterVal?: Array<Record<string, string | number>>) => this.filterUtilityData(category,filterVal)
            }
          });
        }
        this.setLoadState();
      }, () => {
        this.setLoadState();
      });
    this.chartDataSubscriptions.push(aggregateUtilityChartData);
  }

  private mergeCategories(chartData: Array<{ key: string, value: number }>): Array<{ key: string, value: number }> {
    const categories = chartData.reduce((acc, item) => {
      acc[item.key] = (acc[item.key] || 0) + item.value;
      return acc;
    }, {} as { [key: string]: number });
    return Object.entries(categories).map(([key, value]) => ({ key, value }));
  }

  /**
   * Method to check categories based on its length and return label accordingly.
   * @param aggreg aggregation response from API
   * @returns string value
   */
  private labelCategories(aggreg: AggregationResultModuleFieldName): string {
    const categories = aggreg.group[AggregationRequestModuleFieldName.GroupByEnum.CATEGORIES] as string[];
    return categories.length ? categories.join(', ') : this.translationService.instant('metrics.utilities.emptyCategories');
  }

  private setLoadState(): void {
    this.loadStateCount--;
    if (this.loadStateCount === 0) {
      if (this.metricCardList.length > 0) {
        this.metricCardList.sort((a, b) => a.position - b.position);
        this.loadState = LoaderState.success;
      } else {
        this.loadState = LoaderState.nocontent;
      }
    }
  }

  private generateFilterQuery(chartValues: Record<string, string>): ChartFilters {
    const filters = {};
    let filterObject = {};
    if (this.filterIds.length) {
      filterObject = { 'TAXONOMY_ID': { 'in': this.filterIds }};
      filters['taxonomy'] = this.taxonomyTitles;
    }
    if (chartValues['key']) {
      filters['category'] = chartValues['key'];
    }
    return {filterObject, filters};
  }

  private filterUtilityData(category: any, data: Array<Record<string, string | number>>): any {
    return data.filter((x: any) => {
      if (category === 'Uncategorized') {
        return x.category === category || x.category.length === 0;
      } else {
        return x.category === category;
      }
    });
  }
}
