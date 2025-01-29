import { Component, OnDestroy, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { MetricsFilterService } from '@app/core/services/metrics-filter.service';
import { getTaxonomyIdsTitle } from '@app/core/utils/taxonomy.utils';
import { TaxonomyDetails } from '@app/shared/interfaces/taxonomy-list.interface';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { TranslateService } from '@ngx-translate/core';
import { combineLatest, forkJoin, Observable, Subscription } from 'rxjs';
import { map } from 'rxjs/internal/operators/map';
import { catchError } from 'rxjs/operators';
import { ChartDataInterface, ChartFilters, ChartType, MetricsCard, MetricsTableType } from '../shared/components/metrics-card/metrics-card.interface';
import { ChartGlobalStyles } from '../shared/utils/chart-global-styles.utils';
import {
  AggregationRequestRelationshipFieldName,
  AggregationResultRelationshipFieldName,
  ReferenceControllerService
} from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-ims',
  templateUrl: './ims.component.html'
})
export class ImsComponent implements OnInit, OnDestroy {
  metricCardList: MetricsCard[] = [];
  loadState: LoaderState;
  projectId: number;
  combinedSubscription: Subscription;
  taxonomyFilter: {[key: string]: object};

  constructor(
    private clientProjectRelationship: ClientProjectRelationshipService,
    private referenceControllerService: ReferenceControllerService,
    private chartGlobalStylesService: ChartGlobalStyles,
    private translationService: TranslateService,
    private metricsFilterService: MetricsFilterService
  ) { }

  ngOnInit(): void {
    this.loadState = LoaderState.loading;
    this.combinedSubscription = combineLatest([
      this.clientProjectRelationship.getClientProjectObservable(),
      this.metricsFilterService.getMetricsTaxonomyFilter()
    ]).subscribe(([response, filterDetail]) => {
      const filterDetails: TaxonomyDetails = getTaxonomyIdsTitle(filterDetail);
      this.projectId = response.getProjectId();
      this.taxonomyFilter = filterDetails.taxonomyIds.length ? { 'in': filterDetails.taxonomyIds }: {};
      this.buildAllIMSCharts();
    });
  }

  /**
   * method to generate filter string based on the chart title
   * @param  chartValues clicked chart value
   * @param  title chart title of selected chart
   * @returns Object containing filter string and filter
   */
   generateFilterQuery(chartValues: Record<string, string>, title: string): ChartFilters {
    const filters = {};
    const filterObject = {
      taxonomies: this.taxonomyFilter,
      properties: {}
    };
    switch (title) {
      case this.translationService.instant('metrics.ims.allCallsChartTitle'):
        if (chartValues.key === 'UNKNOWN') {
          filterObject.properties['DB_ACCESS_TYPE'] =  null;
        } else {
          filterObject.properties['DB_ACCESS_TYPE'] = chartValues.key;
        }
      break;
      case this.translationService.instant('metrics.ims.getCallsChartTitle'):
        if (chartValues?.key) {
          filterObject.properties['DB_ACCESS_OPERATION'] = chartValues.key;
        } else {
          filterObject.properties['DB_ACCESS_TYPE'] = 'READ';
        }
      break;
      case this.translationService.instant('metrics.ims.insertDeleteReplaceCallsChart'):
        if (chartValues?.key) {
          filterObject.properties['DB_ACCESS_OPERATION'] = chartValues.key;
        } else {
          filterObject.properties['DB_ACCESS_TYPE'] = ['STORE', 'UPDATE', 'DELETE'];
        }
      break;
      case this.translationService.instant('metrics.ims.otherCallsChart'):
        if (chartValues?.key) {
          filterObject.properties['DB_ACCESS_OPERATION'] = chartValues.key;
        } else {
          filterObject.properties['DB_ACCESS_TYPE'] = 'OTHER';
        }
      break;
    }
    return { filterObject , filters };
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
   ngOnDestroy(): void {
    this.combinedSubscription.unsubscribe();
  }

  /**
   * Build all the chart if the chart representing all calls has data
   */
  private buildAllIMSCharts(): void {
    this.metricCardList = [];
    const accessType = AggregationRequestRelationshipFieldName.GroupByEnum.PROPERTY_DB_ACCESS_TYPE;
    const accessOperation = AggregationRequestRelationshipFieldName.GroupByEnum.PROPERTY_DB_ACCESS_OPERATION;

    this.fetchIMSChartData({}, [accessType], accessType).subscribe((chartData: ChartDataInterface[]) => {
      if ( ! chartData.length) {
        this.loadState = LoaderState.nocontent;
        return null;
      }

      this.createCard(this.translationService.instant('metrics.ims.allCallsChartTitle') as string, chartData, 'classTheme', 1);
      const groupBy = [ accessType, accessOperation ];
      forkJoin([
        this.fetchIMSChartData( { PROPERTY_DB_ACCESS_TYPE: { eq: 'READ' as any }}, groupBy, accessOperation),
        this.fetchIMSChartData( { PROPERTY_DB_ACCESS_TYPE: { in: ['STORE', 'UPDATE', 'DELETE'] }},groupBy, accessOperation),
        this.fetchIMSChartData( { PROPERTY_DB_ACCESS_TYPE: { in: ['OTHER'] }}, groupBy, accessOperation)
      ]).subscribe((dataForCharts: ChartDataInterface[][]) => {
        this.loadState = LoaderState.success;
        const chartTitles: string[] = [
          'getCallsChartTitle',
          'insertDeleteReplaceCallsChart',
          'otherCallsChart'
        ];
        dataForCharts.forEach((imsChart: ChartDataInterface[], index: number) => {
          if (imsChart.length > 0) {
            this.createCard(this.translationService.instant('metrics.ims.' + chartTitles[index]) as string, imsChart, 'seqTheme', index + 1);
          }
        });
      });
    });
  }

  /**
   * Generic method to fetch data related to IMS chart and map result to Chart Data
   * @param filter filter value to be passed to the request
   * @param filterGroups Value to be passed to the request
   * @param groupBy Value to be passed to the request
   * @param keyProperty Response property to use as key for the chart data
   * @returns Observable containing the chart data corresponding to the request response
   */
  private fetchIMSChartData(
    additionalFilter: { [key: string]: { [key: string]: object; }},
    groupBy: AggregationRequestRelationshipFieldName.GroupByEnum[],
    keyProperty: AggregationRequestRelationshipFieldName.OrderByEnum
  ): Observable<ChartDataInterface[]> {
    const request: AggregationRequestRelationshipFieldName = {
      filterObject: {
        ...additionalFilter,
        TAXONOMY_ID: this.taxonomyFilter,
        DST_TECHNOLOGY: { 'eq': 'IMS' as any },
        DST_TYPE: { 'eq': 'DBD' as any},
        SRC_STORAGE: { 'eq': 'FILE' as any },
        SRC_TECHNOLOGY: { 'notEq': 'IMS' as any }
      },
      groupBy: new Set(groupBy),
      fields: { ID: 'COUNT' }
    };
    return this.referenceControllerService.getAggregatedValues1(this.projectId, request, 'REFERENCES').pipe(
      map((aggregationArray: AggregationResultRelationshipFieldName[]) => (
        aggregationArray.map((resp: AggregationResultRelationshipFieldName) => ({
          key: resp.group[keyProperty] == null ? 'UNKNOWN' : resp.group[keyProperty],
          value: resp.fields['ID']
        }))
      )),
      catchError(() => {
        this.loadState = LoaderState.error;
        return [];
      })
    );
  }

  /**
   * Add a Metrics Card
   * @param title Card Title
   * @param chartData Data given to the chart
   * @param theme Theme to apply to the chart
   * @param position Card position in the page
   */
  private createCard(title: string, chartData: ChartDataInterface[], theme: string, position: number): void {
    if (this.metricCardList.findIndex(card => card.title === title) === -1) {
      this.metricCardList.push({
        title,
        chartConfig: this.chartGlobalStylesService.getPieConfig(chartData, 'value', 'key', theme, true),
        chartType: ChartType.PIE,
        position,
        chartFilterData: {
          showChartDetails: true,
          showExportButton: true,
          tableType: MetricsTableType.IMSTable,
          filterArgs: ['key'],
          queryFilterBuilder: (chartValues?: Record<string,string>): ChartFilters => this.generateFilterQuery(chartValues, title),
        }
      });
    }
  }
}
