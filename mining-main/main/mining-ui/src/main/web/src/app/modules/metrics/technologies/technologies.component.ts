import { Component, OnDestroy, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { forkJoin, Subscription } from 'rxjs';
import { ChartFilters, ChartType, Kpi, MetricsCard, MetricsTableType } from '../shared/components/metrics-card/metrics-card.interface';
import { ChartGlobalStyles } from '../shared/utils/chart-global-styles.utils';
import { MetricsFilterService } from '@app/core/services/metrics-filter.service';
import { TechnologiesChartData } from './technologies-chartdata.interface';
import { TaxonomyDetails } from '@app/shared/interfaces/taxonomy-list.interface';
import { getTaxonomyIdsTitle } from '@app/core/utils/taxonomy.utils';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import {
  AggregationRequestModuleFieldName,
  AggregationResultModuleFieldName,
  ModuleControllerService
} from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-technologies',
  templateUrl: './technologies.component.html'
})
export class TechnologiesComponent implements OnInit, OnDestroy {

  metricCardList: MetricsCard[] = [];
  loadState: LoaderState;
  clientProjectSubscription: Subscription;
  metricsFilterSubscription: Subscription;
  combinedSubscriptions: Subscription[] = [];
  chartDataSubscriptions: Subscription[] = [];
  projectId: number;
  labels: { [key: string]: { [key: string]: string } };
  private readonly AGG_LINES_OF_CODE = AggregationRequestModuleFieldName.GroupByEnum.LINES_OF_CODE;

  constructor(
    private moduleControllerService: ModuleControllerService,
    private translationService: TranslateService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private metricsFilterService: MetricsFilterService,
    private chartGlobalStyles: ChartGlobalStyles,
    private labelMappingService: LabelMappingService
  ) { }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.metricsFilterSubscription = this.metricsFilterService.getMetricsTaxonomyFilter().subscribe((filterDetail: TaxonomyFilterSelected[]) => {
        const filterDetails: TaxonomyDetails  = getTaxonomyIdsTitle(filterDetail);
        this.metricCardList.length = 0;
        this.chartDataSubscriptions?.forEach((chartDataSubscription: Subscription) => chartDataSubscription.unsubscribe());
        this.chartDataSubscriptions.length = 0;
        this.loadState = LoaderState.loading;
        this.projectId = response.getProjectId();
        this.fetchCharts(this.projectId, filterDetails.taxonomyIds, filterDetails.taxonomyTitles);
      });
    });
    this.combinedSubscriptions.push(this.metricsFilterSubscription, this.clientProjectSubscription);
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.combinedSubscriptions.forEach(subscription => subscription?.unsubscribe());
    this.chartDataSubscriptions?.forEach((chartDataSubscription: Subscription) => chartDataSubscription.unsubscribe());
  }

  private fetchCharts(projectId: number, filterId: number[], filterTitle: string[]) {
    const taxonomyFilterObj = filterId.length ? { 'TAXONOMY_ID': { 'in': filterId }} : {};
    const requestAnalyzed: AggregationRequestModuleFieldName = {
      filterObject: { ...taxonomyFilterObj, IDENTIFICATION: { 'eq': 'IDENTIFIED' as any }},
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY, AggregationRequestModuleFieldName.GroupByEnum.TYPE]),
      orderBy: [AggregationRequestModuleFieldName.OrderByEnum.TECHNOLOGY],
      fields: {
        [AggregationRequestModuleFieldName.GroupByEnum.ID]: 'COUNT',
      }
    };

    const requestAnalyzedForSum: AggregationRequestModuleFieldName = {
      ...requestAnalyzed,
      filterObject: {
        ...taxonomyFilterObj,
        IDENTIFICATION: { 'eq': 'IDENTIFIED' as any },
        REPRESENTATION: { 'eq': 'PHYSICAL' as any}
      },
      fields: {
        [this.AGG_LINES_OF_CODE]: 'SUM'
      }
    };
    const requestMissing: AggregationRequestModuleFieldName = { ...requestAnalyzed };
    requestMissing.filterObject = {
      ...taxonomyFilterObj,
      IDENTIFICATION: { 'eq': 'MISSING' as any }
    };
    const aggregatorData = forkJoin([
      this.moduleControllerService.getAggregatedValues2(projectId, requestAnalyzed),
      this.moduleControllerService.getAggregatedValues2(projectId, requestMissing),
      this.moduleControllerService.getAggregatedValues2(projectId, requestAnalyzedForSum),
    ]).subscribe(
      ([analyzed, missing, analyzedSum]: [AggregationResultModuleFieldName[], AggregationResultModuleFieldName[], AggregationResultModuleFieldName[]]) => {
      const listAnalyzedTechno = this.filterTechno(analyzed);
      const listMissingTechno = this.filterTechno(missing);
      const listAnalyzedSum = this.filterTechno(analyzedSum);
      const technoSet = new Set(listAnalyzedTechno.concat(listMissingTechno).concat(listAnalyzedSum).sort());
      if (technoSet && technoSet.size) {
        technoSet.forEach((techno: string) => {
          this.buildChartCard(techno, analyzed, missing, analyzedSum, filterTitle, filterId);
        });
        this.loadState = LoaderState.success;
      } else {
        this.loadState = LoaderState.nocontent;
      }
    }, () => {
      this.loadState = LoaderState.error;
    });

    this.chartDataSubscriptions.push(aggregatorData);
  }

  private buildChartCard(
    techno: string,
    analyzed: AggregationResultModuleFieldName[],
    missing: AggregationResultModuleFieldName[],
    analyzedSum: AggregationResultModuleFieldName[],
    filterTitle: string[],
    filterId: number[]
  ) {
    const chartData: TechnologiesChartData[] = [];
    const uniqueAnalyzedKeys: string[] = [];
    let id: number;
    const missingTechnoAggreg: AggregationResultModuleFieldName[] = missing.filter(
      result => result.group[AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY] + '' === techno);
    const analyzedTheSum: AggregationResultModuleFieldName[] = analyzedSum.filter(
      result => result.group[AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY] + '' === techno);
    missingTechnoAggreg.forEach((aggreg) => {
      if (uniqueAnalyzedKeys.indexOf(aggreg.group[AggregationRequestModuleFieldName.GroupByEnum.TYPE] + '') === - 1) {
        id = +aggreg.fields[AggregationRequestModuleFieldName.GroupByEnum.ID];
        uniqueAnalyzedKeys.push(aggreg.group[AggregationRequestModuleFieldName.GroupByEnum.TYPE] + '');
        chartData.push({ ...this.missingAnalyzedObj(aggreg, id, 'Missing') });
        chartData.push({ ...this.missingAnalyzedObj(aggreg, 0, 'Analyzed') });
      }
    });
    const analyzedTechnoAggreg: AggregationResultModuleFieldName[] = analyzed.filter(
      result => result.group[AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY] + '' === techno);
    analyzedTechnoAggreg.forEach((aggreg) => {
      if (uniqueAnalyzedKeys.indexOf(aggreg.group[AggregationRequestModuleFieldName.GroupByEnum.TYPE] + '') === -1) {
        id = +aggreg.fields[AggregationRequestModuleFieldName.GroupByEnum.ID];
        chartData.push({ ...this.missingAnalyzedObj(aggreg, 0, 'Missing') });
        chartData.push({ ...this.missingAnalyzedObj(aggreg, id, 'Analyzed') });
      } else {
        chartData.forEach((chartDataItem) => {
          if (chartDataItem.key === aggreg.group[AggregationRequestModuleFieldName.GroupByEnum.TYPE] + '') {
            chartDataItem.totalFile = chartDataItem.totalFile + +aggreg.fields[AggregationRequestModuleFieldName.GroupByEnum.ID];
            if (chartDataItem.type === 'Analyzed') {
              chartDataItem.value = +aggreg.fields[AggregationRequestModuleFieldName.GroupByEnum.ID];
            }
          }
        });
      }
    });
    const kpiList: Kpi[] = this.buildKpilist(analyzedTechnoAggreg, missingTechnoAggreg, analyzedTheSum);
    chartData.sort((a: { key: string, value: number, totalFile?: number },
      b: { key: string, value: number, totalFile?: number }) => b.totalFile - a.totalFile);
      chartData.forEach((data: TechnologiesChartData) => {
        data.key = this.labelMappingService.mapLabel(LabelType.TYPE, data.key);
      });
      this.metricCardList.push({
      title: this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, techno),
      kpiList,
      chartType: ChartType.COLUMN,
      chartConfig: this.chartGlobalStyles.getColumnConfig(chartData, 'key', 'value', 'stackColumnTheme', true, 'type', 'stackColumnLabel', true),
      chartFilterData: {
        tableType: MetricsTableType.ModuleDetailsTable,
        showChartDetails: true,
        showExportButton: true,
        filterArgs: ['key', 'type', 'typeFilter'],
        queryFilterBuilder: (chartValues?: Record<string, string>): ChartFilters => this.generateFilterQuery(techno, filterId, chartValues, filterTitle)
      }
    });
  }

  private buildKpilist(
    analyzedTechnoAggreg: AggregationResultModuleFieldName[],
    missingTechnoAggreg: AggregationResultModuleFieldName[],
    analyzedTheSum: AggregationResultModuleFieldName[]
  ): Kpi[] {
    const kpiList: Kpi[] = [];
    const unknown = this.translationService.instant('messageService.unknown');
    // Statistics are displayed only if LoC aren't -1
    if (
      analyzedTechnoAggreg.findIndex(
        (aggreg) => +aggreg.fields[this.AGG_LINES_OF_CODE] === -1
      ) === -1 &&
      missingTechnoAggreg.findIndex(
        (aggreg) => +aggreg.fields[this.AGG_LINES_OF_CODE] === -1
      ) === -1 &&
      analyzedTheSum.findIndex(
        (aggreg) => +aggreg.fields[this.AGG_LINES_OF_CODE] === -1
      ) === -1
    ) {
      let kpiLines: number | string = unknown;
      if (analyzedTheSum.length > 0 || missingTechnoAggreg.length > 0) {
        kpiLines = analyzedTheSum.reduce(
          (a, b) => this.calculatePositiveVal(a, b, this.AGG_LINES_OF_CODE),
          0
        );
        kpiLines += missingTechnoAggreg.reduce(
          (a, b) => this.calculatePositiveVal(a, b, this.AGG_LINES_OF_CODE),
          0
        );
      }
      kpiList.push({
        title: this.translationService.instant('sourceLinesOfCode'),
        value: kpiLines,
        tooltipTitle: this.translationService.instant('projectDashboard.loCTooltip'),
      });

      let kpiMissing: number | string = unknown;
      if (missingTechnoAggreg.length > 0) {
        kpiMissing = missingTechnoAggreg.reduce(
          (a, b) => this.calculatePositiveVal(a, b, AggregationRequestModuleFieldName.GroupByEnum.ID),
          0
        );
      }

      kpiList.push({ title: this.translationService.instant('metrics.modulesMissing'), value: kpiMissing });
      let kpiAnalyzed: number | string = unknown;
      if (analyzedTechnoAggreg.length > 0) {
        kpiAnalyzed = analyzedTechnoAggreg.reduce(
          (a, b) => this.calculatePositiveVal(a, b, AggregationRequestModuleFieldName.GroupByEnum.ID),
          0
        );
      }

      kpiList.push({ title: this.translationService.instant('metrics.modulesAnalyzed'), value: kpiAnalyzed });
    }
    return kpiList;
  }

  private onlyUniqueTechnology(value: AggregationResultModuleFieldName, index: number, self: AggregationResultModuleFieldName[]) {
    const technoIndex = self.findIndex((result: AggregationResultModuleFieldName) =>
      result.group[AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY] === value.group[AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY]
    );
    return technoIndex === index;
  }

  private calculatePositiveVal(sum: number, currentAggreg: AggregationResultModuleFieldName, propertyName: string) {
    return sum + (+currentAggreg.fields[propertyName] >= 0 ? +currentAggreg.fields[propertyName] : 0);
  }

  private filterTechno(technologies: AggregationResultModuleFieldName[]) {
    return [...technologies].filter(this.onlyUniqueTechnology).map(
      (aggregation: AggregationResultModuleFieldName) => aggregation.group[AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY] + '');
  }

  private missingAnalyzedObj(aggreg: AggregationResultModuleFieldName, value: number, type: string): TechnologiesChartData {
    return {
      key: aggreg.group[AggregationRequestModuleFieldName.GroupByEnum.TYPE] + '',
      value,
      type,
      totalFile: +aggreg.fields[AggregationRequestModuleFieldName.GroupByEnum.ID],
      typeFilter: aggreg.group[AggregationRequestModuleFieldName.GroupByEnum.TYPE] + ''
    };
  }

  private generateFilterQuery(techno: string, filterId: number[], chartValues: Record<string, string>, filterTitle: string[]): ChartFilters {
    const filterObject = {};
    let filters = { technology: this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, techno) };
    filterObject['content_technology'] = { 'eq': techno };
    if (Object.keys(chartValues).length !== 0) {
      filterObject['content_type'] = { 'eq': chartValues['typeFilter']};
      const type = chartValues['type'].toLowerCase();
      if (type === 'missing') {
        filterObject['content_identification'] = { 'eq': 'MISSING'};
      } else {
        filterObject['content_identification'] = { 'eq':  'IDENTIFIED'};
      }
      filters['type'] = chartValues['key'];
      filters['category'] = chartValues['type'];
    }
    if (filterId.length) {
      filterObject['content_taxonomies_id'] = { 'in': filterId };
      filters = Object.assign({ taxonomy: filterTitle }, filters);
    }
    return { filterObject, filters };
  }
}
