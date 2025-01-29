import { Component, OnDestroy, OnInit } from '@angular/core';
import { ColumnOptions, LineOptions } from '@antv/g2plot';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { MetricsFilterService } from '@app/core/services/metrics-filter.service';
import { sortArrayBasedOnKeyAndDirection } from '@app/core/utils/sort.util';
import {
  AggregationRequestModuleFieldName,
  AggregationRequestStatementFieldName,
  AggregationResultModuleFieldName,
  AggregationResultStatementFieldName,
  ModuleControllerService,
  StatementControllerService
} from '@innowake/mining-api-angular-client';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { forkJoin, Observable, Subscription } from 'rxjs';
import {
  ChartDataInterface, ChartFilters, ChartType, DescriptionPosition,
  MetricsCard, MetricsTableType
} from '../shared/components/metrics-card/metrics-card.interface';
import { ChartGlobalStyles } from '../shared/utils/chart-global-styles.utils';
import { ProgramComplexity } from './code-quality.interface';
import {getTaxonomyIdsTitle} from '../../../core/utils/taxonomy.utils';
import { TaxonomyDetails } from '@app/shared/interfaces/taxonomy-list.interface';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';

@Component({
  selector: 'app-code-quality',
  templateUrl: './code-quality.component.html'
})

export class CodeQualityComponent implements OnInit, OnDestroy {
  metricCardList: MetricsCard[] = [];
  loadState: LoaderState;
  taxonomyFilter: {
    [key: string]: {
        [key: string]: any;
    };
  };
  clientProjectSubscription: Subscription;
  metricsFilterSubscription: Subscription;
  combinedSubscription: Subscription[] = [];
  chartDataSubscriptions: Subscription[] = [];
  projectId: number;
  taxonomyTitle: string[] = [];
  labels: { [key: string]: { [key: string]: string } };
  private complexityRange: object[] = [
    {'COMPLEXITY': {
      'gte': 0 as any,
      'lte': 10 as any
    }},
    {'COMPLEXITY': {
      'gte': 11 as any,
      'lte': 20 as any
    }},
    {'COMPLEXITY': {
      'gte': 21 as any,
      'lte': 50 as any
    }},
    {'COMPLEXITY': {
      'gte': 51 as any
    }}
  ];
  private complexityArray: string[] = ['Low', 'Medium', 'High', 'Very High'];
  private progComplexityQuery = { 'LINES_OF_CODE': { 'gte': 1 as any }, 'REPRESENTATION': { 'eq': 'PHYSICAL' as any } };
  private screenComplexityQuery = {
    'LINES_OF_CODE': {
      'gte': 1 as any
    },
    'COMPLEXITY': {
      'gte': 0 as any
    },
    'TYPE': {
      'in': [
        'MAP',
        'BMS_MAP',
        'DIALOG',
        'DIALOG_PRIV_RES',
        'HELP'
      ]
    }
  };

  constructor(
    private moduleService: ModuleControllerService,
    private statementService: StatementControllerService,
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
        this.chartDataSubscriptions?.forEach((chartDataSubscription: Subscription) => chartDataSubscription.unsubscribe());
        this.chartDataSubscriptions.length = 0;
        this.taxonomyFilter = filterDetails.taxonomyIds.length ? { 'TAXONOMY_ID': { 'in': filterDetails.taxonomyIds }} : {};
        this.taxonomyTitle = filterDetails.taxonomyTitles;
        this.metricCardList.length = 0;
        this.projectId = response.getProjectId();
        this.loadState = LoaderState.loading;
        this.fetchChartsData(response.getProjectId());
      });
    });
    this.combinedSubscription.push(this.metricsFilterSubscription, this.clientProjectSubscription);
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.combinedSubscription.forEach(subscription => subscription?.unsubscribe());
    this.chartDataSubscriptions?.forEach((chartDataSubscription: Subscription) => chartDataSubscription.unsubscribe());
  }

  private fetchChartsData(projectId: number): void {
    const programComplexity: Array<Observable<AggregationResultModuleFieldName[]>> = [];
    const programComplexityLoc: Array<Observable<AggregationResultModuleFieldName[]>> = [];
    const filterObject = {...this.progComplexityQuery, ...this.taxonomyFilter};

    const progComplexityRequest: AggregationRequestModuleFieldName = {
      filterObject,
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.COMPLEXITY]),
      fields: {
        ID: 'COUNT'
      }
    };

    const progCycloMaticAndLoc: AggregationRequestModuleFieldName = {
      filterObject,
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY]),
      fields: {
        ID: 'COUNT'
      }
    };

    const sqlFilterObject = {
      ...this.taxonomyFilter,
      'HALSTEAD_COMPLEXITY': { 'gt': 0 as any },
      'TECHNOLOGY': { eq: 'SQL' as any }
    };
    const sqlStatementRequest: AggregationRequestStatementFieldName = {
      filterObject: sqlFilterObject,
      groupBy: new Set([AggregationRequestStatementFieldName.GroupByEnum.HALSTEAD_COMPLEXITY]),
      orderBy: [AggregationRequestStatementFieldName.OrderByEnum.HALSTEAD_COMPLEXITY],
      fields: {
        ID: 'COUNT'
      }
    };
    const screenComplexityRequest = { ...progComplexityRequest };
    screenComplexityRequest.filterObject = {...this.screenComplexityQuery, ...this.taxonomyFilter};
    this.complexityRange.forEach((rangeItem) => {
      const programComplexityWithRange = { ...progCycloMaticAndLoc, filterObject: { ...this.progComplexityQuery, ...rangeItem, ...this.taxonomyFilter }};
      programComplexity.push(this.moduleService.getAggregatedValues2(projectId, programComplexityWithRange));
      const programComplexityWithLoc = { ...programComplexityWithRange };
      programComplexityWithLoc.fields = { LINES_OF_CODE: 'SUM' };
      const programComplexityWithRangeWithLoc = { ...programComplexityWithLoc, filterObject: {...this.progComplexityQuery, ...rangeItem,
        ...this.taxonomyFilter }};
      programComplexityLoc.push(this.moduleService.getAggregatedValues2(projectId, programComplexityWithRangeWithLoc));
    });
    this.fetchProgramCycloMetricsComplexity(programComplexity, 1, 'ID');
    this.fetchProgramCycloMetricsComplexity(programComplexityLoc, 2, 'LINES_OF_CODE');
    this.fetchDataForComplexityCharts(screenComplexityRequest, projectId, 3, 'ID');
    this.fetchDataForProgramLOCvsCommentsChart(projectId);
    this.fetchDataForDeadCodeByLanguageTypeChart(projectId);
    this.fetchDataForJobStepCountChart(projectId);
    this.fetchDataForHalsteadComplexityCharts(sqlStatementRequest, projectId, 7, 'ID');
    this.fetchDataForHalsteadComplexityDistinctCharts(projectId, 8);
    this.fetchDataForSQLStatementsDistributionChart(projectId);
  }

  private fetchDataForComplexityCharts(request: AggregationRequestModuleFieldName, projectId: number, cardIndex: number, fieldType: string): void {
    const dataForComplexChart = this.moduleService.getAggregatedValues2(projectId, request)
      .subscribe((response: AggregationResultModuleFieldName[]) => {
        if (response.length) {
          this.loadState = LoaderState.success;
          const chartData = this.formatComplexityChartData(response, fieldType);
          const chartConfig = this.chartGlobalStyles.getColumnConfig(chartData, 'key', 'value', 'semaTheme', false, '', 'defaultLabel', true);
          this.addDataToCardList(chartConfig, cardIndex, ChartType.COLUMN, true, MetricsTableType.ModuleDetailsTable);
        } else {
          this.setLoadState(LoaderState.nocontent);
        }
      }, () => {
        this.setLoadState(LoaderState.error);
      });
    this.chartDataSubscriptions.push(dataForComplexChart);
  }

  private fetchDataForProgramLOCvsCommentsChart(projectId: number): void {
    const filterObject = {
      ...this.taxonomyFilter,
      'LINES_OF_CODE': { 'gte': 1 as any},
      'TYPE': { 'eq' : 'PROGRAM' as any}
    };
    const request: AggregationRequestModuleFieldName = {
      filterObject,
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.NAME]),
      orderBy: ['LINES_OF_CODE'],
      fields: {
        LINES_OF_CODE: 'SUM',
        LINES_OF_COMMENT: 'SUM'
      }
    };
    const dataForProgramChart = this.moduleService.getAggregatedValues2(projectId, request)
      .subscribe((response: AggregationResultModuleFieldName[]) => {
        if (response.length) {
          this.loadState = LoaderState.success;
          const chartData: Array<{ key: string, value: number, category: string }> = [];
          /* Doing Array reversal as with the server query can't do the order by desc */
          response.reverse();
          response.forEach((item: AggregationResultModuleFieldName) => {
            chartData.push({
              key: item.group['NAME'].toString(), value: +item.fields['LINES_OF_COMMENT'],
              category: this.translationService.instant('commentLinesOfCode')
            });
            chartData.push({ key: item.group['NAME'].toString(), value: +item.fields['LINES_OF_CODE'],
              category: this.translationService.instant('sourceLinesOfCode') });
          });
          const chartConfig = this.chartGlobalStyles.getLineConfig(chartData, 'key', 'value', 'classTheme', 'category', true);
          this.addDataToCardList(chartConfig, 4, ChartType.LINE, true, MetricsTableType.ModuleDetailsTable);
        } else {
          this.setLoadState(LoaderState.nocontent);
        }
      }, () => {
        this.setLoadState(LoaderState.error);
      });
    this.chartDataSubscriptions.push(dataForProgramChart);
  }

  private fetchDataForDeadCodeByLanguageTypeChart(projectId: number): void {
    const metricsDeadCodeKey = 'metrics.linesOfDeadCode';
    const codeFilterObject = {
      ...this.taxonomyFilter,
      'LINES_OF_CODE': {
        'gte': 1 as any
      }
    };
    const requestCode: AggregationRequestModuleFieldName = {
      filterObject: codeFilterObject,
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY]),
      fields: {
        LINES_OF_CODE: 'SUM'
      }
    };

    const deadCodeFilter = {
      ...this.taxonomyFilter,
      'LINES_OF_DEAD_CODE': {
        'gte': 1 as any
      }
    };
    const requestDeadCode: AggregationRequestModuleFieldName = {
      filterObject: deadCodeFilter,
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY]),
      fields: {
        LINES_OF_DEAD_CODE: 'SUM'
      }
    };

    const aggregateData = forkJoin([
      this.moduleService.getAggregatedValues2(projectId, requestCode),
      this.moduleService.getAggregatedValues2(projectId, requestDeadCode)
    ]).subscribe(([codeData, deadCodeData]: [AggregationResultModuleFieldName[], AggregationResultModuleFieldName[]]) => {
      if (codeData.length || deadCodeData.length) {
        this.loadState = LoaderState.success;
        const chartData: Array<{ key: string, value: number, type: string, typeFilter: string }> = [];
        if (codeData.length) {
          codeData.forEach((item: AggregationResultModuleFieldName) => {
            chartData.push({ key: item.group['TECHNOLOGY'].toString(), value: +item.fields['LINES_OF_CODE'],
              type: this.translationService.instant('sourceLinesOfCode'),
            typeFilter: item.group['TECHNOLOGY'].toString() });
            chartData.push({ key: item.group['TECHNOLOGY'].toString(), value: 0, type: this.translationService.instant(metricsDeadCodeKey),
            typeFilter: item.group['TECHNOLOGY'].toString() });
          });
        }

        if (deadCodeData.length) {
          deadCodeData.forEach((item: AggregationResultModuleFieldName) => {
            const record = chartData.find(ele =>
              ele.key === item.group['TECHNOLOGY'].toString() && ele.type === this.translationService.instant(metricsDeadCodeKey));
            if (record) {
              record.value = +item.fields['LINES_OF_DEAD_CODE'];
            } else {
              chartData.push({ key: item.group['TECHNOLOGY'].toString(), value: 0, type: this.translationService.instant('sourceLinesOfCode'),
              typeFilter: item.group['TECHNOLOGY'].toString() });
              chartData.push({
                key: item.group['TECHNOLOGY'].toString(), value: +item.fields['LINES_OF_DEAD_CODE'],
                type: this.translationService.instant(metricsDeadCodeKey), typeFilter: item.group['TECHNOLOGY'].toString()
              });
            }
          });
        }

        /* Sort by key in alphabetical order */
        chartData.sort((a, b) => a.key.localeCompare(b.key));
        chartData.forEach(data=>{
          data.key = this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, data.key);
        });
        const chartConfig = this.chartGlobalStyles.getColumnConfig(chartData, 'key', 'value', 'classTheme', true, 'type', 'defaultLabel', true);
        this.addDataToCardList(chartConfig, 5, ChartType.COLUMN, true, MetricsTableType.ModuleDetailsTable);
      } else {
        this.setLoadState(LoaderState.nocontent);
      }
    }, () => this.setLoadState(LoaderState.error));

    this.chartDataSubscriptions.push(aggregateData);
  }

  private fetchDataForJobStepCountChart(projectId: number): void {
    const request1: AggregationRequestModuleFieldName = {
      filterObject: {
        ...this.taxonomyFilter,
        'TYPE': {
          'in': [
            'EXEC',
            'EXEC_PGM'
          ]
        }
      },
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.CONTAINING_MODULE_ID]),
      fields: {
        ID: 'COUNT'
      }
    };

    const completeDataForAggregate = this.moduleService.getAggregatedValues2(projectId, request1).subscribe((results: AggregationResultModuleFieldName[]) => {
      if (results && results.length) {
        this.loadState = LoaderState.success;
        const chartData: ChartDataInterface[] = [
          { key: '1-25', value: 0 },
          { key: '26-50', value: 0 },
          { key: '51-75', value: 0 },
          { key: '76-100', value: 0 },
          { key: '>100', value: 0 },
        ];
        results.forEach((result) => {
          if (+result.fields.ID <= 25) {
            chartData[0].value++;
          } else if (+result.fields.ID > 25 && +result.fields.ID <= 50) {
            chartData[1].value++;
          } else if (+result.fields.ID > 50 && +result.fields.ID <= 75) {
            chartData[2].value++;
          } else if (+result.fields.ID > 75 && +result.fields.ID <= 100) {
            chartData[3].value++;
          } else {
            chartData[4].value++;
          }
        });
        const chartConfig = this.chartGlobalStyles.getColumnConfig(chartData, 'key', 'value', 'seqTheme', false, '', 'defaultLabel', false, false);
        this.addDataToCardList(chartConfig, 6, ChartType.COLUMN);
      } else {
        this.setLoadState(LoaderState.nocontent);
      }
    }, () => this.setLoadState(LoaderState.error));
    this.chartDataSubscriptions.push(completeDataForAggregate);
  }

  private fetchDataForHalsteadComplexityCharts(request: AggregationRequestStatementFieldName, projectId: number, cardIndex: number, fieldType: string): void {
    const sqlStatementsAggreValue = this.statementService.getStatementAggregatedValues(projectId, request)
      .subscribe((response: AggregationResultModuleFieldName[]) => {
        if (response.length) {
          this.loadState = LoaderState.success;
          const chartData = this.formatHalsteadComplexityChartData(response, fieldType);
          const chartConfig = this.chartGlobalStyles.getColumnConfig(chartData, 'key', 'value', 'semaTheme', false, '', 'defaultLabel', true);
          this.addDataToCardList(chartConfig, cardIndex, ChartType.COLUMN, true, MetricsTableType.SqlDetailsTable);
        } else {
          this.setLoadState(LoaderState.nocontent);
        }
      }, () => {
        this.setLoadState(LoaderState.error);
      });
    this.chartDataSubscriptions.push(sqlStatementsAggreValue);
  }

  private fetchDataForSQLStatementsDistributionChart(projectId: number): void {
    const request1: AggregationRequestStatementFieldName = {
      filterObject: {
        ...this.taxonomyFilter,
        [AggregationRequestStatementFieldName.GroupByEnum.TECHNOLOGY]: { 'eq': 'SQL' as any}
      },
      groupBy: new Set([AggregationRequestStatementFieldName.GroupByEnum.ID]),
      fields: {
        TEXT_LENGTH: 'SUM'
      }
    };

    const sqlDataDistributionChart = this.statementService.getStatementAggregatedValues(projectId, request1)
    .subscribe((results: AggregationResultModuleFieldName[]) => {
      if (results && results.length) {
        this.loadState = LoaderState.success;
        const chartData: ChartDataInterface[] = [
          { key: this.translationService.instant('module.lowComplexity'), value: 0 },
          { key: this.translationService.instant('module.mediumComplexity'), value: 0 },
          { key: this.translationService.instant('module.highComplexity'), value: 0 },
          { key: this.translationService.instant('module.veryHighComplexity'), value: 0 }
        ];
        results.forEach((result) => {
          if (+result.fields.TEXT_LENGTH > 0 && +result.fields.TEXT_LENGTH <= 400) {
            chartData[0].value++;
          } else if (+result.fields.TEXT_LENGTH > 400 && +result.fields.TEXT_LENGTH <= 800) {
            chartData[1].value++;
          } else if (+result.fields.TEXT_LENGTH > 800 && +result.fields.TEXT_LENGTH <= 1200) {
            chartData[2].value++;
          } else if (+result.fields.TEXT_LENGTH > 1200) {
            chartData[3].value++;
          }
        });
        const chartConfig = this.chartGlobalStyles.getColumnConfig(chartData, 'key', 'value', 'semaTheme', false, '', 'defaultLabel', true);
        this.addDataToCardList(chartConfig, 9, ChartType.COLUMN, true, MetricsTableType.SqlDetailsTable);
      } else {
        this.setLoadState(LoaderState.nocontent);
      }
    }, () => this.setLoadState(LoaderState.error));
    this.chartDataSubscriptions.push(sqlDataDistributionChart);
  }

  private formatComplexityChartData(data: AggregationResultModuleFieldName[], fieldType: string): ChartDataInterface[] {
    const respArr: ChartDataInterface[] = [];
    data.forEach((item: AggregationResultModuleFieldName) => {
      let key: string = null;
      let position: number;
      const complexity = +item.group[AggregationRequestModuleFieldName.GroupByEnum.COMPLEXITY];
      if (complexity >= 0 && complexity <= 10) {
        key = 'Low';
        position = 1;
      } else if (complexity > 10 && complexity <= 20) {
        key = 'Medium';
        position = 2;
      } else if (complexity > 20 && complexity <= 50) {
        key = 'High';
        position = 3;
      } else {
        key = 'Very High';
        position = 4;
      }

      if (key) {
        const record = respArr.find((record) => record['key'] === key);
        if (record) {
          record.value += +item.fields[fieldType];
        } else {
          respArr.push({ key, value: +item.fields[fieldType], position });
        }
      }
    });
    /* Sorting from low to high */
    respArr.sort((a, b) => a.position - b.position);
    return respArr;
  }

  private formatHalsteadComplexityChartData(data: AggregationResultModuleFieldName[], fieldType: string): ChartDataInterface[] {
    const respArr: Array<{ key: string, value: number }> = [];
    data.forEach((item: AggregationResultModuleFieldName) => {
      let key: string = null;
      const complexity = +item.group[AggregationRequestStatementFieldName.GroupByEnum.HALSTEAD_COMPLEXITY];
      if (complexity > 0 && complexity <= 10) {
        key = this.translationService.instant('module.lowComplexity');
      } else if (complexity > 10 && complexity <= 20) {
        key = this.translationService.instant('module.mediumComplexity');
      } else if (complexity > 20 && complexity <= 30) {
        key = this.translationService.instant('module.highComplexity');
      } else {
        key = this.translationService.instant('module.veryHighComplexity');
      }

      if (key) {
        const record = respArr.find((record) => record['key'] === key);
        if (record) {
          record.value += +item.fields[fieldType];
        } else {
          this.complexityArray.forEach(element => {
            if (key !== element) {
              respArr.push({ key: element, value: 0 });
            } else {
              respArr.push({ key, value: +item.fields[fieldType] });
            }
          });
        }
      }
    });
    return respArr;
  }

  private addDataToCardList(chartConfig: ChartDataInterface[] | ColumnOptions | LineOptions,
    cardIndex: number, chartType: ChartType, showChartDetails?: boolean, chartTable?: MetricsTableType): void {
    this.metricCardList.push({
      title: this.translationService.instant(`metrics.codeQuality.cardTitle${cardIndex}`),
      description: this.translationService.instant(`metrics.codeQuality.cardDescription${cardIndex}`),
      descriptionPosition: DescriptionPosition.Top,
      chartType,
      chartConfig,
      position: cardIndex,
      tableType: chartTable,
      chartFilterData: {
        showChartDetails: showChartDetails ? showChartDetails : false,
        tableType: chartTable,
        showExportButton: true,
        filterArgs: ['key', 'type', 'name', 'typeFilter'],
        queryFilterBuilder: (chartValues?: Record<string, string>, position?: number, tableType?: MetricsTableType):
          ChartFilters => this.generateFilterQuery(chartValues, position, tableType)
      }
    });
    this.metricCardList.sort((a, b) => a.position - b.position);
  }

  private setLoadState(state: LoaderState): void {
    if (this.loadState !== LoaderState.success) {
      this.loadState = state;
    }
  }

  private generateFilterQuery(chartValues: Record<string, string>, chartIndex: number, tableType?: MetricsTableType): ChartFilters {
    const filters = {};
    let filterObject = {};
    if (this.taxonomyTitle.length) {
      filters['taxonomy'] = this.taxonomyTitle;
    }
    if (tableType === MetricsTableType.SqlDetailsTable) {
      filterObject = this.getFilterObjectForSqlCharts(chartIndex, chartValues['key']);
      filters['Complexity'] = chartValues['key'];
    } else {
      const chartKey = (chartIndex === 5)? chartValues['typeFilter'] : chartValues['key'];
      const complexity = this.getFilterQueryByChartFilters(chartKey, chartValues['type']);
      const gqlTaxoFilter = { 'content_taxonomies_id': this.taxonomyFilter['TAXONOMY_ID'] };
      const gqlComplexityQuery = {
        content_sourceMetrics_codeLines: this.progComplexityQuery.LINES_OF_CODE,
        content_representation: this.progComplexityQuery.REPRESENTATION
      };
      switch (chartIndex) {
        case 1:
        case 2:
          filterObject = {
            ...gqlComplexityQuery,
            ...gqlTaxoFilter,
            ...this.getSelectedTechnology(chartValues['typeFilter']),
            ...complexity
          };
          if (chartValues['key']) {
            filters['Complexity'] = chartValues['key'];
            filters['Technology'] = chartValues['technology'];
          }
          break;
        case 3:
          const gqlScreenQuery = {
            content_sourceMetrics_codeLines: this.screenComplexityQuery.LINES_OF_CODE,
            content_sourceMetrics_complexityMcCabe: this.screenComplexityQuery.COMPLEXITY,
            content_type: this.screenComplexityQuery.TYPE
          };
          filterObject = {
            ...gqlScreenQuery,
            ...gqlTaxoFilter,
            ...complexity
          };
          if (chartValues['key']) {
            filters['Complexity'] = chartValues['key'];
          }
          break;
        case 4:
          filterObject = {
            ...gqlComplexityQuery,
            ...gqlTaxoFilter,
            ...complexity
          };
          if (chartValues['key']) {
            filters['Complexity'] = chartValues['key'];
          }
          break;
        case 5:
          if (chartValues['key']) {
            filterObject = {
              ...gqlTaxoFilter,
              ...complexity
            };
            filters['Technology'] = chartValues['key'];
            filters['category'] = chartValues['type'];
          } else {
            filterObject = {
              ...gqlTaxoFilter,
              ['content_sourceMetrics_codeLines']: { 'gte': 1 }
            };
          }
          break;
      }
    }
    return { filterObject, filters };
  }

  private getFilterQueryByChartFilters(complexityType?: string, type?: string): { [key: string]: object } {
    let filterObject = {};
    if (complexityType) {
      switch (complexityType.toLowerCase()) {
        case 'low':
          filterObject['content_sourceMetrics_complexityMcCabe'] = this.complexityRange[0]['COMPLEXITY'];
          break;
        case 'medium':
          filterObject['content_sourceMetrics_complexityMcCabe'] = this.complexityRange[1]['COMPLEXITY'];
          break;
        case 'high':
          filterObject['content_sourceMetrics_complexityMcCabe'] = this.complexityRange[2]['COMPLEXITY'];
          break;
        case 'very high':
          filterObject['content_sourceMetrics_complexityMcCabe'] = this.complexityRange[3]['COMPLEXITY'];
          break;
        default:
          if (type === this.translationService.instant('sourceLinesOfCode')) {
            filterObject = {
              ['content_sourceMetrics_codeLines']: { 'gte': 1 },
              ['content_technology']: { 'eq': complexityType }
            };
          } else {
            filterObject = {
              ['content_sourceMetrics_deadCodeLines']: { 'gte': 1 },
              ['content_technology']: { 'eq': complexityType }
            };
          }
          break;
      }
    }
    return filterObject;
  }

  private getFilterObjectForSqlCharts(chartIndex: number,  category: string): { [key: string]: object } {
    let filterObject = {'content_technology': { eq: 'SQL' as any}};
    let filter = '';
    if (chartIndex === 7 || chartIndex === 8) {
      filter = 'content_halsteadComplexity';
    } else {
      filter = 'content_textLength';
    }
    const ranges = {
      content_halsteadComplexity: {
        low: {'content_halsteadComplexity': {
          'gt': 0 as any,
          'lte': 10 as any
        }},
        medium: {'content_halsteadComplexity': {
          'gt': 10 as any,
          'lte': 20 as any
        }},
        high: {'content_halsteadComplexity': {
          'gt': 20 as any,
          'lte': 30 as any
        }},
        'very high': {'content_halsteadComplexity': {
          'gt': 30 as any
        }}
      },
      content_textLength: {
        low: {'content_textLength': {
          'gte': 1 as any,
          'lte': 400 as any
        }},
        medium: {'content_textLength': {
          'gte': 401 as any,
          'lte': 800 as any
        }},
        high: {'content_textLength': {
          'gte': 801 as any,
          'lte': 1200 as any
        }},
        'very high': {'content_textLength': {
          'gte': 1201 as any
        }}
      }
    };
    if (category) {
      filterObject = {...filterObject, ...ranges[filter][category.toLowerCase()]};
    } else {
      filterObject[filter] = { 'gte': 1 };
    }
    return filterObject;
  }

  private fetchProgramCycloMetricsComplexity(progCyclomaticAndLoc: Array<Observable<AggregationResultModuleFieldName[]>>,
    index: number, propertyDecider: string): void {
    const programCycloMetricComplexity = forkJoin(progCyclomaticAndLoc).subscribe((programComplexityWithRangeResp: AggregationResultModuleFieldName[][]) => {
      this.loadState = LoaderState.success;
      const formattedData: ProgramComplexity[] = this.formatComplexityWithRangeChartData(programComplexityWithRangeResp, propertyDecider);
      if (formattedData.length) {
        const sortedFormattedData: Array<{ key: string, value: number }> = sortArrayBasedOnKeyAndDirection(formattedData, 'technology', 'ASC', 'string');
        const chartConfig = this.chartGlobalStyles.getColumnConfig(sortedFormattedData, 'key', 'value', 'classTheme', true, 'technology'
          , 'defaultLabel', true, true);
        this.addDataToCardList(chartConfig, index, ChartType.COLUMN, true, MetricsTableType.ModuleDetailsTable);
      }
    });
    this.chartDataSubscriptions.push(programCycloMetricComplexity);
  }

  private formatComplexityWithRangeChartData(programComplexityWithRangeResp: AggregationResultModuleFieldName[][],
    propertyDecider: string): ProgramComplexity[] {
    const formattedComplexityData: ProgramComplexity[] = [];
    programComplexityWithRangeResp.forEach((programComplexity: AggregationResultModuleFieldName[], index: number) => {
      formattedComplexityData.push(...this.createProgramComplexityArray(programComplexity, propertyDecider, this.complexityArray[index]));
    });
    return formattedComplexityData;
  }

  private getSelectedTechnology(chartValues: string): { [key: string]: object } {
    return chartValues ? { ['content_technology']: { 'eq': chartValues }} : {};
  }

  private createProgramComplexityArray(programComplexity: AggregationResultModuleFieldName[], propertyDecider: string, key: string): ProgramComplexity[] {
    return programComplexity.map((programComplexityItem) => ({
      key,
      'technology': this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, programComplexityItem['group']['TECHNOLOGY'].toString()),
      'value': +programComplexityItem['fields'][propertyDecider],
      'typeFilter': programComplexityItem['group']['TECHNOLOGY'].toString()
    })
    );
  }

  private fetchDataForHalsteadComplexityDistinctCharts(projectId: number, cardIndex: number): void {
    const request1: AggregationRequestStatementFieldName = {
      filterObject: {
        'TECHNOLOGY': { eq: 'SQL' as any },
        'HALSTEAD_COMPLEXITY': {
          'gt': 0 as any,
          'lte': 10 as any
        }
      },
      fields: {
        DISTINCT_TABLES: 'AVG'
      }
    };
    const request2 = { ...request1 };
    request2.filterObject = {
      'TECHNOLOGY': { eq: 'SQL' as any },
      'HALSTEAD_COMPLEXITY': {
        'gt': 10 as any,
        'lte': 20 as any
      }
    };
    const request3 = { ...request1 };
    request3.filterObject = {
      'TECHNOLOGY': { eq: 'SQL' as any },
      'HALSTEAD_COMPLEXITY': {
        'gt': 20 as any,
        'lte': 30 as any
      }
    };
    const request4 = { ...request1 };
    request4.filterObject = {
      'TECHNOLOGY': { eq: 'SQL' as any },
      'HALSTEAD_COMPLEXITY': {
        'gt': 30 as any
      }
    };
    const sqlStatementsAggreValue = forkJoin([
      this.statementService.getStatementAggregatedValues(projectId, request1),
      this.statementService.getStatementAggregatedValues(projectId, request2),
      this.statementService.getStatementAggregatedValues(projectId, request3),
      this.statementService.getStatementAggregatedValues(projectId, request4)
    ]).subscribe((response: AggregationResultStatementFieldName[][]) => {
      const arrayLengthSum = response.reduce((sum: number, result: AggregationResultStatementFieldName[]) => sum + result.length, 0);
      if (arrayLengthSum) {
        this.loadState = LoaderState.success;
        const chartData: ChartDataInterface[] = [
          { key: 'Low', value: +response[0][0]?.fields.DISTINCT_TABLES || 0 },
          { key: 'Medium', value: +response[1][0]?.fields.DISTINCT_TABLES || 0 },
          { key: 'High', value: +response[2][0]?.fields.DISTINCT_TABLES || 0 },
          { key: 'Very High', value: +response[3][0]?.fields.DISTINCT_TABLES || 0 }
        ];
        const chartConfig = this.chartGlobalStyles.getColumnConfig(chartData, 'key', 'value', 'semaTheme', false, '', 'defaultLabel', true);
        this.addDataToCardList(chartConfig, cardIndex, ChartType.COLUMN, true, MetricsTableType.SqlDetailsTable);
      } else {
        this.setLoadState(LoaderState.nocontent);
      }
    }, () => {
      this.setLoadState(LoaderState.error);
    });
    this.chartDataSubscriptions.push(sqlStatementsAggreValue);
  }
}
