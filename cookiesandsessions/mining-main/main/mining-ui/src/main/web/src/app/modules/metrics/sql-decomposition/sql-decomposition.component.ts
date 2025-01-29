import { Component, OnDestroy, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';
import { ChartDataInterface, ChartFilters, ChartType, MetricsCard, MetricsTableType } from '../shared/components/metrics-card/metrics-card.interface';
import { ChartGlobalStyles } from '../shared/utils/chart-global-styles.utils';
import { MetricsFilterService } from '@app/core/services/metrics-filter.service';
import { TaxonomyDetails } from '@app/shared/interfaces/taxonomy-list.interface';
import { getTaxonomyIdsTitle } from '@app/core/utils/taxonomy.utils';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import {
  AggregationRequestStatementFieldName,
  AggregationResultStatementFieldName,
  StatementControllerService
} from '@innowake/mining-api-angular-client';

const othersSqlStatementQuery = { 'notIn': ['ALTER*', 'CREATE*', 'DROP*', 'GRANT*', 'INSERT', 'UPDATE', 'DELETE', 'MERGE', 'SELECT']};

@Component({
  selector: 'app-sql-decomposition',
  templateUrl: './sql-decomposition.component.html',
})


export class SqlDecompositionComponent implements OnInit, OnDestroy {
  metricCardList: MetricsCard[] = [];
  sqlTypeMap = new Map<string, number>();

  dmlCount = 0;
  ddlCount = 0;
  queryCount = 0;
  errorCount = 0;
  otherCount = 0;

  loadState: LoaderState;
  clientProjectSubscription: Subscription;
  metricsFilterSubscription: Subscription;
  combinedSubscription: Subscription[] = [];
  chartDataSubscriptions: Subscription[] = [];
  projectId: number;
  filterIds: number[];
  taxonomyTitles: string[];

  constructor(
    private statementControllerService: StatementControllerService,
    private translationService: TranslateService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private metricsFilterService: MetricsFilterService,
    private chartGlobalStyles: ChartGlobalStyles) { }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.metricsFilterSubscription = this.metricsFilterService.getMetricsTaxonomyFilter().subscribe((filterDetail: TaxonomyFilterSelected[]) => {
        const filterDetails: TaxonomyDetails = getTaxonomyIdsTitle(filterDetail);
        const baseFilter = { 'TECHNOLOGY': { eq: 'SQL' as any }};
        if (filterDetails.taxonomyIds.length) {
          baseFilter['TAXONOMY_ID'] = { 'in': filterDetails.taxonomyIds };
        }
        this.filterIds = filterDetails.taxonomyIds;
        this.taxonomyTitles = filterDetails.taxonomyTitles;
        const allSqlStmtsRequest: AggregationRequestStatementFieldName = {
          filterObject: baseFilter,
          groupBy: new Set([AggregationRequestStatementFieldName.GroupByEnum.STATEMENT_TYPE]),
          fields: {
            ID: 'COUNT',
          },
        };
        const dmlStmtsRequest: AggregationRequestStatementFieldName = {
          filterObject: {
            ...baseFilter,
            'STATEMENT_TYPE': { 'in': ['INSERT', 'UPDATE', 'DELETE', 'MERGE']}
          },
          groupBy: new Set([AggregationRequestStatementFieldName.GroupByEnum.STATEMENT_TYPE]),
          fields: {
            ID: 'COUNT',
          },
        };
        const ddlStmtsRequest: AggregationRequestStatementFieldName = {
          filterObject: {
            ...baseFilter,
            'STATEMENT_TYPE': { 'in': ['ALTER*', 'CREATE*', 'DROP*', 'GRANT*']}
          },
          groupBy: new Set([AggregationRequestStatementFieldName.GroupByEnum.STATEMENT_TYPE]),
          fields: {
            ID: 'COUNT',
          },
        };
        const otherSqlStmtsRequest: AggregationRequestStatementFieldName = {
          filterObject: {
            ...baseFilter,
            ...{ 'STATEMENT_TYPE': othersSqlStatementQuery }
          },
          groupBy: new Set([AggregationRequestStatementFieldName.GroupByEnum.STATEMENT_TYPE]),
          fields: {
            ID: 'COUNT',
          },
        };
        this.metricCardList.length = 0;
        this.chartDataSubscriptions?.forEach((chartDataSubscription: Subscription) => chartDataSubscription.unsubscribe());
        this.chartDataSubscriptions.length = 0;
        this.resetCounters();
        this.loadState = LoaderState.loading;
        this.projectId = response.getProjectId();
        this.fetchSqlStatementsToDisplay(response.getProjectId(), allSqlStmtsRequest, ddlStmtsRequest, dmlStmtsRequest, otherSqlStmtsRequest);
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

  /**
   * Method to reset all the counters on updating filter
   */
  private resetCounters(): void {
    this.dmlCount = 0;
    this.ddlCount = 0;
    this.queryCount = 0;
    this.errorCount = 0;
    this.otherCount = 0;
  }

  /**
   * Service calls to fetch the sql decomposition response
   *
   * @param projectId
   * @param allSqlStmtsRequest request to display in card 1 ALL SQL statements
   * @param ddlStmtsRequest request to display in card 2 DDL SQL statements
   * @param dmlStmtsRequest request to display in card 3 DML SQL statements
   * @param otherSqlStmtsRequest request to display in card 4 Other SQL statements
   */
  private fetchSqlStatementsToDisplay(
    projectId: number,
    allSqlStmtsRequest: AggregationRequestStatementFieldName,
    ddlStmtsRequest: AggregationRequestStatementFieldName,
    dmlStmtsRequest: AggregationRequestStatementFieldName,
    otherSqlStmtsRequest: AggregationRequestStatementFieldName
  ): void {
    // For Card 1 - ALL SQL statements
    const statementAgregateValue = this.statementControllerService.getStatementAggregatedValues(projectId, allSqlStmtsRequest)
      .subscribe((response: AggregationResultStatementFieldName[]) => {
        if (response && response.length) {
          this.resetCounters();
          const chartData: ChartDataInterface[] = this.transformResponseToChartDataForAllSql(response);
          this.pushSqlStatementsToChart(chartData, 'metrics.sqlDecomposition.card1Title', 'classTheme', 0);
          /* Evaluating the load state here on the first card as the filter is empty in this request,
          so if we don't get any response for this card. Then it means that we won't get any response for any other cards */
          this.loadState = LoaderState.success;
        } else {
          this.loadState = LoaderState.nocontent;
        }
      });
    this.chartDataSubscriptions.push(statementAgregateValue);
    // For Card 2 - DML SQL statements
    const sqlStatementAggregate = this.statementControllerService.getStatementAggregatedValues(projectId, dmlStmtsRequest)
      .subscribe((response: AggregationResultStatementFieldName[]) => {
        if (response && response.length) {
          const chartData: ChartDataInterface[] = this.transformResponseToChartData(response);
          this.pushSqlStatementsToChart(chartData, 'metrics.sqlDecomposition.card2Title', 'seqTheme', 1);
        }
      });
    this.chartDataSubscriptions.push(sqlStatementAggregate);

    // For Card 3 - DDL SQL statements
    const sqlAggregateData = this.statementControllerService.getStatementAggregatedValues(projectId, ddlStmtsRequest)
      .subscribe((response: AggregationResultStatementFieldName[]) => {
        if (response && response.length) {
          const chartData: ChartDataInterface[] = this.transformResponseToChartData(response);
          this.pushSqlStatementsToChart(chartData, 'metrics.sqlDecomposition.card3Title', 'seqTheme', 2);
        }
      });
    this.chartDataSubscriptions.push(sqlAggregateData);

    // For Card 4 - Other SQL statements
    const aggregateData = this.statementControllerService.getStatementAggregatedValues(projectId, otherSqlStmtsRequest)
      .subscribe((response: AggregationResultStatementFieldName[]) => {
        if (response && response.length) {
          const chartData: ChartDataInterface[] = this.transformResponseToChartData(response);
          this.pushSqlStatementsToChart(chartData, 'metrics.sqlDecomposition.card4Title', 'seqTheme', 3);
        }
      });
    this.chartDataSubscriptions.push(aggregateData);
  }

  /**
   * To push the data to the metric chart
   *
   * @param chartData To display the data on the respective charts
   * @param title Title for the respective metric cards
   * @param index To maintain consistency of displaying metric cards
   */
  private pushSqlStatementsToChart(chartData: ChartDataInterface[], title: string, chartTheme: string, index: number): void {
    this.metricCardList.push({
      title: this.translationService.instant(title),
      chartType: ChartType.PIE,
      chartConfig: this.chartGlobalStyles.getPieConfig(chartData, 'value', 'key', chartTheme, true),
      position: index,
      chartFilterData: {
        showChartDetails: true,
        showExportButton: true,
        tableType: MetricsTableType.SqlDetailsTable,
        filterArgs: ['key'],
        queryFilterBuilder: (chartValues?: Record<string,string>, position?: number): ChartFilters => this.generateFilterQuery(chartValues, position),
      }
    });
    // adding sorting logic to maintain the consistency in ordering of the cards
    this.metricCardList.sort((a, b) => a.position - b.position);
  }

  /**
   * To transform the response of the calls with filter to chart data
   * @param response For the cards, the backend response
   * @returns Data to pass to display on the chart
   */
  private transformResponseToChartData(response: AggregationResultStatementFieldName[]): ChartDataInterface[] {
    const chartData: ChartDataInterface[] = [];
    response.forEach((aggreg: AggregationResultStatementFieldName) => {
      if (aggreg.group[AggregationRequestStatementFieldName.GroupByEnum.STATEMENT_TYPE]) {
        chartData.push({
          key: aggreg.group[AggregationRequestStatementFieldName.GroupByEnum.STATEMENT_TYPE] as any,
          value: aggreg.fields['ID'] as any,
        });
      }
    });

    return chartData;
  }

  /**
   * To transform the response for the call without filter to chart data
   * @param response For All SQL Card, the response from backend
   * @returns Data to pass to display on chart for All SQL card
   */
  private transformResponseToChartDataForAllSql(response: AggregationResultStatementFieldName[]): ChartDataInterface[] {
    this.sqlTypeMap = new Map<string, number>();
    response.forEach((response) => {
      this.groupBasedOnSqlTypes(response);
    });

    const chartDataForAllSql: ChartDataInterface[] = [];
    for (const [sqlType, count] of this.sqlTypeMap) {
      chartDataForAllSql.push({
        key: sqlType,
        value: count,
      });
    }
    return chartDataForAllSql;
  }

  /**
   * Grouping the statement types based on sql types
   *
   * @param result Segregating SQL statements
   */
  private groupBasedOnSqlTypes(result: AggregationResultStatementFieldName): void {
    const sqlType: string = result.group[AggregationRequestStatementFieldName.GroupByEnum.STATEMENT_TYPE] as any;
    const count = result.fields['ID'] as any;
    if (sqlType) {
      switch (sqlType) {
        case 'INSERT':
        case 'DELETE':
        case 'UPDATE':
        case 'MERGE':
          this.dmlCount += count;
          this.sqlTypeMap.set('DML', this.dmlCount);
          break;
        case 'SELECT':
          this.queryCount += count;
          this.sqlTypeMap.set('QUERY', this.queryCount);
          break;
        case 'ERROR':
          this.errorCount += count;
          this.sqlTypeMap.set('ERROR', this.errorCount);
          break;
        default:
          if (this.isDDL(sqlType)) {
            this.ddlCount += count;
            this.sqlTypeMap.set('DDL', this.ddlCount);
          } else {
            this.otherCount += count;
            this.sqlTypeMap.set('OTHER', this.otherCount);
          }
      }
    }
  }

  /**
   * To check whether the Sql type is DDL statement
   *
   * @param sqlType sql statement to check if its a DDL statement
   * @returns true if the sqlType is starts with any of DDL statements
   */
  private isDDL(sqlType: string): boolean {
    return (
      sqlType.startsWith('ALTER') ||
      sqlType.startsWith('CREATE') ||
      sqlType.startsWith('DROP') ||
      sqlType.startsWith('GRANT')
    );
  }

  private generateFilterQuery(chartValues: Record<string, string>, position: number): ChartFilters {
    const filters = {};
    const filterObject: {[key: string]: {[key: string]: object}}  = {'content_technology': { eq: 'SQL' as any}};
    if (this.filterIds.length) {
      filters['taxonomy'] = this.taxonomyTitles;
      filterObject['content_taxonomy'] = { 'in': this.filterIds };
    }
    if (chartValues['key']) {
      position === 0 ? filters['setOfStatements'] = chartValues['key'] : filters['statement'] = chartValues['key'];
    }
    if (chartValues.key && chartValues.key !== 'OTHER') {
      filterObject['content_type'] = { 'in': this.setQueryTypes(chartValues.key) };
    } else if (chartValues.key === 'OTHER') {
      filterObject['content_type'] = othersSqlStatementQuery;
    } else {
      this.metricCardList.forEach((cardList) => {
        const queryKeys: string[] = [];
        if (position === cardList.position && cardList.title !== this.translationService.instant('metrics.sqlDecomposition.card1Title')) {
          cardList.chartConfig['data'].forEach((data: { key: string; }) => {
            queryKeys.push(...this.setQueryTypes(data.key));
          });
          filterObject['content_type'] = { 'in': queryKeys };
        }
      });
    }
    return { filterObject, filters };
  }

  private setQueryTypes(chartValues: string): string[] {
    switch (chartValues) {
      case 'DML':
        return ['INSERT','UPDATE','DELETE','MERGE'];
      case 'QUERY':
        return ['SELECT'];
      case'DDL':
        return ['ALTER_INDEX','ALTER_CONSTRAINT','ALTER_CHECK_CONSTRAINT','ALTER_FUNCTION','ALTER_TABLE','ALTER_PROCEDURE','ALTER_VIEW','ALTER_PRIMARY_KEY',
              'ALTER_FOREIGN_KEY','CREATE_CONSTRAINT','CREATE_CHECK_CONSTRAINT','CREATE_FUNCTION','CREATE_TABLE','CREATE_PROCEDURE','CREATE_VIEW',
              'CREATE_PRIMARY_KEY','CREATE_FOREIGN_KEY','CREATE_INDEX','DROP_FOREIGN_KEY','DROP_INDEX','DROP_CONSTRAINT','DROP_CHECK_CONSTRAINT',
              'DROP_FUNCTION','DROP_TABLE','DROP_PROCEDURE','DROP_VIEW','DROP_PRIMARY_KEY','GRANT'];
      default:
        return [chartValues];
    }
  }
}
