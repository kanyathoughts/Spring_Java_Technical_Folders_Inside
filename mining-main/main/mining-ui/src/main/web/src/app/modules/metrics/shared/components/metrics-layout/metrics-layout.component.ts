import { Component, Input, OnChanges, OnDestroy } from '@angular/core';
import { Logger } from '@app/core';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { dateFormatter } from '@app/core/utils/date-formatter.utils';
import { graphQlQuery } from '@app/core/utils/graphql.util';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { MiningTableConfig } from '@app/shared/components/mining-table/mining-table-config.interface';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateService } from '@ngx-translate/core';
import { NzTableQueryParams } from 'ng-zorro-antd/table';
import { map } from 'rxjs/operators';
import { ChartDetailConfigService } from '../../../../../core/services/chart-details-config.service';
import { ChartFilters, IMSFilterData, MetricsCard, MetricsTableType } from '../metrics-card/metrics-card.interface';
import { getBasePath } from '@app/core/utils/base-path.utils';
import { HttpClient } from '@angular/common/http';
import { ModuleMetricsDetailsService } from '@app/core/services/module-details/module-metrics-details.service';
import { Observable, Subscription } from 'rxjs';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { ActivatedRoute, Params, Router } from '@angular/router';
import {
  AggregationRequestModuleFieldName,
  AggregationResultModuleFieldName, ModuleControllerService,
  ModulePojo,
} from '@innowake/mining-api-angular-client';
import { ImsDetailsGQL, ModulesGQL } from '@app/graphql/generated/generated';

const log = new Logger('BrowseMiningModulesComponent');

@Component({
  selector: 'mn-metrics-layout',
  templateUrl: './metrics-layout.component.html'
})
export class MetricsLayoutComponent implements OnChanges, OnDestroy {
  @Input() title: string;
  @Input() projectId: number;
  @Input() metricCardList: MetricsCard[];
  @Input() loadState: LoaderState;
  @Input() showTaxonomyFilter = true;
  showChartDetails = false;
  pageIndex = 1;
  totalRecords: number;
  chartDataValue: any[];
  selectedCard: MetricsCard;
  filterData: ChartFilters;
  chartTableConfig: MiningTableConfig;
  private currentFilter: Record<string, string>;
  private routeSubscription: Subscription;

  constructor(
    private modulesGQL: ModulesGQL,
    private translateService: TranslateService,
    private numberFormatter: NumberFormatter,
    private moduleController: ModuleControllerService,
    private graphQlControllerService: GraphQlControllerService,
    private http: HttpClient,
    private moduleMetricsDetailsService: ModuleMetricsDetailsService,
    private chartDetailConfig: ChartDetailConfigService,
    private imsCharDetailsGql: ImsDetailsGQL,
    private jobManager: JobManagerService,
    public route: ActivatedRoute,
    public router: Router
  ) {}

  ngOnChanges(): void {
    if (this.routeSubscription) {
      this.routeSubscription.unsubscribe();
    }
    // Fetches chart details based on URL queryparams.
    this.routeSubscription = this.route.queryParams.subscribe((param) => {
      if (Object.keys(param).length) {
        this.openChartDetailsTable((param['filter'] ? JSON.parse(param['filter'] as string) : {}) as Record<string, string>, param.index as number, false);
      } else {
        this.showChartDetails = false;
      }
    });
  }

  /**
   * Opens the detail table in the drawer for the specific card.
   *
   * @param filter filter string used to fetch the data from the server.
   * @param index index of the card in the card list.
   */
  openChartDetailsTable(filter: Record<string, string>, index: number, navigatetoURL: boolean = true): void {
    if (this.metricCardList[index]?.chartFilterData && this.metricCardList[index].chartFilterData.showChartDetails) {
      this.showChartDetails = true;
      this.selectedCard = this.metricCardList[index];
      this.currentFilter = filter;
      this.filterData = this.selectedCard.chartFilterData.queryFilterBuilder(this.currentFilter, this.selectedCard.position, this.selectedCard.tableType);
      this.chartTableConfig = this.chartDetailConfig.getChartDataConfigByTableType(this.selectedCard.chartFilterData.tableType, this.projectId);
      this.pageIndex = 1;
      // When filter fetched from URL, navigation to the same URL is not required.
      if (navigatetoURL) {
        const queryParams = {};
        queryParams['index'] = index;
        if (filter && Object.keys(filter).length) {
          queryParams['filter'] = JSON.stringify(filter);
        }
        this.navigateToMetricsURL(queryParams);
      }
      this.fetchChartData(this.pageIndex, this.chartTableConfig.rows);
    }
  }

  /**
   * Makes appropriate server call to get the paginated data.
   *
   * @param event consists of changes in sorting, filtering and navigation pages(currently only using this).
   */
  onQueryParamChange(event: NzTableQueryParams): void {
    if (this.pageIndex !== event.pageIndex) {
      this.pageIndex = event.pageIndex;
      this.fetchChartData(this.pageIndex, event.pageSize);
    }
  }

  /**
   * Closes the cart detail drawer.
   */
  closeChartDetails(): void {
    this.showChartDetails = false;
    this.navigateToMetricsURL();
  }

  /**
   * Navigates to the module detail page.
   *
   * @param module The module data to navigate to detail page.
   * @returns link for detail page.
   */
  navigateToDetails(module: ModulePojo): string {
    return RouteBuilder.buildModuleRoute(this.projectId, module.linkHash, 'details/overview');
  }

  /**
   * Navigates to the metrics URL.
   * @param queryParams query params to be added to URL.
   */
  navigateToMetricsURL(queryParams?: Params): void {
    this.router.navigate(
      [],
      {
        relativeTo: this.route,
        queryParams
      }
    ).catch(() => { });
  }

  /**
   * Method is for Sorting the filters in table data.
   * We are disabling the default feature of sort but will make it enable if required.
   *
   * @returns 0 as it means to be disable.
   */
  filterSort(): number {
    return 0;
  }

  /**
   * Export data to a csv file format.
   */
  exportMetricsDetails(): void {
    this.jobManager.invokeExportJob('datapoint-csv', this.buildParams(), this.projectId);
  }

  ngOnDestroy(): void {
    this.routeSubscription.unsubscribe();
  }

  private fetchChartData(pageIndex: number, pageSize: number): void {
    this.chartTableConfig.loading = true;
    this.chartDataValue = [];
    /*
      As currently we have only one table so returning the object as is, But in future we will have
      different table type and the formattedData object should be modified accordingly then.
    */
    switch (this.selectedCard.chartFilterData.tableType) {
      case MetricsTableType.ModuleDetailsTable:
        this.modulesGQL.fetch({ projectId: this.projectId, size: pageSize, page: pageIndex - 1, filterObject: this.filterData.filterObject },
          {
            fetchPolicy: 'network-only'
          })
          .subscribe((response) => {
            this.chartTableConfig.loading = false;
            this.totalRecords = response.data.modules.totalElements;
            this.chartDataValue = this.getFormattedDataForModuleDetailsTable(response.data.modules.content);
          });
        break;
      case MetricsTableType.SqlDetailsTable:
        this.setSQLTableData(pageIndex, pageSize);
        break;
      case MetricsTableType.UtilitiesTable:
        this.setUtilityTableData();
        break;
      case MetricsTableType.InterfacesTable:
        this.setInterfaceTableData();
        break;
      case MetricsTableType.IMSTable:
        this.setIMSTableData(pageIndex, pageSize);
        break;
    }
  }

  private setIMSTableData(pageIndex: number, pageSize: number): void {
    const outReferenceFilter = this.filterData.filterObject;
    this.imsCharDetailsGql.fetch({
      projectId: this.projectId,
      propertiesFilter: outReferenceFilter.properties,
      taxonomyFilter: outReferenceFilter.taxonomies
    }).subscribe((response) => {
      this.chartTableConfig.loading = false;
      if (response) {
        const imsData = this.createIMSTableData(response);
        this.totalRecords = imsData.length;
        this.chartDataValue = this.paginator(imsData, pageIndex, pageSize);
      }
    }, (error) => {
      log.error(error);
      this.chartTableConfig.loading = false;
    });
  }

  private setSQLTableData(pageIndex: number, pageSize: number): void {
    const content = [
      { name: 'id', path: 'content.module.id' },
      { name: 'name', path: 'content.module.name' },
      { name: 'type', path: 'content.type' },
      { name: 'sqlLength', path: 'content.sqlLength' },
      { name: 'tables', path: 'content.tables' },
      { name: 'distinctTables', path: 'content.distinctTables' },
      { name: 'customComplexity', path: 'content.customComplexity' },
      { name: 'halsteadComplexity', path: 'content.halsteadComplexity' },
      { name: 'halsteadDifficulty', path: 'content.halsteadDifficulty' },
      { name: 'text', path: 'content.text' },
      { name: 'textLength', path: 'content.textLength' }
    ];
    const requestQuery: { [key: string]: any } = {
      'query': graphQlQuery(
        'statements',
        {'projectId': this.projectId, 'filterObject': '$filter', page: pageIndex - 1, size: pageSize},
        content,
        ['totalElements', 'size'],
        true
      ),
      'variables': {
        filter: this.filterData.filterObject
      }
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      this.chartTableConfig.loading = false;
      if (response) {
        const records = response.data.statements;
        this.totalRecords = records.totalElements;
        this.chartDataValue = this.getFormattedDataForSQLTable(records.content as any[]);
      }
    }, (error) => {
      log.error(error);
      this.chartTableConfig.loading = false;
    });
  }

  private getFormattedDataForSQLTable(data: any[]): any[] {
    const tableData: Array<Record<string, string | number>> = [];
    data.forEach((dataItem: any, index) => {
      tableData.push({
        'id': index,
        'moduleId': dataItem.module.id,
        'name': dataItem.module.name,
        'statementTypeLink': dataItem.type,
        'sqlLength': dataItem.sqlLength,
        'tables': dataItem.tables,
        'distinctTables': dataItem.distinctTables,
        'customComplexity': dataItem.customComplexity,
        'halsteadComplexity': this.numberFormatter.transform(dataItem.halsteadComplexity as string),
        'halsteadDifficulty': this.numberFormatter.transform(dataItem.halsteadDifficulty as string),
        'text': dataItem.text,
        'textLength': dataItem.textLength
      });
    });
    return tableData;
  }

  private getFormattedDataForModuleDetailsTable(data: any[]): any[] {
    const notAvailable = this.translateService.instant('notAvailable');
    return data.map((val) => {
      const formattedVal = {...val};
      formattedVal.complexityLevel = this.getComplexityDescription(val.complexityLevel as string);
      formattedVal.metricsDate = val.metricsDate ? dateFormatter(val.metricsDate as Date) : notAvailable;
      if (val.sourceMetrics == null) {
        formattedVal.sourceMetrics = { codeLines: notAvailable, commentLines: notAvailable };
      } else {
        formattedVal.sourceMetrics = {
          codeLines: this.moduleMetricsDetailsService.getSourceMetricsDetails(+formattedVal.sourceMetrics?.codeLines),
          commentLines: this.moduleMetricsDetailsService.getSourceMetricsDetails(+formattedVal.sourceMetrics?.commentLines)
        };
      }
      return formattedVal;
    });
  }

  private setUtilityTableData(): void {
    this.chartTableConfig.isExportVisible = true;
    const filterObject = this.filterData.filterObject;
    const requestCategoryAndInvocations: AggregationRequestModuleFieldName = {
      filterObject,
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.NAME, AggregationRequestModuleFieldName.GroupByEnum.CATEGORIES]),
      fields: {
        [AggregationRequestModuleFieldName.GroupByEnum.ID]: 'COUNT'
      },
      csvHeaders: {
        [AggregationRequestModuleFieldName.GroupByEnum.CATEGORIES]: this.translateService.instant('category'),
        [AggregationRequestModuleFieldName.GroupByEnum.NAME]: this.translateService.instant('utilityName'),
        [AggregationRequestModuleFieldName.GroupByEnum.ID]: this.translateService.instant('numberOfInvocations')
      }
    };
    this.moduleController.getAggregatedUtilityValues(this.projectId, requestCategoryAndInvocations)
      .subscribe((results: AggregationResultModuleFieldName[]) => {
        if (results && results.length) {
          this.chartTableConfig.loading = false;
          const tableData: Array<Record<string, string | number>> = results.map(
            (aggreg, index) => ({
              utilityName: aggreg.group[AggregationRequestModuleFieldName.GroupByEnum.NAME] + '',
              numberOfInvocations: this.numberFormatter.transform(`${aggreg.fields[AggregationRequestModuleFieldName.GroupByEnum.ID]}`),
              category: aggreg.group[AggregationRequestModuleFieldName.GroupByEnum.CATEGORIES] + '',
              id: index
            })
          );
          this.chartDataValue = this.getFormattedDataForUtilityTable(tableData);
          this.totalRecords = this.chartDataValue.length;
        }
      });
    this.chartTableConfig.externalExportCallback = () => this.buildCSVRequest(requestCategoryAndInvocations);
  }

  private getFormattedDataForUtilityTable(data: Array<Record<string, string | number>>): any[] {
    const categoryColumn = {
      category: {
        field: 'category',
        header: 'category',
      }
    };
    const updatedUtilitiesTableConfig = { ...this.chartTableConfig };
    let formattedData = data;
    if (this.currentFilter['key']) {
      formattedData = this.selectedCard.chartFilterData.dataFilter(this.currentFilter['key'], data);
    } else {
      updatedUtilitiesTableConfig.columnMap = { ...this.chartTableConfig.columnMap, ...categoryColumn };
      this.chartTableConfig = updatedUtilitiesTableConfig;
    }
    return formattedData;
  }

  private getComplexityDescription(complexity: string): string {
    let description = 'module.veryHighComplexity';
    if (complexity === 'UNKNOWN') {
      description = 'module.unknownComplexity';
    } else if (complexity === 'LOW') {
      description = 'module.lowComplexity';
    } else if (complexity === 'MEDIUM') {
      description = 'module.mediumComplexity';
    } else if (complexity === 'HIGH') {
      description = 'module.highComplexity';
    }
    return this.translateService.instant(description);
  }

  private setInterfaceTableData(): void {
    this.chartTableConfig.isExportVisible = true;
    const filterObject = this.filterData.filterObject;
    const requestInterface: AggregationRequestModuleFieldName = {
      filterObject,
      groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.NAME, AggregationRequestModuleFieldName.GroupByEnum.INTERFACE]),
      fields: {
        [AggregationRequestModuleFieldName.GroupByEnum.INBOUND]: 'SUM',
        [AggregationRequestModuleFieldName.GroupByEnum.OUTBOUND]: 'SUM'
      }
    };
    this.moduleController.getAggregatedUtilityValues(this.projectId, requestInterface)
      .pipe(map((result: AggregationResultModuleFieldName[]) => result.filter(val => val.fields.INBOUND as any !== 0 || val.fields.OUTBOUND as any !== 0)))
      .subscribe((results: AggregationResultModuleFieldName[]) => {
        this.chartTableConfig.loading = false;
        if (results && results.length) {
          this.chartDataValue = this.getFormattedDataForInterfaceTable(results);
          this.totalRecords = this.chartDataValue.length;
        }
      });
      this.chartTableConfig.externalExportCallback = () => this.buildCSVRequest(requestInterface);
  }

  private getFormattedDataForInterfaceTable(data: any): any {
    const tableData: Array<{ 'Protocol': string; 'Inbound': string; 'Outbound': string; 'id': number }> = [];
    let filterData: AggregationResultModuleFieldName[] = data;
    if (this.currentFilter['key']) {
      filterData = this.selectedCard.chartFilterData.dataFilter(this.currentFilter['key'], data as Array<Record<string, string | number>>);
    }
    filterData.forEach((dataItem: any, index: number) => {
      tableData.push({
        'Protocol': dataItem.group[AggregationRequestModuleFieldName.GroupByEnum.NAME],
        'Inbound': dataItem.fields[AggregationRequestModuleFieldName.GroupByEnum.INBOUND],
        'Outbound': dataItem.fields[AggregationRequestModuleFieldName.GroupByEnum.OUTBOUND],
        'id': index
      });
    });
    return tableData;
  }

  private createIMSTableData(response: any): IMSFilterData[] {
    const responseData: any[] = response?.data?.modules?.content ?? [];
    const tableData: any[] = responseData.flatMap((element) => element.dependencies?.map((reference: any): any => ({
      'moduleId': element.id,
      'moduleName': element.name,
      'method': reference.properties['DB_ACCESS_OPERATION'],
      'databaseName': reference.module.name,
      'segmentName': reference.properties['IMS_SEGMENTS']?.slice(1, -1),
      'statement': reference.properties['STATEMENT']
    }) ?? []));
    tableData.forEach((element, index) => {
      element.id = index + 1;
    });

    return tableData;
  }

  private paginator(items: any, pageIndex: number, pageSize: number): any[] {
    return items.slice((pageIndex - 1) * pageSize).slice(0, pageSize);
  }

  private buildParams(): object {
    const params = {};
    switch (this.selectedCard.chartFilterData.tableType) {
      case MetricsTableType.ModuleDetailsTable:
        params['$query'] = 'modules';
        params['filterObject'] = JSON.stringify(this.filterData.filterObject);
        break;
      case MetricsTableType.SqlDetailsTable:
        params['$query'] = 'statements';
        params['filterObject'] = JSON.stringify(this.filterData.filterObject);
        break;
      case MetricsTableType.IMSTable:
        const filter = {
          content_taxonomies_id: this.filterData.filterObject.taxonomies,
          content_technology: { notEq: 'IMS' },
          content_storage: { eq: 'FILE' },
          content_outReferencesTechnology: { eq: 'IMS' },
          content_dependencyCount: { gte: 1 }
        };
        params['$unroll'] = 'true';
        params['$query'] = 'modules';
        params['filterObject'] = JSON.stringify(filter);
        if (this.filterData.filterObject) {
          const dependenciesFilter = {
            content_technology: {eq: 'IMS'},
            content_type: {eq: 'DBD'}
          };
          params['content.dependencies.filterObject'] = JSON.stringify(dependenciesFilter);
          params['content.dependencies.direction'] = 'OUT';
          params['content.dependencies.properties'] = JSON.stringify(this.filterData.filterObject.properties);
        }
        break;
    }
    Object.keys(this.chartTableConfig.columnMap).forEach((element: any) => {
      if (typeof params['$columns'] == 'undefined') {
        params['$columns'] = [];
      }
      params['$columns'].push(this.chartTableConfig.columnMap[element].path);
    });
    return params;
  }

  private buildCSVRequest(requestParam: AggregationRequestModuleFieldName): Observable<any> {
    const url = getBasePath() + '/api/v1/projects/' + this.projectId + '/modules/utility-aggregations/csv';
    return this.http.post(url, requestParam, { observe: 'response', responseType: 'blob' });
  }
}
