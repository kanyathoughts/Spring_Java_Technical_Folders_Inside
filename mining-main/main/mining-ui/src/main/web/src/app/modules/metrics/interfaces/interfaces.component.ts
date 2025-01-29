import { Component, OnDestroy, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';
import { ChartDataInterface, ChartFilters, ChartType, DescriptionPosition, MetricsCard, MetricsTableType
} from '../shared/components/metrics-card/metrics-card.interface';
import { ChartGlobalStyles } from '../shared/utils/chart-global-styles.utils';
import { AggregationRequestModuleFieldName, AggregationResultModuleFieldName, ModuleControllerService } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-interfaces',
  templateUrl: './interfaces.component.html',
})

export class InterfacesComponent implements OnInit, OnDestroy {
  metricCardList: MetricsCard[] = [];
  aggregationResult: AggregationResultModuleFieldName[];
  loadState: LoaderState;
  clientProjectSubscription: Subscription;
  chartDataSubscriptions: Subscription[] = [];
  projectId: number;
  filterIds: number[];
  taxonomyTitles: string[];

  constructor(
    private moduleControllerService: ModuleControllerService,
    private translationService: TranslateService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private chartGlobalStyles: ChartGlobalStyles) { }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.projectId = response.getProjectId();
      // setting both the filterIds and taxonomyTitles as empty temporarly will set it back once taxonomy filter available for it.
      this.filterIds = [];
      this.taxonomyTitles = [];
      const request: AggregationRequestModuleFieldName = {
        groupBy: new Set([AggregationRequestModuleFieldName.GroupByEnum.INTERFACE]),
        orderBy: [],
        fields: {
          INBOUND: 'SUM',
          OUTBOUND: 'SUM',
        },
      };
      this.metricCardList.length = 0;
      this.chartDataSubscriptions?.forEach((chartDataSubscription: Subscription) => chartDataSubscription.unsubscribe());
      this.chartDataSubscriptions.length = 0;
      this.loadState = LoaderState.loading;
      this.fetchInterfacesData(request, response.getProjectId());
    });
  }

  /**
   * method to create filter and filter string based on the selection
   * @param  chartValues Object containing name and value for the selected card
   * @returns object containing filterString and selected filter
   */
  generateFilterQuery(chartValues: Record<string, string>): ChartFilters {
    const filters = {};
    const filterObject = {};
    if (this.filterIds.length) {
      filters['taxonomy'] = this.taxonomyTitles;
    }
    if (chartValues['key']) {
      filters['category'] = chartValues['key'];
    }
    return { filterObject, filters };
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
    this.chartDataSubscriptions?.forEach((chartDataSubscription: Subscription) => chartDataSubscription.unsubscribe());
  }

  private fetchInterfacesData(request: AggregationRequestModuleFieldName, projectId: number) {
    const aggregateUtilityData = this.moduleControllerService
      .getAggregatedUtilityValues(projectId, request)
      .subscribe((response: AggregationResultModuleFieldName[]) => {
        if (response && response.length) {
          const chartData: ChartDataInterface[] = [];
          // filtering the response which isn't having any interface data to avoid displaying in the metrics card
          response
            .filter(res => res.group[AggregationRequestModuleFieldName.GroupByEnum.INTERFACE] as any !== '' &&
              Number(res.fields['INBOUND']) + Number(res.fields['OUTBOUND']) !== 0)
            .forEach((aggreg) => {
              chartData.push({
                key: aggreg.group[AggregationRequestModuleFieldName.GroupByEnum.INTERFACE] as any,
                value: Number(aggreg.fields['INBOUND']) + Number(aggreg.fields['OUTBOUND']),
              });
            });

          if (chartData.length) {
            this.metricCardList.push({
              title: this.translationService.instant('metrics.interfaces.cardTitle'),
              kpiList: [],
              description: this.translationService.instant('metrics.interfaces.cardDescription'),
              descriptionPosition: DescriptionPosition.Top,
              chartType: ChartType.COLUMN,
              chartConfig: this.chartGlobalStyles.getColumnConfig(chartData, 'key', 'value', 'classTheme', false, '', 'defaultLabel', true),
              chartFilterData: {
                tableType: MetricsTableType.InterfacesTable,
                showChartDetails: true,
                filterArgs: ['key', 'value'],
                queryFilterBuilder: (chartValues?: Record<string, string>): ChartFilters => this.generateFilterQuery(chartValues),
                dataFilter: (category: string, filterVal?: Array<Record<string, string | number>>) => this.filterInterfaceData(category, filterVal)
              }
            });
          }
          this.loadState = chartData.length ? LoaderState.success : LoaderState.nocontent;
        } else {
          this.loadState = LoaderState.nocontent;
        }
      }, () => {
        this.loadState = LoaderState.error;
      });
    this.chartDataSubscriptions.push(aggregateUtilityData);
  }

  private filterInterfaceData(interfaces: string, data: AggregationResultModuleFieldName[]): any {
    return data.filter(x =>  x.group.INTERFACE as any === interfaces);
  }
}
