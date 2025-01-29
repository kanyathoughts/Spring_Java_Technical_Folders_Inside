import { Component, OnDestroy, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs/internal/Subscription';
import { ChartDataInterface, ChartType, DescriptionPosition, MetricsCard } from '../shared/components/metrics-card/metrics-card.interface';
import { ChartGlobalStyles } from '../shared/utils/chart-global-styles.utils';
import { MetricsFilterService } from '@app/core/services/metrics-filter.service';
import { TaxonomyDetails } from '@app/shared/interfaces/taxonomy-list.interface';
import { getTaxonomyIdsTitle } from '@app/core/utils/taxonomy.utils';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { forkJoin } from 'rxjs';
import { ColumnOptions, LineOptions } from '@antv/g2plot';
import {
  AggregationRequestAnnotationFieldName,
  AggregationRequestDataDictionaryFieldName,
  AggregationResultAnnotationFieldName,
  AggregationResultDataDictionaryFieldName,
  AnnotationControllerService,
  DataDictionaryControllerService,
  EntityId
} from '@innowake/mining-api-angular-client';

const ruleMapping = new Map<string, string[]>([
  ['Field Computation Rule', ['fieldComputationRule', 'cardTitle4', 'cardDescription4']],
  ['Technical Rule', ['technicalRule', 'cardTitle5', 'cardDescription5']],
  ['Validation Rule', ['validationRule', 'cardTitle6', 'cardDescription6']],
  ['Error Processing Rule', ['errorProcessingRule', 'cardTitle7', 'cardDescription7']]
]);
@Component({
  selector: 'app-rule-candidates',
  templateUrl: './rule-candidates.component.html'
})
export class RuleCandidatesComponent implements OnInit, OnDestroy {

  metricCardList: MetricsCard[] = [];
  loadState: LoaderState;
  metricsFilterSubscription: Subscription;
  chartDataSubscription: Subscription;
  private databaseCandidates: Array<{ key: string; value: number; }> = [];
  private ruleCandidates: Array<[{ key: string; value: number; }]> = [];
  private businessRuleCandidates: Array<{ key: string; value: number; }> = [];
  private dataDictionaryCandidates: Array<{ key: string; value: number; }> = [];
  private clientProjectSubscription: Subscription;

  constructor(
    private annotationControllerService: AnnotationControllerService,
    private translationService: TranslateService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private metricsFilterService: MetricsFilterService,
    private chartGlobalStyles: ChartGlobalStyles,
    private dataDictionaryControllerService: DataDictionaryControllerService) { }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.metricsFilterSubscription?.unsubscribe();
      this.metricsFilterSubscription = this.metricsFilterService.getMetricsTaxonomyFilter().subscribe((filterDetail: TaxonomyFilterSelected[]) => {
        const filterDetails: TaxonomyDetails = getTaxonomyIdsTitle(filterDetail);
        this.loadState = LoaderState.loading;
        this.metricCardList.length = 0;
        this.fetchRuleCandidatesData(response.getProjectId(), filterDetails.taxonomyIds);
      });
    });
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.metricsFilterSubscription?.unsubscribe();
    this.clientProjectSubscription.unsubscribe();
    this.chartDataSubscription?.unsubscribe();
  }

  /**
   * Method to fetch the aggregate data for Bar Chart.
   * @param requestCategoryAndInvocations input JSON payload for API getAggregatedValues.
   * @param id  contains the project ID.
   * @param filterIds contains the ID on filter selection.
   */
  private fetchRuleCandidatesData(id: EntityId, filterIds: number[]) {
    const taxonomyFilter = filterIds.length ? { 'TAXONOMY_ID': { 'in': filterIds }} : {};
    const requestCategoryAndInvocations: AggregationRequestAnnotationFieldName = {
      filterObject: {
        ...taxonomyFilter,
        [AggregationRequestAnnotationFieldName.GroupByEnum.STATE]: { 'eq': 'CANDIDATE' as any }
      },
      fields: {
        [AggregationRequestAnnotationFieldName.GroupByEnum.ID]: 'COUNT'
      },
      groupBy: new Set([AggregationRequestAnnotationFieldName.GroupByEnum.TYPE, AggregationRequestAnnotationFieldName.GroupByEnum.CATEGORY])
    };
    const dataDictionaryRequestString: AggregationRequestDataDictionaryFieldName = {
      filterObject: {
        ...taxonomyFilter,
        [AggregationRequestDataDictionaryFieldName.GroupByEnum.IS_CANDIDATE]: { 'eq': true}
      },
      fields: {
        [AggregationRequestAnnotationFieldName.GroupByEnum.ID]: 'COUNT'
      }
    };

    this.chartDataSubscription?.unsubscribe();
    this.chartDataSubscription = forkJoin([
      this.annotationControllerService.getAggregatedValues4(id, requestCategoryAndInvocations),
      this.dataDictionaryControllerService.getAggregatedValues3(id, dataDictionaryRequestString)
    ]).subscribe(([results, dataDictionaryResult]: [AggregationResultAnnotationFieldName[], AggregationResultDataDictionaryFieldName[]]) => {
      this.loadState = LoaderState.success;
      if (results && results.length || dataDictionaryResult && dataDictionaryResult.length) {
        this.parseCandidates(results, dataDictionaryResult);
      }
      this. displayCards();
    }, () => {
      this.loadState = LoaderState.error;
    });
  }

  private parseCandidates(annotationCandidates: AggregationResultAnnotationFieldName[], ddCandidates: AggregationResultDataDictionaryFieldName[]) {
    this.databaseCandidates = [];
    this.businessRuleCandidates = [];
    this.dataDictionaryCandidates = [];
    this.ruleCandidates = [];
    annotationCandidates?.forEach((aggregationResult: AggregationResultAnnotationFieldName) => {
      if (aggregationResult.group[AggregationRequestAnnotationFieldName.GroupByEnum.TYPE] + '' === 'DATABASE') {
        if ( ! this.databaseCandidates.length) {
          this.databaseCandidates.push({
            key: aggregationResult.group[AggregationRequestAnnotationFieldName.GroupByEnum.TYPE] + '',
            value: +aggregationResult.fields[AggregationRequestAnnotationFieldName.GroupByEnum.ID]
          });
        } else {
          this.databaseCandidates[0].value += +aggregationResult.fields[AggregationRequestAnnotationFieldName.GroupByEnum.ID];
        }
      } else if (aggregationResult.group[AggregationRequestAnnotationFieldName.GroupByEnum.TYPE] + '' === 'RULE'
        && aggregationResult.group[AggregationRequestAnnotationFieldName.GroupByEnum.CATEGORY] + '' === 'Business Rule') {
        this.businessRuleCandidates.push({
          key: aggregationResult.group[AggregationRequestAnnotationFieldName.GroupByEnum.CATEGORY] + '',
          value: +aggregationResult.fields[AggregationRequestAnnotationFieldName.GroupByEnum.ID]
        });
      } else if (aggregationResult.group[AggregationRequestAnnotationFieldName.GroupByEnum.TYPE] + '' === 'RULE'
        && ruleMapping.has(aggregationResult.group[AggregationRequestAnnotationFieldName.GroupByEnum.CATEGORY].toString())) {
        this.ruleCandidates.push([{
          key: aggregationResult.group[AggregationRequestAnnotationFieldName.GroupByEnum.CATEGORY] + '',
          value: +aggregationResult.fields[AggregationRequestAnnotationFieldName.GroupByEnum.ID]
        }]);
      }
    });
    ddCandidates?.forEach((aggregationResult: AggregationResultDataDictionaryFieldName) => {
      if (+aggregationResult.fields[AggregationRequestDataDictionaryFieldName.GroupByEnum.ID] !== 0) {
        this.dataDictionaryCandidates.push({
          key: this.translationService.instant('metrics.ruleCandidates.dataDictionary'),
          value: +aggregationResult.fields[AggregationRequestDataDictionaryFieldName.GroupByEnum.ID]
        });
      }
    });
  }

  private displayCards() {
    if (this.businessRuleCandidates.length || this.databaseCandidates.length || this.dataDictionaryCandidates.length) {
      if (this.businessRuleCandidates.length > 0) {
        const chartConfig = this.chartGlobalStyles.getBarConfig(this.businessRuleCandidates, 'value', 'key', 'classTheme');
        this.addDataToCardList('cardTitle1', 'cardDescription1', false, chartConfig, ChartType.BAR);
      }
      if (this.databaseCandidates.length > 0) {
        const chartConfig = this.chartGlobalStyles.getBarConfig(this.databaseCandidates, 'value', 'key', 'classTheme');
        this.addDataToCardList('cardTitle2', 'cardDescription2', false, chartConfig, ChartType.BAR);
      }
      if (this.dataDictionaryCandidates.length > 0) {
        const chartConfig = this.chartGlobalStyles.getBarConfig(this.dataDictionaryCandidates, 'value', 'key', 'classTheme');
        this.addDataToCardList('cardTitle3', 'cardDescription3', false, chartConfig, ChartType.BAR);
      }
      this.ruleCandidates.sort((a, b) => (a[0].key > b[0].key) ? 1 : -1);
      this.ruleCandidates?.forEach((businessRuleCategory) => {
        if (businessRuleCategory.length > 0) {
          const chartConfig = this.chartGlobalStyles.getBarConfig(businessRuleCategory, 'value', 'key', 'classTheme');
          this.addDataToCardList(ruleMapping.get(businessRuleCategory[0].key)[1],
            ruleMapping.get(businessRuleCategory[0].key)[2], false, chartConfig, ChartType.BAR);
        }
      });
    } else {
      this.addDataToCardList('menuItem', 'noDataCardDescription', true);
    }
  }

  private addDataToCardList(cardTitle: string, cardDescription: string, showEclipseHint: boolean,
    chartConfig?: ChartDataInterface[] | ColumnOptions | LineOptions, chartType?: ChartType): void {
    this.metricCardList.push({
      title: this.translationService.instant(`metrics.ruleCandidates.${cardTitle}`),
      description: this.translationService.instant(`metrics.ruleCandidates.${cardDescription}`),
      descriptionPosition: DescriptionPosition.Top,
      chartType,
      chartConfig,
      showEclipseHint
    });
  }
}
