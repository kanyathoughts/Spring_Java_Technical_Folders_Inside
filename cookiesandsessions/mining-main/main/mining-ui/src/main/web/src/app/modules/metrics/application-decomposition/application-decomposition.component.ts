import { Component, OnDestroy, OnInit } from '@angular/core';
import { Logger } from '@app/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { TaxonomyPojo, TaxonomyControllerService } from '@innowake/mining-api-angular-client';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';
import { ChartFilters, ChartType, DescriptionPosition, MetricsCard, MetricsTableType } from '../shared/components/metrics-card/metrics-card.interface';
import { ChartGlobalStyles } from '../shared/utils/chart-global-styles.utils';
import { MetricsFilterService } from '@app/core/services/metrics-filter.service';
import { getTaxonomyIdsTitle } from '@app/core/utils/taxonomy.utils';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { map } from 'rxjs/operators';

const log = new Logger('ApplicationDecompositionComponent');

@Component({
  selector: 'app-application-decomposition',
  templateUrl: './application-decomposition.component.html'
})
export class ApplicationDecompositionComponent implements OnInit, OnDestroy {

  metricCardList: MetricsCard[] = [];
  loadState: LoaderState;
  clientProjectSubscription: Subscription;
  metricsFilterSubscription: Subscription;
  chartDataSubscription: Subscription;
  projectId: number;
  taxIds: number[] = [];

  constructor(
    private taxonomyControllerService: TaxonomyControllerService,
    private translationService: TranslateService,
    private metricsFilterService: MetricsFilterService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private chartGlobalStyles: ChartGlobalStyles
  ) { }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.metricsFilterSubscription?.unsubscribe();
      this.metricsFilterSubscription = this.metricsFilterService.getMetricsTaxonomyFilter().subscribe((filterDetail: TaxonomyFilterSelected[]) => {
        this.metricCardList.length = 0;
        this.loadState = LoaderState.loading;
        this.projectId = response.getProjectId();
        const filterDetails = getTaxonomyIdsTitle(filterDetail);
        this.fetchApplicationDecompositionData(response.getProjectId(), filterDetails.taxonomyIds, filterDetails.taxonomyTitles);
      });
      this.taxonomyControllerService.findAllTaxonomies(this.projectId, 'Program Type').pipe(
        map((resp: TaxonomyPojo[]) => resp.map(taxonomy => taxonomy.id))
      ).subscribe(
        (ids: number[]) => {
          this.taxIds = ids;
        }
      );
    });
  }

  /**
   * method to create filter and filter string based on the selection
   * @param  chartValues Object containing name and value for the selected card
   * @param  filterTitle Taxonomy filter is selected
   * @param  selectedTaxonomy ids of selected taxonomy.
   * @returns object containing filterString and selected filter
   */
  generateFilterQuery(chartValues: Record<string, string>, filterTitle: string, selectedTaxonomy: number[]): ChartFilters {
    const filterObject = {};
    const filterTitleArray: string[] = [];
    if (chartValues && chartValues.name) {
      filterTitleArray.push(chartValues.name);
      filterObject['content_taxonomies_name'] = { in: chartValues.name.split(':')[1].trim() };
    } else {
      filterObject['content_taxonomies_id'] = { in: this.taxIds};
    }
    if (filterTitle) {
      if (filterTitle !== '("Program Type")') {
        filterObject['content_taxonomies_id'] = { in: selectedTaxonomy };
      }
      const filterString = filterTitle.substring(2, filterTitle.length - 2).replace('","', ',');
      if (filterTitleArray.findIndex(x => x.trim() === filterString.trim()) < 0) {
        filterTitleArray.push(filterString);
      }
    }
    const filters = filterTitleArray.length > 0 ? { taxonomy: filterTitleArray.join(' , ') } : {};
    return { filterObject, filters };
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.chartDataSubscription?.unsubscribe();
    this.clientProjectSubscription.unsubscribe();
    this.metricsFilterSubscription?.unsubscribe();
  }

  private fetchApplicationDecompositionData(projectId: number, selectedTaxonomy: number[], filterTitle: string[]): void {
    const selectedTaxonomyIds: number[] = selectedTaxonomy.length ? selectedTaxonomy.join(',').split(',').map(Number) : [];
    const formatedFilterTitle = filterTitle && filterTitle?.length ? '("' + filterTitle.join('","') + '")' : '';
    this.chartDataSubscription?.unsubscribe();
    this.chartDataSubscription = this.taxonomyControllerService.findAllTaxonomies(projectId, 'Program Type', selectedTaxonomyIds).pipe(
      map((resp: TaxonomyPojo[]) => resp.filter(taxonomy => taxonomy.taxonomyReferenceCount > 0))
    ).subscribe(
      (taxonomies: TaxonomyPojo[]) => {
        if (taxonomies && taxonomies.length) {
          const chartData: Array<{ name: string, value: number }> = [];
          taxonomies.forEach((item: TaxonomyPojo) => {
            chartData.push({ name: `${item['type'].name}: ${item['name']}`, value: item['taxonomyReferenceCount'] });
          });
          this.metricCardList.push({
            title: this.translationService.instant('metrics.applicationDecomposition.cardTitle'),
            descriptionPosition: DescriptionPosition.Bottom,
            chartType: ChartType.PIE,
            chartConfig: this.chartGlobalStyles.getPieConfig(chartData, 'value', 'name', 'classTheme', true),
            showEclipseHint: false,
            chartFilterData: {
              tableType: MetricsTableType.ModuleDetailsTable,
              showChartDetails: true,
              showExportButton: true,
              filterArgs: ['name', 'value'],
              queryFilterBuilder: (chartValues?: Record<string, string>): ChartFilters =>
                this.generateFilterQuery(chartValues, formatedFilterTitle, selectedTaxonomyIds)
            }
          });
        } else {
          this.metricCardList.push({
            title: this.translationService.instant('metrics.applicationDecomposition.noDataCardTitle'),
            description: this.translationService.instant('metrics.applicationDecomposition.noDataCardDescription'),
            descriptionPosition: DescriptionPosition.Top,
            showEclipseHint: true,
          });
        }
        this.loadState = LoaderState.success;
      },
      (error: any) => {
        this.loadState = LoaderState.error;
        log.error(`Error Occured: ${error.message}`);
      }
    );
  }
}
