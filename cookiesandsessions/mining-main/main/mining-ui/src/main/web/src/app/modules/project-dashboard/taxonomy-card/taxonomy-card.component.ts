import { AfterViewInit, Component, ElementRef, Input, NgZone, OnDestroy, ViewChild } from '@angular/core';
import { Bar, Plot } from '@antv/g2plot';
import { TranslateService } from '@ngx-translate/core';
import { ChartGlobalStyles } from '@app/modules/metrics/shared/utils/chart-global-styles.utils';
import { forkJoin, ObservableInput, Subscription } from 'rxjs';
import FileSaveSupport from '@app/core/utils/file-save-support.utils';
import { NzMessageService } from 'ng-zorro-antd/message';
import { Logger } from '@app/core';
import { HttpErrorResponse } from '@angular/common/http';
import {
  AggregationRequestTaxonomyFieldName,
  AggregationResultTaxonomyFieldName,
  TaxonomyControllerService
} from '@innowake/mining-api-angular-client';

const allTaxonomies = 'all';
const log = new Logger('Enhanced Taxonomy Card');

@Component({
  selector: 'mn-taxonomy-card',
  templateUrl: './taxonomy-card.component.html'
})
export class TaxonomyCardComponent implements AfterViewInit, OnDestroy {

  @Input() taxonomies: Array<{ category: string, taxonomies: Array<{ name: string }> }> = [];
  @Input() projectId: number;
  @ViewChild('taxonomyChart') chart: ElementRef;
  selectedTaxonomy = allTaxonomies;
  private locRequest: AggregationRequestTaxonomyFieldName;
  private moduleRequest: AggregationRequestTaxonomyFieldName;
  private aggregatedDataSubscription: Subscription;
  private chartPlot: Plot<any> | undefined;

  constructor(private readonly ngZone: NgZone,
    private taxonomyControllerService: TaxonomyControllerService,
    private chartGlobalStyles: ChartGlobalStyles,
    private translateService: TranslateService,
    private messageService: NzMessageService) { }

  ngAfterViewInit(): void {
    // For getting LOC >
    this.locRequest = {
      filterObject: {
        MODULE_REPRESENTATION: { 'eq': 'PHYSICAL' as any },
        MODULE_LINES_OF_CODE: { 'gte': 0 as any }
      },
      groupBy: new Set([AggregationRequestTaxonomyFieldName.GroupByEnum.TYPE_NAME]),
      fields: {
        [AggregationRequestTaxonomyFieldName.GroupByEnum.MODULE_LINES_OF_CODE]: 'SUM'
      },
      orderBy: [AggregationRequestTaxonomyFieldName.GroupByEnum.MODULE_LINES_OF_CODE]
    };
    // For Modules Count >
    this.moduleRequest = {
      groupBy: new Set([AggregationRequestTaxonomyFieldName.GroupByEnum.TYPE_NAME]),
      fields: {
        [AggregationRequestTaxonomyFieldName.GroupByEnum.MODULE_ID]: 'COUNT_DISTINCT'
      }
    };
    this.fetchChartData(this.locRequest, this.moduleRequest);
  }

  /**
   * Method to apply filter based on input value.
   * @param filter as input value.
   */
  OnInputValueChange(value: string): void {
    if (this.chartPlot) {
      this.chartPlot.destroy();
      this.chartPlot = undefined;
    }
    const locReq = { ...this.locRequest };
    const moduleReq = { ...this.moduleRequest };
    if (value !== allTaxonomies) {
      const filter = { TYPE_NAME: { 'eq': value as any}};

      locReq.filterObject = {...filter, ...locReq.filterObject};
      locReq.groupBy = new Set([AggregationRequestTaxonomyFieldName.GroupByEnum.NAME]);

      moduleReq.filterObject = {...filter, ...moduleReq.filterObject};
      moduleReq.groupBy = new Set([AggregationRequestTaxonomyFieldName.GroupByEnum.NAME]);
    }
    this.fetchChartData(locReq, moduleReq);
  }

  /**
   * Exports Single Chart as Image.
   * @param format format of chart to be exported, default format PNG.
   */
  async exportChart(format: string): Promise<void> {
    const cardTitle = this.selectedTaxonomy === allTaxonomies ? this.translateService.instant('projectDashboard.taxonomyCardTitle').replace(' ', '-')
    : this.selectedTaxonomy.replace(' ', '-');
    const baseFileName = this.translateService.instant('taxonomies') + '_' + cardTitle;
    try {
      const canvas = this.chartPlot.chart.getCanvas();
      canvas.get('timeline').stopAllAnimations();
      const canvasDom = canvas.get('el');
      const dataURL: string = await canvasDom.toDataURL('image/png');
      await FileSaveSupport.save(dataURL, baseFileName + format);
    } catch (error) {
      this.messageService.error(this.translateService.instant('chartExportError') as string);
    }
  }

  ngOnDestroy(): void {
    if (this.chartPlot) {
      this.chartPlot.destroy();
      this.chartPlot = undefined;
    }
    this.aggregatedDataSubscription.unsubscribe();
  }

  private fetchChartData(locRequest: AggregationRequestTaxonomyFieldName, moduleRequest: AggregationRequestTaxonomyFieldName) {
    const chartName = 'projectDashboard.modules';
    const chartData: Array<{ label: string, name: string, value: number }> = [];
    let slocRequest: ObservableInput<{ [key: string]: number }> | ObservableInput<AggregationResultTaxonomyFieldName[]>;
    if (this.selectedTaxonomy === allTaxonomies) {
      slocRequest = this.taxonomyControllerService.getSlocValues(this.projectId);
    } else {
      slocRequest = this.taxonomyControllerService.getAggregatedValues(this.projectId, locRequest);
    }
    this.aggregatedDataSubscription = forkJoin([slocRequest, this.taxonomyControllerService.getAggregatedValues(this.projectId, moduleRequest)])
      .subscribe(([slocResponse, moduleResponse]: [any, AggregationResultTaxonomyFieldName[]]) => {
        if (slocResponse && this.selectedTaxonomy === allTaxonomies) {
          for (const key in slocResponse) {
            if (slocResponse.hasOwnProperty(key)) {
              chartData.push({ label: key, name: this.translateService.instant('sourceLinesOfCode'), value: slocResponse[key] });
              chartData.push({ label: key, name: this.translateService.instant(chartName), value: null });
            }
          }
        }
        if (slocResponse && this.selectedTaxonomy !== allTaxonomies) {
          /* Doing Array reversal as with the server query can't do the order by desc */
          slocResponse.reverse();
          slocResponse.forEach((aggreg: AggregationResultTaxonomyFieldName) => {
            chartData.push(this.locModuleObj(aggreg, 'sourceLinesOfCode', AggregationRequestTaxonomyFieldName.GroupByEnum.MODULE_LINES_OF_CODE));
            chartData.push(this.locModuleObj(aggreg, chartName, null));
          });
        }
        if (moduleResponse) {
          moduleResponse.forEach((aggreg: AggregationResultTaxonomyFieldName) => {
            const label = aggreg.group[AggregationRequestTaxonomyFieldName.GroupByEnum.TYPE_NAME]
              ? aggreg.group[AggregationRequestTaxonomyFieldName.GroupByEnum.TYPE_NAME]
              : aggreg.group[AggregationRequestTaxonomyFieldName.GroupByEnum.NAME];
            const record = chartData.find(ele =>
              ele.label === label.toString() && ele.name === this.translateService.instant(chartName));
            if (record) {
              record.value = +aggreg.fields[AggregationRequestTaxonomyFieldName.GroupByEnum.MODULE_ID];
            } else {
              chartData.push(this.locModuleObj(aggreg, 'sourceLinesOfCode', null));
              chartData.push(this.locModuleObj(aggreg, chartName, AggregationRequestTaxonomyFieldName.GroupByEnum.MODULE_ID));
            }
          });
        }
        const chartConfig = this.chartGlobalStyles.getGroupedBarConfig(chartData, 'value', 'label', 'name', 'classTheme');
        this.chartPlot = new Bar(this.chart.nativeElement as HTMLElement, chartConfig);
        if (this.chartPlot) {
          this.ngZone.runOutsideAngular(() => this.chartPlot.render());
        }
      }, (error: HttpErrorResponse) => {
        log.error(error.message);
      });
  }

  private locModuleObj(aggreg: AggregationResultTaxonomyFieldName, name: string, field: string) {
    const label = aggreg.group[AggregationRequestTaxonomyFieldName.GroupByEnum.TYPE_NAME] ?? aggreg.group[AggregationRequestTaxonomyFieldName.GroupByEnum.NAME];
    return {
      label: label + '',
      name: this.translateService.instant(name),
      value: field ? +aggreg.fields[AggregationRequestTaxonomyFieldName.GroupByEnum[field]] : 0
    };
  }
}
