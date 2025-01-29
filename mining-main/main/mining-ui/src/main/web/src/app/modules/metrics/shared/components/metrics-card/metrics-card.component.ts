import { Component, Input, AfterViewInit, ViewChild, ElementRef, NgZone, OnDestroy, Output, EventEmitter } from '@angular/core';
import { Bar, BarOptions, Column, ColumnOptions, Line, LineOptions, Pie, PieOptions, Plot } from '@antv/g2plot';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { ChartEvent, ChartFilterData, ChartType, DescriptionPosition, Kpi } from './metrics-card.interface';
import FileSaveSupport from '@app/core/utils/file-save-support.utils';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { ChartCardClickHandlerService } from '@app/core/services/chart-card-click-handler.service';
import { Subscription } from 'rxjs';

@Component({
  selector: 'app-metrics-card',
  templateUrl: './metrics-card.component.html'
})
export class MetricsCardComponent implements AfterViewInit, OnDestroy {
  @Input() pageTitle: string;
  @Input() title: string;
  @Input() description: string;
  @Input() descriptionPosition = DescriptionPosition.Top;
  @Input() kpiList: Kpi[];
  @Input() chartType: ChartType;
  @Input() chartConfig: any;
  @Input() showEclipseHint: boolean;
  @Input() chartFilterData: ChartFilterData;

  @Output() openTableForFilter: EventEmitter<Record<string, string>> = new EventEmitter();
  @ViewChild('metricChart') chart: ElementRef;
  filterObj = {};
  clickedChartEntity: ChartEvent = ChartEvent.CHART;

  private chartPlot: Plot<any> | undefined;
  private clickServiceSubscription: Subscription;

  constructor(
    private readonly ngZone: NgZone,
    public formateNumber: NumberFormatter,
    private messageService: NzMessageService,
    private translateService: TranslateService,
    public clickService: ChartCardClickHandlerService
  ) { }

  ngAfterViewInit(): void {
    if ( ! this.showEclipseHint) {
      switch (this.chartType) {
        case ChartType.BAR:
          this.chartPlot = new Bar(this.chart.nativeElement as HTMLElement, this.chartConfig as BarOptions);
          break;

        case ChartType.COLUMN:
          this.chartPlot = new Column(this.chart.nativeElement as HTMLElement, this.chartConfig as ColumnOptions);
          break;

        case ChartType.LINE:
          this.chartPlot = new Line(this.chart.nativeElement as HTMLElement, this.chartConfig as LineOptions);
          break;

        case ChartType.PIE:
          this.chartPlot = new Pie(this.chart.nativeElement as HTMLElement, this.chartConfig as PieOptions);
          break;
      }
      if (this.chartPlot) {
        this.clickService.addListenerToChart(this.chartPlot, this.title);
        this.clickServiceSubscription = this.clickService.getFinalClick().subscribe((clickEle: { event: ChartEvent, cardTitle: string }) => {
          if (clickEle.cardTitle !== this.title || clickEle.event === ChartEvent.LEGEND) {
            return null;
          }
          if (clickEle.event === ChartEvent.ELEMENT) {
            const args = this.clickService.getElementArgs();
            this.chartFilterData?.filterArgs.forEach((item: string) => {
              this.filterObj[item] = args.data.data[item];
              if (args.data.data['technology']) {
                this.filterObj['technology'] = args.data.data['technology'];
              }
            });
          }
          this.generateFilterObjAndEmitEvent();
        });
        this.ngZone.runOutsideAngular(() => this.chartPlot.render());
      }
    }
  }

  /**
   * Exports Single Chart as Image.
   * @param format format of chart to be exported, default format PNG.
   * @param event Dom event used for stopping event bubbling.
   */
  async exportChart(format: string, event: Event): Promise<void> {
    event.stopPropagation(); // Stops opening of detail table.
    const baseFileName = this.pageTitle.replace(/\s+/g, '-') + '_' + this.title.replace(/\s+/g, '-');
    try {
      const canvas = this.chartPlot.chart.getCanvas();
      canvas.get('timeline').stopAllAnimations();
      const canvasDom = canvas.get('el');
      const dataURL: string = await canvasDom.toDataURL('image/png');
      await FileSaveSupport.save(dataURL, baseFileName + format);
    } catch (error) {
      this.messageService.error(`${this.translateService.instant('chartExportError', { cardTitle: this.title })}`);
    }
  }

  /**
   * generate the filterObj from the available filterObj arguments and emit the event to parent component
   * when clicked on chart or card.
   */
  generateFilterObjAndEmitEvent(): void {
    this.openTableForFilter.emit(this.filterObj);
    this.filterObj = {};
  }

  ngOnDestroy(): void {
    if (this.chartPlot) {
      this.chartPlot.destroy();
      this.chartPlot = undefined;
    }
    this.clickServiceSubscription?.unsubscribe();
  }
}
