import { AfterViewInit, Component, ElementRef, EventEmitter, Input, NgZone, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { Pie, PieOptions, Plot } from '@antv/g2plot';
import { ChartCardClickHandlerService } from '@app/core/services/chart-card-click-handler.service';
import { ChartEvent } from '@app/modules/metrics/shared/components/metrics-card/metrics-card.interface';
import { ChartGlobalStyles } from '@app/modules/metrics/shared/utils/chart-global-styles.utils';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';
import { DnaCard, DnaChartFilterData } from '../dna-card.interface';
import html2canvas from 'html2canvas';
import FileSaveSupport from '@app/core/utils/file-save-support.utils';
import { NzMessageService } from 'ng-zorro-antd/message';

@Component({
  selector: 'app-dna-card',
  templateUrl: 'dna-card.component.html'
})
export class DnaCardComponent implements AfterViewInit, OnInit, OnDestroy {
  @Input() chartConfig: any;
  @Input() dnaCardData: DnaCard;
  @Input() clusterModuleCount: number;
  @Input() chartFilterData: DnaChartFilterData;
  @Output() openTableForFilter: EventEmitter<Record<string, number>> = new EventEmitter();
  @ViewChild('dnaChart') chart: ElementRef;
  filterObj = {};
  isCollapseActive = false;
  dnaChartTxt = '';
  chartId: string;
  private chartPlot: Plot<any> | undefined;
  private clickServiceSubscription: Subscription;

  constructor(
    private readonly ngZone: NgZone,
    private translateService: TranslateService,
    private chartGlobalStyles: ChartGlobalStyles,
    private numberFormatter: NumberFormatter,
    public clickService: ChartCardClickHandlerService,
    private messageService: NzMessageService,
  ) { }

  ngAfterViewInit(): void {
    this.chartPlot = new Pie(this.chart.nativeElement as HTMLElement, this.chartConfig as PieOptions);
    if (this.chartPlot) {
      this.clickService.addListenerToChart(this.chartPlot, this.dnaCardData.title);
      this.clickServiceSubscription = this.clickService.getFinalClick().subscribe((clickEle: { event: ChartEvent, cardTitle: string }) => {
        if (clickEle.cardTitle !== this.dnaCardData.title || clickEle.event === ChartEvent.LEGEND) {
          return null;
        }
        if (clickEle.event === ChartEvent.ELEMENT) {
          const args = this.clickService.getElementArgs();
          this.chartFilterData?.filterArgs.forEach((item: string) => {
            this.filterObj[item] = args.data.data[item];
          });
        }
        this.generateFilterObjAndEmitEvent();
      });
      this.ngZone.runOutsideAngular(() => this.chartPlot.render());
    }
  }

  ngOnInit(): void {
    const clusterLenth = this.dnaCardData.clustersLength;
    const totalCluster = this.dnaCardData.clusterModuleCount;
    const {assignedModuleCount} = this.dnaCardData;
    this.chartId = this.dnaCardData.title.replace(' ','-') + '-export';
    this.dnaChartTxt = this.translateService.instant('dnaAnalysis.noClusterTxt');
    const clusterPercentage = this.numberFormatter.transform((assignedModuleCount / totalCluster) * 100);
    if (clusterLenth === 1) {
      this.dnaChartTxt = this.translateService.instant('dnaAnalysis.oneClusterTxt',
        { clusterPercentage });
    } else {
      this.dnaChartTxt = this.translateService.instant('dnaAnalysis.donutChartTxt',
        { clusterNumber: clusterLenth, clusterPercentage });
    }
    this.chartConfig = this.chartGlobalStyles.getDonutConfig(this.dnaCardData.chartData, 'value', 'key','seqTheme', true);
    if (clusterLenth === 1) {
      this.chartConfig.statistic.content.content = this.translateService.instant('dnaAnalysis.donutChartWithOneClusterModuleTxt',
        { clusterNumber: clusterLenth, modules: assignedModuleCount });
      return;
    }
    this.chartConfig.statistic.content.content = this.translateService.instant('dnaAnalysis.donutChartModuleTxt',
      { clusterNumber: clusterLenth, modules: assignedModuleCount });
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
    this.clickServiceSubscription.unsubscribe();
  }

  /**
   * Exports single charts as image.
   * @param format format of chart to be exported.
   * @param event Dom event used for stopping event bubbling.
   */
  async exportDNAChart(format: string, event: Event): Promise<void> {
    event.stopPropagation(); // Stops opening of detail table.
    const baseFileName = this.translateService.instant('navigation.dna') + '_' + this.dnaCardData.title.replace(' ', '-');
    try {
      const cardCanvas: HTMLCanvasElement = await html2canvas(document.getElementById(this.chartId), {
        /* To get the configuration of chart when collapsed for different sizes of window. */
        onclone: (clonedDocument: Document) => {
          const element = clonedDocument.getElementById(this.chartId).getElementsByClassName('ant-collapse-content')[0] as HTMLElement;
          clonedDocument.getElementById(this.chartId).getElementsByClassName('ant-collapse-arrow')[0].remove();
          element.classList.add('ant-collapse-content-active');
          element.style.height = '';
          element.style.overflow = '';
          element.style.borderTopWidth = '';
        }
      });
      await FileSaveSupport.save(cardCanvas.toDataURL(), baseFileName + format);
    } catch (error) {
      this.messageService.error(`${this.translateService.instant('chartExportError', { cardTitle: this.dnaCardData.title })}`);
    }
  }
}
