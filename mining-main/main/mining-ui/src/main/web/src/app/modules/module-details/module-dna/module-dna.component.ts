import { AfterViewInit, ChangeDetectorRef, Component, ElementRef, Input, NgZone, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Data } from '@angular/router';
import { Pie, PieOptions, Plot } from '@antv/g2plot';
import { ChartCardClickHandlerService } from '@app/core/services/chart-card-click-handler.service';
import { DNAAnalysisService } from '@app/core/services/dna-analysis.service';
import { DnaCard, DnaChartFilterData } from '@app/modules/dna-analysis/dna-card.interface';
import { ChartEvent, ChartFilters } from '@app/modules/metrics/shared/components/metrics-card/metrics-card.interface';
import { ChartGlobalStyles } from '@app/modules/metrics/shared/utils/chart-global-styles.utils';
import { EntityId } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';

@Component({
  selector: 'mn-module-dna',
  templateUrl: './module-dna.component.html'
})
export class ModuleDNAComponent implements AfterViewInit, OnInit, OnDestroy {
  @Input() dnaCardData: DnaCard;
  @Input() chartFilterData: DnaChartFilterData;
  @Input() projectId: number;
  @Input() moduleId: EntityId;
  @ViewChild('moduleDnaChart') chart: ElementRef;
  filterObj = {};
  algorithm: string;
  chartConfig: any = [];
  chartTitle: string;
  titleToolTip: string;
  infoText: string;
  moduleCount: number;
  chartDataFilters: ChartFilters;
  isModuleList = false;
  showChartDetails = false;
  private chartPlot: Plot<any> | undefined;
  private clickServiceSubscription: Subscription;

  constructor(
    private translateService: TranslateService,
    private readonly ngZone: NgZone,
    private chartGlobalStyles: ChartGlobalStyles,
    public clickService: ChartCardClickHandlerService,
    private dnaAnalysis: DNAAnalysisService,
    private changeDetector: ChangeDetectorRef
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
      this.ngZone.runOutsideAngular(() => {
        this.chartPlot.render();
        this.chartPlot.setState('selected', (data: Data) => data.index === this.dnaCardData.selectedIndex);
        this.chartPlot.setState('inactive', (data: Data) => data.index !== this.dnaCardData.selectedIndex);
      });
    }
    this.changeDetector.detectChanges();
  }

  ngOnInit(): void {
    this.moduleCount = this.dnaCardData['similarModuleCount'] - 1;
    this.chartConfig = this.chartGlobalStyles.getDonutConfig(this.dnaCardData.chartData, 'value', 'key', 'seqTheme', true);
    this.algorithm = this.dnaCardData.title;
    this.chartTitle = this.dnaCardData.title.split(' ')[1];
    this.infoText = this.dnaCardData.selectedIndex === -1 ? this.translateService.instant('noClusterinfoText',
      { sequencer: this.chartTitle.toLocaleLowerCase() }) : this.translateService.instant('infoText',
        { sequencer: this.chartTitle.toLocaleLowerCase() });
    if (this.chartTitle === 'Methods') {
      this.titleToolTip = this.translateService.instant('methodToolTip');
    } else {
      this.titleToolTip = this.translateService.instant('skeletonToolTip');
    }
    this.chartConfig.statistic.content.content = '';
  }

  /**
   * Generates the filterObj from the available filterObj arguments and emit the event to parent component
   * when clicked on chart or card.
   * @param moduleList string that specifies if module list should be displayed.
   */
  generateFilterObjAndEmitEvent(moduleList?: string): void {
    this.showChartDetails = true;
    this.chartDataFilters = this.dnaAnalysis.openChartDetailsTable(this.filterObj, this.dnaCardData);
    if (moduleList) {
      this.chartDataFilters.filters['cluster'] = this.dnaCardData.selectedIndex as any;
      this.isModuleList = true;
    } else {
      this.isModuleList = false;
    }
    this.filterObj = {};
  }

  /**
   * Closes the chart detail drawer.
   */
   closeChartDetails(): void {
    this.showChartDetails = false;
  }

  ngOnDestroy(): void {
    if (this.chartPlot) {
      this.chartPlot.destroy();
      this.chartPlot = undefined;
    }
    this.clickServiceSubscription.unsubscribe();
  }
}
