import { Component, Input, OnChanges } from '@angular/core';
import { ModuleComplexityDetails } from '../models/module-complexity-details';
import { dateFormatter } from '@app/core/utils/date-formatter.utils';
import { TranslateService } from '@ngx-translate/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { ModuleMetricsDetailsService } from '@app/core/services/module-details/module-metrics-details.service';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { ModulePojo } from '@innowake/mining-api-angular-client';

@Component({
    selector: 'mn-module-characteristics',
    templateUrl: './module-characteristics.component.html'
})
export class ModuleCharacteristicsComponent implements OnChanges{
  @Input() module: ModulePojo;
  @Input() moduleComplexity: ModuleComplexityDetails;
  @Input() numberOfErrors: number;
  moduleComplexityColor: string;
  moduleComplexityDescription: string;
  lastScanDate: string;
  technology: string;
  moduleType: string;
  modulePath: string;
  labels: { [key: string]: { [key: string]: string } };

  constructor(
    private translateService: TranslateService,
    public formateNumber: NumberFormatter,
    public moduleMetricsDetailsService: ModuleMetricsDetailsService,
    private labelMappingService: LabelMappingService
    ) {
   }

  ngOnChanges(): void {
    this.lastScanDate = this.module?.metricsDate ? dateFormatter(this.module.metricsDate) : this.translateService.instant('notAvailable');
    this.technology = this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, this.module.technology);
    this.moduleType = this.labelMappingService.mapLabel(LabelType.TYPE, this.module.type);
    this.modulePath = this.module?.path ? this.module.path : this.module?.parentPath ? this.module.parentPath
      : this.translateService.instant('notAvailable');
  }

  /**
   * Method to get the number of lines of comments,
   * along with the percentage as compared to total lines of code in the module.
   */
  getNumberOfLinesOfComment(): string {
    if ( ! this.module?.sourceMetrics || ! this.module.sourceMetrics?.commentLines) {
      return '0';
    }
    let percentText = '';
    if (this.module.sourceMetrics?.codeLines > 0) {
      let percentage = (this.module.sourceMetrics.commentLines / this.module.sourceMetrics.codeLines) * 100;
      percentage = Math.floor(percentage);
      percentText = ` (${percentage}%)`;
    }
    return this.formateNumber.transform(this.module.sourceMetrics.commentLines) + percentText;
  }
}
