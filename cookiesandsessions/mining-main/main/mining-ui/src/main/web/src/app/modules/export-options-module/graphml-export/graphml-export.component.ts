import { Component, Input, OnInit, Optional } from '@angular/core';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { sortArrayBasedOnKeyAndDirection } from '@app/core/utils/sort.util';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { TranslateService } from '@ngx-translate/core';
import { NzDrawerRef } from 'ng-zorro-antd/drawer';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { EXPORT_TOKEN } from '../export-options-module.component';
import { DataPoint } from '../export-options-module.interface';
import { DataPointControllerService, JobControllerService, JobInformation, MiningDataPointDefinitionWithPath } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-graphml-export',
  templateUrl: './graphml-export.component.html'
})
export class GraphmlExportComponent implements OnInit {

  @Input() projectId: number;

  dataPoints: DataPoint[];
  isDownloading = false;

  constructor(
    private dataPointControllerService: DataPointControllerService,
    private jobController: JobControllerService,
    private jobManager: JobManagerService,
    private notification: NzNotificationService,
    private translateService: TranslateService,
    @Optional() public drawerRef: NzDrawerRef<string>
  ) { }

  ngOnInit(): void {
    this.dataPointControllerService.getDataPointsForType(this.projectId, TypeName.MODULE, [Usages.MININGUIGRAPHML]).subscribe(
      (dataPointDefinitions: MiningDataPointDefinitionWithPath[]) => {
        const sortedDataPointDefinitions: MiningDataPointDefinitionWithPath[]  = dataPointDefinitions.map((dataPointDefinition) =>
          ({ label: dataPointDefinition.displayName, id: dataPointDefinition.path, checked: false })
        );
        this.dataPoints = sortArrayBasedOnKeyAndDirection(sortedDataPointDefinitions, 'label', 'ASC', 'string');
      }
    );
  }

  /**
   * method to download graphML for selected data points
   */
  downloadGraphML(): void {
    const selectedDataPoints = this.dataPoints.filter(dataPoint => dataPoint.checked).map(dataPoint => dataPoint.id);
    const options = selectedDataPoints.length === 0 ? {} : { attributes: selectedDataPoints };
    this.invokeExportJob('graphml', options);
  }

  /**
   * method to close the drawer
   */
  closeGraphMLdrawer(): void {
    this.drawerRef?.close();
  }

  private invokeExportJob(format: string, options: { [key: string]: string[] } = {}): void {
    this.isDownloading = true;
    this.jobController.submitJobExtension(this.projectId, format, options).subscribe(jobId => {
      this.dataPoints.forEach((dataPointDefinition) => {
        dataPointDefinition.checked = false;
      });
      const remoteJob = {
        jobId: jobId as unknown as string,
        uiToken: EXPORT_TOKEN + format,
        autoDownloadResult: true,
        foreground: true
      };
      this.jobManager.register(remoteJob).status$.subscribe((result: string) => {
        if(result === JobInformation.StatusEnum.SUCCESS) {
          this.isDownloading = false;
        }
      });
    }, () => {
      this.isDownloading = false;
      this.notification.error(
        this.translateService.instant('exportOptionsComponent.graphMLExportFailed') as string,
        this.translateService.instant('contactSupport') as string,
        { nzDuration: 0 }
      );
    });
  }
}
