import { Component, Input, OnInit } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { FormBuilder, FormGroup } from '@angular/forms';
import { DataLineageInputData } from './data-linaege-export.interface';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { EntityId, JobControllerService } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'data-lineage-export',
  templateUrl: './data-lineage-export.component.html',
})

export class DataLineageExportComponent implements OnInit {
  @Input() projectId: number;
  @Input() moduleId: EntityId;
  @Input() fieldOffset: number;
  dataLineageForm: FormGroup;
  isDataLineageExporting = false;
  dataLineageLevels = [this.translateService.instant('dataLineage.moduleLevel'), this.translateService.instant('dataLineage.fieldLevel'),
  this.translateService.instant('dataLineage.statementLevel')];
  selectedDataLineageLevel = this.dataLineageLevels[0];
  selectedDataLevel = this.translateService.instant('dataLineage.moduleDetail');
  inputData: DataLineageInputData = { detailLevel: ['MODULE'] };

  constructor(private jobService: JobControllerService,
    private translateService: TranslateService,
    private modal: NzModalRef, private fb: FormBuilder,
    private jobManagerService: JobManagerService,

  ) {
  }

  ngOnInit(): void {
    this.dataLineageForm = this.fb.group({
      dataLineageSelection: [[]]
    });
    this.dataLineageForm.controls.dataLineageSelection.setValue(this.dataLineageLevels[0]);
  }

  /**
   * method to get the selected data lineage
   */
  onDataLineageChange(): void {
    this.selectedDataLineageLevel = this.dataLineageForm.controls.dataLineageSelection.value;
    switch (this.selectedDataLineageLevel) {
      case this.dataLineageLevels[0]:
        this.inputData.detailLevel = ['MODULE'];
        this.selectedDataLevel = this.translateService.instant('dataLineage.moduleDetail');
        break;
      case this.dataLineageLevels[1]:
        this.inputData.detailLevel = ['FIELD'];
        this.selectedDataLevel = this.translateService.instant('dataLineage.fieldDetail');
        break;
      case this.dataLineageLevels[2]:
        this.inputData.detailLevel = ['STATEMENT'];
        this.selectedDataLevel = this.translateService.instant('dataLineage.statementDetail');
        break;
    }
  }

  /**
   * method to close the data lineage modal
   */
  closeDataLineageModal(): void {
    this.modal.close();
  }

  /**
   * method to export the data lineage
   */
  exportDataLineage(): void {
    this.inputData.moduleId = [this.moduleId + ''];
    if (this.fieldOffset) {
      this.inputData.fieldOffset = [this.fieldOffset+ ''];
    }
    this.jobService.submitJobExtension(this.projectId, 'datalineage-gml', this.inputData).subscribe((data) => {
      this.isDataLineageExporting = true;
      this.getJobStatus(data.toString());
    });
  }

  private getJobStatus(jobId: string) {
    this.jobManagerService.register({ jobId, foreground: true, cancellable: true , autoDownloadResult: true});
    this.closeDataLineageModal();
  }
}
