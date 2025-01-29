import { Component, OnDestroy, OnInit, Optional, TemplateRef, ViewChild } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { getBasePath } from '@app/core/utils/base-path.utils';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ExportDownloadService } from '../../core/services/export-download/export-download.service';
import { TranslateService } from '@ngx-translate/core';
import { Observable, Subscription } from 'rxjs';
import { map } from 'rxjs/operators';
import { exportDetails, ExportParameter, groupedDetail } from './export-options-module.interface';
import { GraphmlExportComponent } from './graphml-export/graphml-export.component';
import { NzDrawerRef, NzDrawerService } from 'ng-zorro-antd/drawer';
import { UntypedFormBuilder, UntypedFormControl, UntypedFormGroup } from '@angular/forms';
import { HttpParams, HttpUrlEncodingCodec } from '@angular/common/http';
import { NzFormTooltipIcon } from 'ng-zorro-antd/form';
import { CustomPropertyMetadata, ExportFormatDescription, IoControllerService, JobControllerService, ParameterDescription, UploadDescription }
  from '@innowake/mining-api-angular-client';
/* used to track whether an export job for a given format is already running, which prevents submitting another one */
export const EXPORT_TOKEN = 'export-options-';

@Component({
  selector: 'app-export-options-module',
  templateUrl: './export-options-module.component.html'
})
export class ExportOptionsModuleComponent implements OnInit, OnDestroy {

  @ViewChild('exportDrawerContent') exportDrawerContent: TemplateRef<any>;
  @Optional() public exportDrawerRef: NzDrawerRef<string>;
  projectId: number;
  projectName: string;
  clientProjectSubscription: Subscription;
  exportExtDetails: exportDetails[] = [];
  exportForm: UntypedFormGroup;
  exportData: ExportParameter[] = [];
  parameterDescData: ExportParameter[] = [];
  isExportSave = false;
  isExportCancel = false;
  isFileUploading = true;
  fileList: any[] =[];
  uploadDescription: UploadDescription;
  infoTooltipIcon: NzFormTooltipIcon = {
    type: 'info-circle',
    theme: 'outline'
  };

  constructor(
    private translateService: TranslateService,
    private jobController: JobControllerService,
    private jobManager: JobManagerService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private exportDownloadService: ExportDownloadService,
    private ioControllerService: IoControllerService,
    private drawerService: NzDrawerService,
    private fb: UntypedFormBuilder
  ) { }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.projectId = response.getProjectId();
      this.projectName = response.getProjectName();
      this.exportExtDetails = [];
      this.fetchExportFormatsDetails();
    });
   }

  /**
   * Exports project data to given format.
   * @param format export format.
   * @param queryParams export parameters.
   */
  exportToFormat(format: string, queryParams: Record<string, string | number | boolean>): void {
    let queryParameters = new HttpParams({ encoder: new HttpUrlEncodingCodec() });
    if (queryParams !== undefined && queryParams !== null) {
      Object.keys(queryParams).forEach((key) => {
        queryParameters = queryParameters.set(key, queryParams[key]);
      });
    }
    const url = getBasePath() + '/api/v1/projects/' + this.projectId + `/export/${format}`;
    this.exportDownloadService.downloadUrl(
      url,
      'exportOptionsComponent.exportInProgress',
      'exportOptionsComponent.exportSuccess',
      'exportOptionsComponent.exportFailure',
      null,
      queryParameters
    );
    this.onCancel();
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.clientProjectSubscription.unsubscribe();
  }

  hasRunningJobOfFormat(format: string): Observable<boolean> {
    return this.jobManager.jobs$.pipe(map(jobs => jobs.filter(job => job.uiToken === EXPORT_TOKEN + format).length > 0));
  }

  /**
   * Method is for assigning the exportDetail specific to the extension type.
   * @param exportDetail is the export detail of the Extension.
   */
  exportExtensions(exportDetail: groupedDetail): void {
    this.exportData = [];
    this.parameterDescData = [];
    if (exportDetail.parameterDescriptions.length > 0 || exportDetail.uploadDescription.supported) {
      const exportFormFields: { [k: string]: UntypedFormControl } = {};
      exportDetail?.parameterDescriptions.forEach((parameterDesc: ParameterDescription) => {
        exportFormFields[parameterDesc.name] = this.fb.control(parameterDesc.defaultValue);
        this.exportData.push({
          name: parameterDesc.name,
          label: parameterDesc.name,
          dataType: parameterDesc.type !== 'STRING' ? parameterDesc.type as CustomPropertyMetadata.DataTypeEnum :
            parameterDesc.allowMultiple ? CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST : CustomPropertyMetadata.DataTypeEnum.STRING,
          fieldType: parameterDesc.allowableValues?.length ? CustomPropertyMetadata.FieldTypeEnum.SELECT : CustomPropertyMetadata.FieldTypeEnum.DEFAULT,
          inputName: parameterDesc.name,
          id: exportDetail.id,
          jobLabel: exportDetail.label,
          extensionType: exportDetail.extensionType,
          optionList: parameterDesc.allowableValues,
          descriptionType: 'parameterDescription',
          description: parameterDesc.description,
          mandatory: parameterDesc.required
        });
      });
      if (exportDetail.uploadDescription.supported) {
        exportFormFields['input'] = this.fb.control('');
        this.exportData.push({
          label: exportDetail.label,
          id: exportDetail.id,
          extensionType: exportDetail.extensionType,
          descriptionType: 'uploadDescription'
        });
      }
      this.uploadDescription = exportDetail.uploadDescription;
      this.exportForm = new UntypedFormGroup(exportFormFields);
      this.exportData.forEach((data: ExportParameter) => {
        if (data.descriptionType === 'parameterDescription') {
          this.parameterDescData.push(data);
          if (data.dataType === CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST) {
            this.exportForm.setControl(data.inputName, this.fb.array([this.fb.control('')]));
          }
        }
      });
      this.fileList = [];
      this.exportDrawerRef = this.drawerService.create({
        nzTitle: this.translateService.instant('exportFormTitle', { exportExtensionLabel: exportDetail.label }),
        nzContent: this.exportDrawerContent,
        nzWidth: '35vw',
        nzPlacement: 'right',
        nzClosable: true,
        nzMaskClosable: false
      });
    } else {
      this.triggerExport(exportDetail.extensionType, exportDetail.id, exportDetail.label, {});
    }
  }

  advancedGraphMLEport(): void {
    this.drawerService.create({
      nzTitle: this.translateService.instant('exportOptionsComponent.graphMLDrawerTitle'),
      nzContent: GraphmlExportComponent,
      nzContentParams: {
        projectId: this.projectId
      },
      nzWidth: '35vw',
      nzPlacement: 'right',
      nzClosable: true,
      nzMaskClosable: false
    });
  }

  /**
   * Submits the export form.
   */
  onSubmit(): void {
    this.isExportSave = true;
    const formData = {};
    this.exportData.forEach((data) => {
      if (data.descriptionType === 'parameterDescription') {
        formData[data.inputName] = data.dataType === CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST ?
          this.exportForm.controls[data.inputName].value.filter((e: any) => e) : data.dataType === CustomPropertyMetadata.DataTypeEnum.STRING ?
          [this.exportForm.controls[data.inputName].value.trim()] : [this.exportForm.controls[data.inputName].value];
      }
    });
    this.triggerExport(this.exportData[0].extensionType, this.exportData[0].id, this.exportData[0].label, formData);
  }

  /**
   * Returns if the form can be submitted. This is the case if its status is VALID.
   * Otherwise `false` is returned.
   * @returns `true` if the form is valid, otherwise `false`.
   */
  canSubmitForm(): boolean {
    if (this.exportForm) {
      return this.exportForm.valid && this.exportForm.dirty;
    }
  }

  /**
   * Closes the export form without submitting.
   * @returns `true` if the form is valid, otherwise `false`.
   */
  onCancel(): void {
    this.isExportCancel = true;
    this.exportDrawerRef?.close();
    this.isExportSave = false;
    this.isExportCancel = false;
    this.fileList = [];
  }

  /**
   * Handles the file submited and block the automatic upload of the component
   * Note: Uploading will be stopped with false, hence false is returned always to stop uploading the file.
   * @param file NG-Zorro file
   */
  beforeUpload = (file: File): boolean => {
    this.fileList = [file];
    this.isFileUploading = false;
    return false;
  };

  private invokeExportJob(format: string, label: string, options: Record<string, string | number | boolean> = {}) {
    this.jobController.submitJobExtensionV2(this.projectId, format, options, this.fileList[0] as Blob).subscribe(jobId => {
      const remoteJob = {
        jobId: jobId as unknown as string,
        uiToken: EXPORT_TOKEN + format,
        // using hardcoded value as particular job will be moved to dedicated API can be removed then.
        autoDownloadResult: format === 'structural-functional-block' ? false : true,
        foreground: true,
        label
      };
      this.jobManager.register(remoteJob);
      this.onCancel();
    }, () => {
      this.onCancel();
    });
  }

  /**
   * Method is for fetching the exportDetail.
   */
  private fetchExportFormatsDetails(): void {
    const exportExtension: ExportFormatDescription[] = [];
    this.ioControllerService.getExportFormats(this.projectId).subscribe((exportFormatDetails: ExportFormatDescription[]) => {
      exportFormatDetails.forEach(element => {
        if (element.showOnExportPage.show) {
          exportExtension.push(element);
        }
      });
      const groupedExtension = exportExtension.reduce((newList, currentValue) => {
        newList[currentValue.showOnExportPage.category] = [...(newList[currentValue.showOnExportPage.category] || []),
        {
          id: currentValue.id, extensionType: currentValue.extensionType, label: currentValue.showOnExportPage.label,
          parameterDescriptions: currentValue.parameterDescriptions, uploadDescription: currentValue.uploadDescription
        }];
        return newList;
      }, {});
      Object.keys(groupedExtension).forEach((element) => {
        const obj: exportDetails = {
          'key': element,
          'value': groupedExtension[element]
        };
        this.exportExtDetails.push(obj);
      });
      this.exportExtDetails.sort((a: exportDetails, b: exportDetails) => a['key'].localeCompare(b['key']));
      this.exportExtDetails.forEach(element => {
        element.value.sort((a: groupedDetail, b: groupedDetail) => a['label'].localeCompare(b['label']));
      });
      // pushing the empty category value into the last.
      if (this.exportExtDetails.length > 1 && this.exportExtDetails[0].key === '') {
        this.exportExtDetails.push(this.exportExtDetails.shift());
      }
    });
  }

  private triggerExport(type: string | ExportFormatDescription, format: string, label: string, formData: Record<string, string | number | boolean>) {
    if (type === ExportFormatDescription.ExtensionTypeEnum.EXPORT_EXTENSION) {
      this.exportToFormat(format, formData);
    } else {
      this.invokeExportJob(format, label, formData);
    }
  }
}
