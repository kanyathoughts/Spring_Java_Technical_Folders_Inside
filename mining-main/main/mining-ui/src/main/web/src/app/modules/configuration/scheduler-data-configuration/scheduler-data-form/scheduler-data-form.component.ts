import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { UntypedFormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { IoControllerService, SchedulerImportPojoPrototype, SchedulerImporterControllerService } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { from, last, map, Subscription, switchMap } from 'rxjs';
const EXPORT_TOKEN = 'export-options-';

@Component({
  selector: 'mn-scheduler-data-form',
  templateUrl: './scheduler-data-form.component.html'
})
export class SchedulerDataFormComponent implements OnInit, OnDestroy {
  @Input() projectId: number;
  @Input() fileList: any;
  @Input() importedList: { [key: string]: string[] };
  @Output() isJobSuccessful = new EventEmitter<boolean>();
  exportForm: UntypedFormGroup;
  schedulerList: string[];
  isExportSave = false;
  isFileUploading = true;
  formModal: NzModalRef;
  selectedSchedulerType: string;
  selectedImporter: string;
  importSubscription: Subscription;

  constructor(
    private jobManager: JobManagerService,
    private fb: UntypedFormBuilder,
    private schedulerImporterController: SchedulerImporterControllerService,
    private ioControllerService: IoControllerService,
    private messageService: NzMessageService,
    private translateService: TranslateService
  ) { }

  ngOnInit(): void {
    this.schedulerList = Object.keys(this.importedList);
    this.selectedSchedulerType = this.schedulerList[0];
    this.selectedImporter = this.importedList[this.selectedSchedulerType][0];
    this.exportForm = this.fb.group({
      schedulerType: [this.selectedSchedulerType, [Validators.required]],
      importer: [this.selectedImporter, [Validators.required]],
      identifier: ['', [Validators.required]],
      description: [''],
      missingModules: [false],
      moduleRelationship: [false]
    });
  }

  /**
   * Submits the form and registers the job.
   */
  onSubmit(): void {
    this.isExportSave = true;
    this.importSubscription?.unsubscribe();
    const formData: SchedulerImportPojoPrototype = {
      schedulerType: this.exportForm.controls.schedulerType.value,
      identifier: this.exportForm.controls.identifier.value.trim(),
      description: this.exportForm.controls.description.value,
      importerUsed: this.exportForm.controls.importer.value
    };
    formData.properties = { createModuleIfMissing: false as unknown as object, establishModuleRelationship: false as unknown as object };
    if (this.exportForm.controls.missingModules.value) {
      formData.properties.createModuleIfMissing = true as unknown as object;
    }
    if (this.exportForm.controls.moduleRelationship.value) {
      formData.properties.establishModuleRelationship = true as unknown as object;
    }
    /* eslint-disable @typescript-eslint/no-unsafe-argument */
    this.importSubscription = this.ioControllerService.uploadFile(this.projectId, this.fileList[0] as Blob).pipe(switchMap((blob: any) => from(blob.text())),
      map((text: string) => JSON.parse(text))).subscribe((uid: string) => {
        this.formModal?.close('submitted');
        formData.source = uid;
        this.schedulerImporterController.createSchedulerImport(this.projectId, formData).subscribe(jobId => {
          const remoteJob = {
            jobId: jobId as unknown as string,
            uiToken: EXPORT_TOKEN + 'scheduler-import',
            autoDownloadResult: false,
            foreground: true,
            label: 'Scheduler'
          };
          this.jobManager.register(remoteJob).status$.pipe(last()).subscribe(() => {
            this.onjobSuccess();
            this.onCancel();
          });
        }, () => {
          this.onCancel();
          this.formModal?.close('');
          this.messageService.create('error', this.translateService.instant('schedulerDataComponent.importError'));
        });
      });
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

  /**
   * Handles the file deletion
   * @param file NG-Zorro file
   * @returns `true` if the file is deleted, otherwise `false`
   */
  onFileDelete = (file: File): boolean => {
    this.fileList.splice(this.fileList.indexOf(file), 1);
    return true;
  };

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
   * Method sets the modal on closing the modal
   */
  closeModal(): void {
    this.formModal?.destroy();
  }

  /**
   * Closes the export form without submitting.
   */
  onCancel(): void {
    this.isExportSave = false;
    this.fileList = [];
  }

  ngOnDestroy(): void {
    this.importSubscription?.unsubscribe();
  }

  private onjobSuccess(): void {
    this.isJobSuccessful.emit(true);
  }
}
