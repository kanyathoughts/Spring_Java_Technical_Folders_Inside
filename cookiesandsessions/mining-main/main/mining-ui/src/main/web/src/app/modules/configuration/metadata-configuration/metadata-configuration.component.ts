import { HttpErrorResponse } from '@angular/common/http';
import { Component, Input, OnInit } from '@angular/core';
import { Logger } from '@app/core';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { JobControllerService } from '@innowake/mining-api-angular-client';
import { last } from 'rxjs/operators';
const log = new Logger('MetadataConfigurationComponent');

@Component ({
    selector: 'mn-metadata-configuration',
    templateUrl: './metadata-configuration.component.html'
  })
export class MetadataConfigurationComponent implements OnInit {

    @Input () clientProjectData: ClientProjectRelationship;
    projectId: number;
    disableImportExport: boolean;
    inputFile: File[] = [];
    fileSrc: string;
    importFileFormat = 'default';

    constructor(
      private jobManager: JobManagerService,
      private jobController: JobControllerService
    ) { }

    ngOnInit(): void {
      this.projectId = this.clientProjectData.getProjectId();
    }

    onExportMetadata(format: string): void {
      this.changeButtonsStatus();
      const extensionId = 'mining-metadata-export';
      this.jobController.submitJobExtensionV2(this.projectId, extensionId, {format}, undefined)
      .subscribe((response: string[]) => {
        this.getJobStatus(response.toString(), true);
      }, (error: HttpErrorResponse) => {
        log.error(error.message);
      });
    }

    beforeUpload = (file: File): boolean => {
      if (file.type === 'application/x-zip' || file.type === 'application/zip') {
        this.importFileFormat = 'compressed';
      } else {
        this.importFileFormat = 'default';
      }

      const reader = new FileReader();
      reader.readAsDataURL(file);
      reader.onload = () => {
        this.fileSrc = reader.result as string;
      };
      this.inputFile = this.inputFile.concat(file);
      this.importMetadata();
      return false;
    };

    private importMetadata(): void {
      this.changeButtonsStatus();
      const extentionId = 'mining-metadata-import';
      this.jobController.submitJobExtensionV2(this.projectId, extentionId, {importFormat : this.importFileFormat}, this.inputFile[0])
      .subscribe((response: string[]) => {
        this.getJobStatus(response.toString(), false);
      }, (error: HttpErrorResponse) => {
        log.error(error.message);
      });
    }

    private getJobStatus(jobId: string, autoDownload: boolean): void {
      const remoteJob = {
        jobId: jobId as unknown as string,
        autoDownloadResult: autoDownload,
        foreground: true,
        cancellable: true
      };
      this.jobManager.register(remoteJob).status$.pipe(last()).subscribe(() => {
        this.changeButtonsStatus();
        if ( ! autoDownload) {
          this.fileSrc = null;
          this.inputFile.splice(0, this.inputFile.length);
        }
      });
    }

    private changeButtonsStatus(): void {
      this.disableImportExport = ! this.disableImportExport;
    }
}
