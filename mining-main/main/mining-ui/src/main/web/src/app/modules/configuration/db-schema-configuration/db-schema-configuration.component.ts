import { Component, Input, OnInit } from '@angular/core';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { HttpErrorResponse } from '@angular/common/http';
import { DataSchemaControllerService, JobInformation, SchemaInfoPojo } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'mn-db-schema-configuration',
  templateUrl: './db-schema-configuration.component.html',
  styleUrls: ['./db-schema-configuration.component.less']
})
export class DbSchemaConfigurationComponent implements OnInit {

  @Input() clientProjectData: ClientProjectRelationship;
  projectId: number;
  importFile: File;
  importData: any;
  importError: any;
  importedSchemas: string[];
  selectedSchemas = new Set<string>();
  existingSchemas: SchemaInfoPojo[];
  uploading = false;
  showCrawlerModal = false;

  constructor(
    private dataSchemaControllerService: DataSchemaControllerService,
    private jobManager: JobManagerService
  ) { }

  ngOnInit(): void {
    this.projectId = this.clientProjectData.getProjectId();
    this.refreshSchemas();
  }

  refreshSchemas(): void {
    this.dataSchemaControllerService.findSchemas(this.projectId).subscribe({
      next: (result) => this.existingSchemas = result
    });
  }

  getModuleDetailRoute(moduleId: number): string {
    return RouteBuilder.buildModuleRoute(this.projectId, moduleId, 'details');
  }

  beforeUpload = (file: File): boolean => {
    this.importError = null;
    this.importedSchemas = null;
    this.selectedSchemas.clear();
    this.importData = {};
    this.importFile = file;
    const reader = new FileReader();
    reader.onload = (e) => {
      try {
        this.importData = JSON.parse(e.target.result as string);
        if (this.importData.schemes == null) {
          this.importData = null;
        }
      } catch (e) {
        this.importData = null;
      }
    };
    reader.readAsText(file);
    return false;
  };

  onSchemaChecked(name: string, checked: boolean): void {
    if (checked) {
      this.selectedSchemas.add(name);
    } else {
      this.selectedSchemas.delete(name);
    }
  }

  onSchemaCheckedAll(checked: boolean): void {
    if (checked) {
      this.importData.schemes.forEach((e: any) => this.selectedSchemas.add(e.name as string));
    } else {
      this.selectedSchemas.clear();
    }
  }

  /**
   * uploads selected schemas to db
   */
  doUpload(): void {
    this.uploading = true;
    const rq = { schemes: [] as any[] };
    this.importData.schemes.forEach((e: any) => {
      if (this.selectedSchemas.has(e.name as string)) {
        rq.schemes.push(e);
      }
    });
    this.dataSchemaControllerService.importSchemas(this.projectId, rq).subscribe({
      next: (response: string[]) => this.checkImportJob(response.toString()),
      error: (error: HttpErrorResponse) => this.onImportComplete(error)
    });
  }

  private checkImportJob(response: string): void {
    const remoteJob = {
      jobId: response,
      foreground: true
    };
    this.jobManager.register(remoteJob).status$.subscribe((result: string) => {
      if (result === JobInformation.StatusEnum.SUCCESS) {
        this.onImportComplete();
        this.refreshSchemas();
      } else if (result === JobInformation.StatusEnum.FAILURE) {
        this.onImportComplete();
      }
    });
  }

  private onImportComplete(error?: HttpErrorResponse): void {
    if (error) {
      this.importError = error;
    }
    this.importedSchemas = Array.from(this.selectedSchemas);
    this.importFile = null;
    this.importData = null;
    this.uploading = false;
  }

}
