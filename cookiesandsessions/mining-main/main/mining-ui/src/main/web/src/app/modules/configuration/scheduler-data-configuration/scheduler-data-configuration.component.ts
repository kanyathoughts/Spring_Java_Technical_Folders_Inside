import { Component, Input, OnInit } from '@angular/core';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { MiningTableConfig } from '@app/shared/components/mining-table/mining-table-config.interface';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { NzModalService } from 'ng-zorro-antd/modal';
import { SchedulerDataModalComponent } from './scheduler-data-modal/scheduler-data-modal.component';
import { SchedulerDataFormComponent } from './scheduler-data-form/scheduler-data-form.component';
import { ProjectRole, SchedulerImporterControllerService } from '@innowake/mining-api-angular-client';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';


@Component({
  selector: 'mn-scheduler-data-configuration',
  templateUrl: './scheduler-data-configuration.component.html',
  styleUrl: './scheduler-data-configuration.component.less',
  providers: [CustomizableTableParametersService, CustomizableTableColumnService]
})
export class SchedulerDataConfigurationComponent implements OnInit {

  @Input() clientProjectData: ClientProjectRelationship;
  usage = Usages.SCHEDULERIMPORTTABLE;
  graphQlType = 'schedulerImports';
  pageType = TypeName.SchedulerImport;
  projectId: number;
  showInitialContent = true;
  fileList: any;
  isSchedulerDataPresent = false;
  tableConfig: MiningTableConfig;
  refreshCoreTable = false;
  isLoading = true;
  importedList: { [key: string]: string[] };
  canImportCSV = false;

  constructor(
    private graphQlService: GraphQlControllerService,
    private translateService: TranslateService,
    private modalService: NzModalService,
    private schedulerImporterController: SchedulerImporterControllerService,
    private authorizationService: KeycloakAuthorizationService
  ) { }

  ngOnInit(): void {
    this.projectId = this.clientProjectData.getProjectId();
    this.canImportCSV = this.authorizationService.hasUserRole(this.clientProjectData, ProjectRole.UserRoleEnum.MANAGER);
    this.fileList = [];
    const tableConfig: any = {};
    tableConfig.isExportVisible = false;
    if (this.canImportCSV) {
      tableConfig.isImportVisible = true;
      tableConfig.importToolTipText = this.translateService.instant('schedulerDataComponent.importFile');
      tableConfig.importAction = () => this.openSchedulerImportModal();
    }
    this.tableConfig = tableConfig;
    this.getSupportedImporters();
    this.uploadedSchedulerData();
  }

  /**
   * Handles the file submited and block the automatic upload of the component
   * @param file NG-Zorro file
   */
  beforeUpload = (file: File): boolean => {
    this.fileList = [file];
    this.showInitialContent = false;
    return false;
  };

  /**
   * Method to handle event on job completion
   * @param event status of the job
   */
  jobCompletionStatus(event: boolean): void {
    this.showInitialContent = true;
    this.isSchedulerDataPresent = event;
  }

  private getSupportedImporters(): void {
    this.schedulerImporterController.getSupportedImporters((this.projectId)).subscribe((response: { [key: string]: string[] }) => {
      this.importedList = response;
    });
  }

  private openSchedulerImportModal(): void {
    this.refreshCoreTable = false;
    const createModal = this.modalService.create<SchedulerDataModalComponent>({
      nzTitle: this.translateService.instant('schedulerDataComponent.importDataFile'),
      nzClosable: true,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzWrapClassName: 'vertical-center-modal',
      nzContent: SchedulerDataModalComponent,
    });
    createModal.afterClose.subscribe(modalData => {
      if (modalData.status === 'success') {
        const formModal = this.modalService.create<SchedulerDataFormComponent>({
          nzTitle: this.translateService.instant('schedulerDataComponent.importDataFile'),
          nzClosable: true,
          nzMaskClosable: false,
          nzKeyboard: true,
          nzAutofocus: null,
          nzCentered: true,
          nzWidth: 470,
          nzBodyStyle: { padding: '24px 0 0 0', height: '450px', overflow: 'auto' },
          nzWrapClassName: 'vertical-center-modal',
          nzContent: SchedulerDataFormComponent,
        });
        const instance = formModal.getContentComponent();
        instance.projectId = this.projectId;
        instance.fileList = modalData.file;
        instance.formModal = formModal;
        instance.importedList = this.importedList;
        formModal.afterClose.subscribe((status: string) => {
          if (status === 'submitted') {
            this.refreshCoreTable = true;
          }
        });
      } else {
        this.fileList = [];
      }
    });
  }

  private uploadedSchedulerData(): void {
    const requestQuery = {
      'query': `query {
        schedulerImports(projectId: ${this.projectId}) {
          content {
            identifier
            description
            schedulerType
          }
        }
      }`
    };
    this.graphQlService.graphQl(requestQuery).subscribe((response: any) => {
      if (response.data.schedulerImports?.content.length > 0) {
        this.isSchedulerDataPresent = true;
      }
      this.isLoading = false;
    }, () => {
      this.isLoading = false;
    });
  }
}
