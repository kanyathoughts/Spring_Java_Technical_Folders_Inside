import { AfterContentChecked, ChangeDetectorRef, Component, Inject, OnDestroy, OnInit,ViewChild,TemplateRef } from '@angular/core';
import { Logger } from '@app/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { AllowedTableActions, LinkType, MiningTableAction } from '@app/shared/components/mining-table/mining-table-action.interface';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { WebAnnotationEditorComponent } from '@app/shared/components/web-annotation-editor/web-annotation-editor.component';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { ActivatedRoute, Router } from '@angular/router';
import { BaseReportingPageComponent } from '@app/shared/components/base-customizable-table/base-reporting-page.component';
import { getHeaderTemplate } from '@app/shared/components/shared-editor-header/shared-editor-header.component';
import { NzModalService } from 'ng-zorro-antd/modal';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { WindowToken } from '@app/core/utils/window';
import { SharedDataDictionaryEditorComponent } from '@app/shared/components/shared-data-dictionary-editor/shared-data-dictionary-editor.component';
import { switchMap } from 'rxjs/operators';
import { HttpErrorResponse } from '@angular/common/http';
import { ClipboardService, Entity } from '@app/core/services/clipboard.service';
import { EDITOR_WIDTH } from '@app/shared/interfaces/annotation-editor.interface';
import { SharedAnnotationEditorService } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.service';
import { last } from 'rxjs/internal/operators/last';
import {
  AnnotationControllerService,
  AnnotationPojo,
  DataDictionaryControllerService,
  DataDictionaryPojo,
  DataPointControllerService,
  EntityId,
  JobControllerService,
  ModuleControllerService,
  ModulePojo,
  ProjectRole,
  SavedSearchControllerService
} from '@innowake/mining-api-angular-client';
import { AnnotationsImportModalComponent } from './annotations-import-modal/annotations-import-modal.component';
import { BulkAction } from '@app/shared/components/mining-table/mining-table-config.interface';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { Observable } from 'rxjs';

const log = new Logger('AnnotationEditModal');

@Component({
  selector: 'annotation-reporting',
  templateUrl: '../../shared/components/base-customizable-table/base-reporting-page.component.html'
})
export class AnnotationReportingComponent extends BaseReportingPageComponent implements OnInit, OnDestroy, AfterContentChecked {
  @ViewChild('extraTemplate', { static: true }) extraTemplateRef: TemplateRef<any>;
  annotationToBeUpdated: AnnotationPojo;

  internalDataPoints = [
    { name: 'id', path: 'content.id' },
    { name: 'id', path: 'content.module.id' },
    { name: 'path', path: 'content.module.path' },
    { name: 'offset', path: 'content.offset' }
  ];

  pageTitle = this.translateService.instant('annotationReporting.label');

  selectedFilter = this.translateService.instant('annotationReporting.label');

  usage: Usages = Usages.ANNOTATIONTABLE;

  pageType = TypeName.PAGEANNOTATION;

  graphQlType = 'annotations';

  errorMessage: string = this.translateService.instant('errorFetchingDetails', { tableType: 'Annotation' });

  canCopyPasteAnnotation = false;

  modalCloseIcon = true;
  annotationIdArray: number[] = [];

  constructor(
    public messageService: NzMessageService,
    public translateService: TranslateService,
    public relationshipService: ClientProjectRelationshipService,
    public userCustomizableTable: CustomizableTableColumnService,
    public graphQlControllerService: GraphQlControllerService,
    public route: ActivatedRoute,
    public dataPointControllerService: DataPointControllerService,
    public savedSearchControllerService: SavedSearchControllerService,
    public modalService: NzModalService,
    public parametersService: CustomizableTableParametersService,
    public router: Router,
    public cd: ChangeDetectorRef,
    public notification: NzNotificationService,
    public sharedAnnotationEditorService: SharedAnnotationEditorService,
    @Inject(WindowToken) private $window: Window,
    private annotationControllerService: AnnotationControllerService,
    private authorizationService: KeycloakAuthorizationService,
    private nzDrawerService: NzDrawerService,
    private moduleControllerService: ModuleControllerService,
    private dataDictionaryControllerService: DataDictionaryControllerService,
    private clipBoardService: ClipboardService,
    private jobManagerService: JobManagerService,
    private jobControllerService: JobControllerService
  ) {
    super(messageService, relationshipService, userCustomizableTable, graphQlControllerService, route,
      dataPointControllerService, savedSearchControllerService, translateService, parametersService, router,
      modalService, notification, cd, sharedAnnotationEditorService);
  }

  ngOnInit(): void {
    this.reloadSubscription = this.parametersService.reloadTableDataFlag$.subscribe(value => {
     if (value) {
      this.refreshCoreTable = true;
      this.parametersService.setReloadTableDataValue(false);
     }
    });
    this.clientProjectSubscription = this.initialize().subscribe((currentClient) => {
      if (this.authorizationService.hasUserRole(currentClient, ProjectRole.UserRoleEnum.EDITOR)) {
        this.canCopyPasteAnnotation = true;
        this.rowActions = [[{ label: 'btnLabel.edit', value: 'edit' },
        {
          type: LinkType.DROPDOWN,
          icon: 'more',
          options: [
            { label: this.translateService.instant('iconToolTip.codeViewer'), value: 'codeviewer', disableItem: (): boolean => false },
            { label: this.translateService.instant('btnLabel.copy'), value: 'copyAnnotationDetails', disableItem: (): boolean => false},
          ]
        }]];
        const bulkActions: BulkAction[] = [
          {
             label: this.translateService.instant('bulkActionButtonLbls.generate'),
             tooltip: this.translateService.instant('genAI.contentWarning'),
             class: 'gen-Ai__generateBtn',
             icon: 'mining-icons:gen-ai-stars',
             action: (data: Set<number>) => this.identifyWebBasedAnalyses(data, 'explainAnnotations')
          },
          {
            label: this.translateService.instant('btnLabel.delete'),
            isDanger: true,
            action: (annotationIds: number[]) => this.userCustomizableTable.bulkDelete(annotationIds, 'Annotation', this)
          }
        ];
        this.tableConfig.bulkActions = bulkActions;
        this.allowSelectDeselectRows = true;
      } else {
        this.rowActions = [];
      }

      if (this.authorizationService.hasUserRole(currentClient, ProjectRole.UserRoleEnum.MANAGER)) {
            const tableConfig: any = {};
            tableConfig.isImportVisible = true;
            tableConfig.importToolTipText = this.translateService.instant('annotationReporting.importAnnotations');
            tableConfig.importAction = () => this.importAnnotationAction();
            this.tableConfig = tableConfig;
        }
    });
  }

  ngAfterContentChecked(): void {
    this.cd.detectChanges();
  }

  /**
   * Method to be called after annotation editor is closed
   * @param annotationReport rowData data of the selected option.
   * @param action of annotation editor
   */
  afterCloseAnnotationEditor(annotationReport: MiningTableOptionSelected['data'], action: AllowedTableActions): void {
    /** When editor is closed with Cancel button */
    if (!action) {
      return;
    }
    annotationReport.name = this.annotationToBeUpdated.moduleName;
    annotationReport.annotationState = this.annotationToBeUpdated.state;
    annotationReport.updatedByUserId = this.annotationToBeUpdated.updatedByUserId;
    annotationReport.sourceCode = this.annotationToBeUpdated.sourceAttachment;
    annotationReport.categoryName = this.annotationToBeUpdated.categoryName;
    annotationReport.annotationState = this.annotationToBeUpdated.state;
    annotationReport.annotationType = this.annotationToBeUpdated.type;
    annotationReport.englishTranslation = this.annotationToBeUpdated.englishTranslation;
    this.dataChangeEvent.next({ id: annotationReport.id, action, data: annotationReport });
    this.refreshCoreTable = true;
  }

  /**
   * Opens the Annotation Details in the Modal with option to update/Delete.
   * @param rowData data of the selected option.
   */
  handleSelectedOption(rowData: MiningTableOptionSelected): void {
    const annotationId: number =  rowData?.data?.id;
    const moduleId: number = rowData?.data?.module?.id;
    switch (rowData.optionValue) {
      case 'codeviewer':
        const path = 'code-viewer?offset=' + rowData.data.offset;
        openInNewTab(this.projectId, moduleId, path, this.$window);
        break;
      case 'edit':
        this.openSharedAnnotationEditorModal(annotationId, moduleId, rowData.data);
        break;
      case 'linkedBusinessVariables':
        const modulePath: string = rowData?.data?.module.path;
        const moduleName: string = rowData?.data?.module.name;
        const selectedBusinessRule: EntityId = rowData?.data?.selectedDDId;
        this.openSharedDataDictionaryEditorModal(moduleId, modulePath, moduleName, selectedBusinessRule);
        break;
      case 'copyAnnotationDetails':
        this.clipBoardService.copyToClipboard(Entity.ANNOTATION, annotationId);
        break;
    }
  }

  ngOnDestroy(): void {
    this.reloadSubscription?.unsubscribe();
    this.destroy();
  }

  /**
   * Opens the Annotation Import in the Modal.
   */
  importAnnotationAction(): void {
    /* setting refresh to false so that on completion changing it to true will trigger refresh */
    this.refreshCoreTable = false;
    const createModal = this.modalService.create<AnnotationsImportModalComponent>({
      nzClosable: false,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzWrapClassName: 'vertical-center-modal',
      nzContent: AnnotationsImportModalComponent
    });
    const instance = createModal.getContentComponent();
    instance.projectId = this.projectId;
    createModal.afterClose.subscribe((result: string) => {
      if (result === 'done') {
        this.refreshCoreTable = true;
      } else if( result === 'retry') {
        this.importAnnotationAction();
      }
    });
  }
  /**
   * identify web based analyses based on selected job type
   * @param ids gives the set of module id
   * @param jobType gives the type of job that user wants perform
   */
  identifyWebBasedAnalyses(ids: Set<number>, jobType: string): void {
      this.annotationIdArray = [];
      ids.forEach((element: number) => {
        this.annotationIdArray.push(element);
      });
      let identifyRequest: Observable<string[]>;
      if (jobType  === 'explainAnnotations') {
         this.annotationConfirmVisible = true;
      }
      identifyRequest?.subscribe((response: string[]) => {
        const jobProgressId = response.toString();
        this.jobManagerService.register({ jobId: jobProgressId, foreground: true, cancellable: true });
      });
  }

  handleAnnotationsModalOk(): void {
    this.jobControllerService.submitJobExtensionV2(this.projectId, 'generate-annotation-descriptions',
      { 'ids': this.annotationIdArray, 'overwrite': this.explainAnnotationsOverwrite })
      .subscribe((response: string[]) => {
        this.registerJob(response.toString(), false);
      }, (error: HttpErrorResponse) => {
        log.error(error.message);
      });
    this.annotationConfirmVisible = false;
  }

  private openSharedAnnotationEditorModal(annotationId: number, moduleId: number, annotationReport: any): void {
    this.annotationControllerService.findAnnotationById(this.projectId, annotationId).subscribe(
      (annotation: AnnotationPojo) => {
        if ( ! annotation) {
          return;
        }
        this.moduleControllerService.findModuleById(this.projectId, moduleId, true).subscribe((module: ModulePojo) => {
          this.sharedAnnotationEditorService.setEditorState(true);
          this.refreshCoreTable = false;
          this.annotationToBeUpdated = annotation;
          this.moduleName = annotation?.moduleName;
           const drawer = this.nzDrawerService.create({
            nzTitle: this.extraTemplateRef,
            nzContent: WebAnnotationEditorComponent,
            nzContentParams: {
              annotation: this.annotationToBeUpdated,
              moduleDetails: module,
              canCopyPasteAnnotation: this.canCopyPasteAnnotation
            },
            nzWidth: '80vw',
            nzPlacement: 'right',
            nzClosable: false,
            nzMaskClosable: false
          });
          this.sharedAnnotationEditorService.setDrawerRef(drawer);
          drawer.afterClose.subscribe((action: AllowedTableActions) => {
            this.afterCloseAnnotationEditor(annotationReport, action);
            this.sharedAnnotationEditorService.setEditorState(false);
          });
        });
      },
      (error: HttpErrorResponse) => {
        this.messageService.error(`${this.translateService.instant('errorFetchingDetails',
          { tableType: this.translateService.instant('annotationReporting.tableTile') })}`);
        log.error(`Error Occured: ${error.message}`);
      }
    );
  }

  private openSharedDataDictionaryEditorModal(moduleId: number, modulePath: string, moduleName: string, recordId: EntityId): void {
    let dataDictionaryItem: DataDictionaryPojo;
    const modulePathTitle: string = this.translateService.instant('annotationReporting.sharedAnnotationEditorPath', { modulePath });
    this.dataDictionaryControllerService.findDataDictionaryEntryByRecordId(this.projectId, moduleId, recordId).pipe(
      switchMap((dataDictionaryRes) => {
        dataDictionaryItem = dataDictionaryRes;
        return this.dataDictionaryControllerService.findLinkedBusinessVariables(this.projectId, moduleId, recordId);
      })).subscribe((linkedAnnotations: AnnotationPojo[]) => {
        if (dataDictionaryItem) {
          this.refreshCoreTable = false;
          const headerTitle: string = this.translateService.instant('sharedDataDictionaryEditor.headerTitle',
            { moduleName }
          );
          this.nzDrawerService.create({
            nzTitle: getHeaderTemplate(headerTitle, modulePathTitle),
            nzContent: SharedDataDictionaryEditorComponent,
            nzContentParams: {
              dataDictionaryItem,
              moduleId,
              modulePath,
              codeAnnotationEditorItems: linkedAnnotations
            },
            nzWidth: linkedAnnotations.length > 0 ? EDITOR_WIDTH.editorWIthAnnotations : EDITOR_WIDTH.editorWithoutAnnotations,
            nzPlacement: 'right',
            nzClosable: true,
            nzMaskClosable: false
          }).afterClose.subscribe((editorAction: MiningTableAction) => {
            if (editorAction?.action === AllowedTableActions.UPDATE || editorAction?.action === AllowedTableActions.DELETE) {
              this.refreshCoreTable = true;
            }
          });
        }
      });
  }

  private registerJob(jobId: string, autoDownload: boolean): void {
    const remoteJob = {
      jobId: jobId as unknown as string,
      autoDownloadResult: autoDownload,
      foreground: true,
      cancellable: true
    };
    this.jobManagerService.register(remoteJob).status$.pipe(last()).subscribe(() => {
    });
  }

}
