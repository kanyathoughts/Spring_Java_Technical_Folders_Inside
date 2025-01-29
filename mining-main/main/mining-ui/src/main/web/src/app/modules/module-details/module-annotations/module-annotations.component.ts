import { AfterContentChecked, ChangeDetectorRef, Component, Inject, Input, OnDestroy, OnInit, TemplateRef, ViewChild } from '@angular/core';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { AllowedTableActions, LinkType, MiningTableAction } from '@app/shared/components/mining-table/mining-table-action.interface';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { Subject, Subscription } from 'rxjs';
import { WebAnnotationEditorComponent } from '@app/shared/components/web-annotation-editor/web-annotation-editor.component';
import { TranslateService } from '@ngx-translate/core';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { WindowToken } from '@app/core/utils/window';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { BulkAction, OptionItem } from '@app/shared/components/mining-table/mining-table-config.interface';
import { getHeaderTemplate } from '@app/shared/components/shared-editor-header/shared-editor-header.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { Logger } from '@app/core';
import { HttpErrorResponse } from '@angular/common/http';
import { switchMap } from 'rxjs/operators';
import { SharedDataDictionaryEditorComponent } from '@app/shared/components/shared-data-dictionary-editor/shared-data-dictionary-editor.component';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { ClipboardService, Entity } from '@app/core/services/clipboard.service';
import { EDITOR_WIDTH } from '@app/shared/interfaces/annotation-editor.interface';
import { SharedAnnotationEditorService } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.service';
import { AnnotationControllerService, AnnotationPojo, DataDictionaryControllerService, DataDictionaryPojo, EntityId, ModuleControllerService,
  ModulePojo, ProjectRole } from '@innowake/mining-api-angular-client';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
const log = new Logger('AnnotationEditModal');

@Component({
  selector: 'mn-module-annotations',
  templateUrl: './module-annotations.component.html'
})
export class ModuleAnnotationsComponent implements OnInit, OnDestroy, AfterContentChecked {
  @ViewChild('extraTemplate', { static: true }) extraTemplateRef: TemplateRef<any>;
  @Input() moduleId: EntityId;
  @Input() projectId: number;
  @Input() module: ModulePojo;
  moduleName: string;
  usage: Usages = Usages.ANNOTATIONTABLE;
  graphQlType = 'annotations';
  pageType = TypeName.PAGEANNOTATION;
  codeAnnotationMaps: { [key: string]: AnnotationPojo[] };
  errorMessage: string;
  dataChangeEvent: Subject<MiningTableAction> = new Subject<MiningTableAction>();
  rowActions: OptionItem[][];
  enableOrderedBusinessRuleCsv = true;
  internalDataPoints = [
    { name: 'id', path: 'content.id' },
    { name: 'name', path: 'content.module.name' },
    { name: 'id', path: 'content.module.id' },
    { name: 'offset', path: 'content.offset' }
  ];
  annotationToBeUpdated: AnnotationPojo;
  refreshCoreTable = false;
  entity = 'module-annotation';
  tableConfig: any = {};
  allowSelectDeselectRows = false;
  private clientProjectSubscription: Subscription;
  private reloadSubscription: Subscription;

  constructor(@Inject(WindowToken) private $window: Window,
    private relationshipService: ClientProjectRelationshipService,
    private authorizationService: KeycloakAuthorizationService,
    private translateService: TranslateService,
    private annotationControllerService: AnnotationControllerService,
    public messageService: NzMessageService,
    private nzDrawerService: NzDrawerService,
    private moduleControllerService: ModuleControllerService,
    private dataDictionaryControllerService: DataDictionaryControllerService,
    private parametersService: CustomizableTableParametersService,
    private clipBoardService: ClipboardService,
    private sharedAnnotationEditorService: SharedAnnotationEditorService,
    private userCustomizableTable: CustomizableTableColumnService,
    private cdref: ChangeDetectorRef
  ) { }

  ngAfterContentChecked(): void {
    this.cdref.detectChanges();
  }

  ngOnInit(): void {
    this.entity = this.entity + '-' + this.moduleId;
    this.reloadSubscription = this.parametersService.reloadTableDataFlag$.subscribe(value => {
      if (value) {
        this.refreshCoreTable = true;
       this.parametersService.setReloadTableDataValue(false);
      }
     });
    this.moduleName = this.module.name;
    this.clientProjectSubscription = this.relationshipService.getClientProjectObservable().subscribe(currentClient => {
      if (this.authorizationService.hasUserRole(currentClient, ProjectRole.UserRoleEnum.EDITOR)) {
        this.rowActions = [[{ label: 'btnLabel.edit', value: 'edit' },
        {
          type: LinkType.DROPDOWN,
          icon: 'more',
          options: [
            { label: this.translateService.instant('iconToolTip.codeViewer'), value: 'codeviewer', disableItem: (): boolean => false },
            { label: this.translateService.instant('btnLabel.copy'), value: 'copyAnnotationDetails', disableItem: (): boolean => false },
          ]
        }]];
        const bulkActions: BulkAction[] = [
          {
            label: this.translateService.instant('btnLabel.delete'),
            isDanger: true,
            action: (annotationIds: number[]) => this.userCustomizableTable.bulkDelete(annotationIds, 'Annotation', this)
          }
        ];
        this.tableConfig.bulkActions = bulkActions;
        this.allowSelectDeselectRows = true;
        this.cdref.detectChanges();
      }
    });
  }

  /**
   * Method to be called after annotation editor is closed
   * @param annotation details of the selected option.
   * @param resultAction of annotation editor
   */
  afterCloseAnnotationEditor(annotation: AnnotationPojo, resultAction: AllowedTableActions): void {
    if ( ! resultAction) {
      return;
    }
    if (resultAction === AllowedTableActions.UPDATE) {
      this.dataChangeEvent.next({ id: annotation.id, action: resultAction, data: annotation });
    }
    this.refreshCoreTable = true;
  }

  /**
   * Opens the Annotation Details in the Modal with option to update/Delete.
   * @param rowData data of the selected option.
   */
  editCodeAnnotation(rowData: MiningTableOptionSelected): void {
    const annotationReport: AnnotationPojo = rowData.data;
    const annotationId: number = annotationReport.id;
    switch (rowData.optionValue) {
      case 'codeviewer':
        const path = 'code-viewer?offset=' + rowData.data.offset;
        openInNewTab(this.projectId, this.module.id, path, this.$window);
        break;
      case 'edit':
        this.refreshCoreTable = false;
        this.annotationControllerService.findAnnotationById(this.projectId, annotationId).subscribe(
          (annotation: AnnotationPojo) => {
            if ( ! annotation) {
              return;
            }
            this.annotationToBeUpdated = annotation;
            this.sharedAnnotationEditorService.setEditorState(true);
           const drawer = this.nzDrawerService.create({
              nzTitle: this.extraTemplateRef,
              nzContent: WebAnnotationEditorComponent,
              nzContentParams: {
                annotation: this.annotationToBeUpdated,
                moduleDetails: this.module
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
          },
          (error: HttpErrorResponse) => {
            this.sharedAnnotationEditorService.setEditorState(false);
            this.messageService.error(`${this.translateService.instant('errorFetchingDetails',
              { tableType: this.translateService.instant('annotationReporting.tableTile') })}`);
            log.error(`Error Occured: ${error.message}`);
          }
        );
        break;
      case 'linkedBusinessVariables':
        const selectedRowAnnotation: string = rowData?.data?.selectedDDId;
        this.openSharedDataDictionaryEditorModal(this.moduleId, selectedRowAnnotation);
        break;
      case 'copyAnnotationDetails':
        this.clipBoardService.copyToClipboard(Entity.ANNOTATION, annotationId);
        break;
    }
  }

  /**
   * Check if the annotation form is dirty or not.
   */
  isFormDirty(): boolean {
    return !this.sharedAnnotationEditorService.getEditorFormState();
  }

  /**
   * Closes the currently open annotation editor.
   */
  onCancel(): void {
    this.sharedAnnotationEditorService.closeDrawer();
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
    this.reloadSubscription?.unsubscribe();
  }

  private openSharedDataDictionaryEditorModal(moduleId: EntityId, recordId: string): void {
    let dataDictionaryItem: DataDictionaryPojo;
    let modulePath: string;
    this.moduleControllerService.findModuleById(this.projectId, moduleId, true).subscribe((module: ModulePojo) => {
      modulePath = this.translateService.instant('annotationReporting.sharedAnnotationEditorPath', { modulePath: module?.path });
    });
    this.dataDictionaryControllerService.findDataDictionaryEntryByRecordId(this.projectId, moduleId, recordId).pipe(
      switchMap((dataDictionaryRes: DataDictionaryPojo) => {
        dataDictionaryItem = dataDictionaryRes;
        if (dataDictionaryRes.annotations.length > 0) {
          return this.dataDictionaryControllerService.findLinkedBusinessVariables(this.projectId, moduleId, recordId);
        }
      })).subscribe((linkedAnnotations: AnnotationPojo[]) => {
        let codeAnnotationEditorItems: any[] = [];
        codeAnnotationEditorItems = linkedAnnotations;
        if (dataDictionaryItem) {
          const headerTitle: string = this.translateService.instant('sharedDataDictionaryEditor.headerTitle',
            { moduleName: this.module.name }
          );
          this.nzDrawerService.create({
            nzTitle: getHeaderTemplate(headerTitle, modulePath),
            nzContent: SharedDataDictionaryEditorComponent,
            nzContentParams: {
              dataDictionaryItem,
              moduleId,
              modulePath,
              codeAnnotationEditorItems
            },
            nzWidth: codeAnnotationEditorItems ? EDITOR_WIDTH.editorWIthAnnotations : EDITOR_WIDTH.editorWithoutAnnotations,
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
}
