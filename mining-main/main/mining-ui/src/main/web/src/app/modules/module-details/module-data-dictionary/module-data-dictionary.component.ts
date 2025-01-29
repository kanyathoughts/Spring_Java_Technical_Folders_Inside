import { Component, OnInit, Input, OnDestroy, Inject, ChangeDetectorRef } from '@angular/core';
import { Subject, Subscription } from 'rxjs';
import { TranslateService } from '@ngx-translate/core';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { AllowedTableActions, LinkType, MiningTableAction } from '@app/shared/components/mining-table/mining-table-action.interface';
import { BadgeCountUpdateOperation } from '../models/module-complexity-details';
import { ModuleBadgeUpdateService } from '@app/core/services/module-details/module-badge-update.service';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { SharedDataDictionaryEditorComponent } from '@app/shared/components/shared-data-dictionary-editor/shared-data-dictionary-editor.component';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { WindowToken } from '@app/core/utils/window';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { BulkAction, OptionItem } from '@app/shared/components/mining-table/mining-table-config.interface';
import { getHeaderTemplate } from '@app/shared/components/shared-editor-header/shared-editor-header.component';
import { WebAnnotationEditorComponent } from '@app/shared/components/web-annotation-editor/web-annotation-editor.component';
import { Logger } from '@app/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { switchMap } from 'rxjs/operators';
import { EDITOR_WIDTH } from '@app/shared/interfaces/annotation-editor.interface';
import { AnnotationControllerService, AnnotationPojo, DataDictionaryControllerService, DataDictionaryPojo, ModuleControllerService,
  ModulePojo, ProjectRole } from '@innowake/mining-api-angular-client';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { Entity } from '@app/core/services/clipboard.service';
const log = new Logger('AnnotationEditModal');

@Component({
  selector: 'mn-module-data-dictionary',
  templateUrl: './module-data-dictionary.component.html'
})
export class ModuleDataDictionaryComponent implements OnInit, OnDestroy {
  @Input() projectId: number;
  @Input() module: ModulePojo;
  @Input() moduleIdForPreFiltering: number[];
  dataDictionaryItems: Array<DataDictionaryPojo & { fieldType: string }> = [];
  errorMessage: string;
  modal: any;
  moduleName: string;
  usage: Usages =  Usages.DATADICTIONARYTABLE;
  graphQlType = 'dataDictionaries';
  pageType = TypeName.PAGEDATADICTIONARY;
  rowActions: OptionItem[][];
  internalDataPoints = [
    { name: 'id', path: 'content.id' },
    { name: 'uid', path: 'content.uid' },
    { name: 'path', path: 'content.module.path' },
    { name: 'id', path: 'content.module.id' },
    { name: 'offset', path: 'content.location.offset'}
  ];
  dataChangeEvent: Subject<MiningTableAction> = new Subject<MiningTableAction>();
  headerTitle: string;
  refreshCoreTable: boolean;
  entity = 'module-data-dictionary';
  tableConfig: any = {};
  allowSelectDeselectRows = false;
  private clientProjectSubscription: Subscription;
  private reloadSubscription: Subscription;
  constructor(
    @Inject(WindowToken) private $window: Window,
    private translateService: TranslateService,
    private authorizationService: KeycloakAuthorizationService,
    private relationshipService: ClientProjectRelationshipService,
    private moduleBadgeUpdateService: ModuleBadgeUpdateService,
    private dataDictionaryControllerService: DataDictionaryControllerService,
    private nzDrawerService: NzDrawerService,
    private annotationControllerService: AnnotationControllerService,
    private moduleControllerService: ModuleControllerService,
    public messageService: NzMessageService,
    private parametersService: CustomizableTableParametersService,
    private userCustomizableTable: CustomizableTableColumnService,
    private cdref: ChangeDetectorRef) {
  }

  ngOnInit(): void {
    this.entity = this.entity + '-' + this.module.id;
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
            { label: this.translateService.instant('iconToolTip.codeViewer'), value: 'codeviewer', disableItem: () => false },
            { label: this.translateService.instant('iconToolTip.openDataLineage'), value: 'dataLineage', disableItem: () => false}
          ]
        },
        ]];
        const bulkActions: BulkAction[] = [
          {
            label: this.translateService.instant('btnLabel.delete'),
            isDanger: true,
            action: (ddIds: number[]) => this.userCustomizableTable.bulkDelete(ddIds, 'Data Dictionary', this)
          },
          {
            label: this.translateService.instant('btnLabel.generateFB'),
            isDanger: false,
            action: (data: Entity[]) => this.userCustomizableTable.bulkFbGeneration(data, 'Data Dictionary', this)
          },
        ];
        this.tableConfig.bulkActions = bulkActions;
        this.allowSelectDeselectRows = true;
      }
    });
    this.cdref.detectChanges();
  }

  /**
   * Opens the Data dictionary Details and navigate to codeview page.
   * @param modalHeader Modal header.
   * @param rowData data of the selected option.
   */
  handleSelectedOption(rowData: MiningTableOptionSelected): void {
    const moduleId: number = this.module.id;
    switch (rowData.optionValue) {
      case 'codeviewer': {
        const path = 'code-viewer?offset=' + rowData.data.location.offset;
        openInNewTab(this.projectId, moduleId, path, this.$window);
        break;
      }
      case 'edit': {
        this.dataDictionaryControllerService.findDataDictionaryEntryByRecordId(this.projectId, moduleId, rowData.data.uid as string)
          .subscribe((dataDictionaryItem: DataDictionaryPojo) => {
            if (dataDictionaryItem) {
              this.editDataDictionary(dataDictionaryItem);
            }
          });
        break;
      }
      case 'linkedBusinessVariables':
      const selectedAnnotationId: number = rowData?.data?.selectedAnnotationId;
      this.openSharedAnnotationEditorModal(selectedAnnotationId, moduleId);
      break;
      case 'dataLineage':
        this.dataDictionaryControllerService.findDataDictionaryEntryByRecordId(this.projectId, moduleId, rowData.data.uid as string)
        .subscribe((dataDictionaryItem: DataDictionaryPojo) => {
          if (dataDictionaryItem) {
            let path = 'data-lineage?offset=' + dataDictionaryItem.location.offset + '&field=' + dataDictionaryItem.dataElementName;
            if (dataDictionaryItem.moduleId !== moduleId) {
              path += '&includingModule=' + this.module.linkHash;
            }
            openInNewTab(this.projectId, dataDictionaryItem.moduleId, path, this.$window);
          }
        });
      break;
    }
  }

  /**
   * Opens the Data dictionary Details in the Modal with option to update/Delete.
   * @param modalHeader Modal header.
   * @param rowData data of the selected option.
   */
  editDataDictionary(data: DataDictionaryPojo): void {
    let dataDictionaryItem: DataDictionaryPojo;
    this.dataDictionaryControllerService.findDataDictionaryEntryByRecordId(this.projectId, this.module.id, data?.uid).pipe(
      switchMap((dataDictionaryResp: DataDictionaryPojo) => {
        dataDictionaryItem = dataDictionaryResp;
        return this.dataDictionaryControllerService.findLinkedBusinessVariables(this.projectId, this.module.id, dataDictionaryResp?.uid);
      })).subscribe((codeAnnotationEditorItems: AnnotationPojo[]) => {
        if (dataDictionaryItem) {
          const headerTitle: string = this.translateService.instant('sharedDataDictionaryEditor.headerTitle',
            { moduleName: this.module.name }
          );
          this.nzDrawerService.create({
            nzTitle: getHeaderTemplate(headerTitle, this.module.path),
            nzContent: SharedDataDictionaryEditorComponent,
            nzContentParams: {
              dataDictionaryItem,
              moduleId: this.module.id,
              modulePath: this.module.path,
              codeAnnotationEditorItems
            },
            nzWidth: codeAnnotationEditorItems.length > 0 ? EDITOR_WIDTH.editorWIthAnnotations : EDITOR_WIDTH.editorWithoutAnnotations,
            nzPlacement: 'right',
            nzClosable: true,
            nzMaskClosable: false
          }).afterClose.subscribe((editorAction: MiningTableAction) => {
            this.handleFormResult(editorAction);
          });
        }
      });
  }

  /**
   * Closes the shared data dictionary editor.
   * @param editorAction the action to be performed, i.e, cancel or delete
   */
  handleFormResult(editorAction: MiningTableAction): void {
    this.refreshCoreTable = false;
    if (editorAction?.action === AllowedTableActions.DELETE) {
      this.dataChangeEvent.next({ id: editorAction.data.id, action: AllowedTableActions.DELETE });
      this.dataDictionaryItems.length = this.dataDictionaryItems.length > 0 ? this.dataDictionaryItems.length - 1 : this.dataDictionaryItems.length;
      this.moduleBadgeUpdateService.updateAnnotationDataDictionary({operation: BadgeCountUpdateOperation.DATA_DICTIONARY_DELETED, count: 1});
      this.refreshCoreTable = true;
    } else if (editorAction?.action === AllowedTableActions.UPDATE) {
      this.dataChangeEvent.next({ id: editorAction.data.id, action: AllowedTableActions.UPDATE, data: editorAction.data });
      this.refreshCoreTable = true;
    }
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
    this.reloadSubscription?.unsubscribe();
  }

  private openSharedAnnotationEditorModal(annotationId: number, moduleId: number): void {
    this.refreshCoreTable = false;
    this.annotationControllerService.findAnnotationById(this.projectId, annotationId).subscribe(
      (annotationToBeUpdated: AnnotationPojo) => {
        if ( ! annotationToBeUpdated) {
          return;
        }
        this.moduleControllerService.findModuleById(this.projectId, moduleId, true).subscribe((module: ModulePojo) => {
          const modulePath: string = this.translateService.instant('annotationReporting.sharedAnnotationEditorPath', { modulePath: module?.path });
          const annotationTitle: string = this.translateService.instant(
            'annotationReporting.sharedAnnotationEditorTitle',
            { moduleName: annotationToBeUpdated?.moduleName }
          );
          this.nzDrawerService.create({
            nzTitle: getHeaderTemplate(annotationTitle, modulePath),
            nzContent: WebAnnotationEditorComponent,
            nzContentParams: {
              annotation: annotationToBeUpdated,
              moduleDetails: module
            },
            nzWidth: '80vw',
            nzPlacement: 'right',
            nzClosable: true,
            nzMaskClosable: false
          }).afterClose.subscribe((editorAction: AllowedTableActions) => {
            if (editorAction === AllowedTableActions.UPDATE || editorAction === AllowedTableActions.DELETE) {
             this.refreshCoreTable = true;
            }
          });
        });
      },
      (error: any) => {
        this.messageService.error(`${this.translateService.instant('errorFetchingDetails',
          { tableType: this.translateService.instant('annotationReporting.tableTile') })}`);
        log.error(`Error Occured: ${error.message}`);
      }
    );
  }

}
