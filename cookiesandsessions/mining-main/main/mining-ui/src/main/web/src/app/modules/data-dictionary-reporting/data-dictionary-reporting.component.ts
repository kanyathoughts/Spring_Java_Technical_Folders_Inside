import { Component, OnDestroy, OnInit, Inject, ChangeDetectorRef } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { BaseReportingPageComponent } from '@app/shared/components/base-customizable-table/base-reporting-page.component';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { AllowedTableActions, LinkType, MiningTableAction } from '@app/shared/components/mining-table/mining-table-action.interface';
import { SharedDataDictionaryEditorComponent } from '@app/shared/components/shared-data-dictionary-editor/shared-data-dictionary-editor.component';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { getHeaderTemplate } from '@app/shared/components/shared-editor-header/shared-editor-header.component';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { WindowToken } from '@app/core/utils/window';
import { NzModalService } from 'ng-zorro-antd/modal';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { WebAnnotationEditorComponent } from '@app/shared/components/web-annotation-editor/web-annotation-editor.component';
import { Logger } from '@app/core';
import { switchMap } from 'rxjs/operators';
import { HttpErrorResponse } from '@angular/common/http';
import { SharedAnnotationEditorService } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.service';
import { AnnotationControllerService, AnnotationPojo, DataDictionaryControllerService, DataDictionaryPojo, DataPointControllerService,
  ModuleControllerService, ModulePojo, ProjectRole, SavedSearchControllerService, FunctionalBlockControllerService } from '@innowake/mining-api-angular-client';
import { BulkAction } from '@app/shared/components/mining-table/mining-table-config.interface';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { Entity } from '@app/core/services/clipboard.service';
const log = new Logger('AnnotationEditModal');

@Component({
  selector: 'data-dictionary-reporting',
  templateUrl: '../../shared/components/base-customizable-table/base-reporting-page.component.html'
})
export class DataDictionaryReportingComponent extends BaseReportingPageComponent implements OnInit, OnDestroy {

  pageTitle: string = this.translateService.instant('codeViewer.dataDictionary');

  selectedFilter = this.translateService.instant('codeViewer.dataDictionary');

  usage: Usages =  Usages.DATADICTIONARYTABLE;

  pageType = TypeName.PAGEDATADICTIONARY;

  graphQlType = 'dataDictionaries';

  dataDictionary = 'Data Dictionary';

  errorMessage = this.translateService.instant('errorFetchingDetails', { tableType: this.dataDictionary });

  internalDataPoints = [
    { name: 'id', path: 'content.id' },
    { name: 'uid', path: 'content.uid' },
    { name: 'path', path: 'content.module.path' },
    { name: 'id', path: 'content.module.id' }
  ];

  constructor(
    public messageService: NzMessageService,
    public translateService: TranslateService,
    public relationshipService: ClientProjectRelationshipService,
    public userCustomizableTable: CustomizableTableColumnService,
    public graphQlControllerService: GraphQlControllerService,
    public route: ActivatedRoute,
    @Inject(WindowToken) private $window: Window,
    public dataPointControllerService: DataPointControllerService,
    public savedSearchControllerService: SavedSearchControllerService,
    public parametersService: CustomizableTableParametersService,
    public router: Router,
    public modalService: NzModalService,
    public cd: ChangeDetectorRef,
    public notification: NzNotificationService,
    private dataDictionaryControllerService: DataDictionaryControllerService,
    private authorizationService: KeycloakAuthorizationService,
    private nzDrawerService: NzDrawerService,
    private annotationControllerService: AnnotationControllerService,
    private moduleControllerService: ModuleControllerService,
    private shareAnnotationEditor: SharedAnnotationEditorService,
    private jobManagerService: JobManagerService,
    private functionalBlockControllerService: FunctionalBlockControllerService
  ) {
    super(messageService, relationshipService, userCustomizableTable, graphQlControllerService, route,
      dataPointControllerService, savedSearchControllerService, translateService, parametersService, router, modalService,
      notification, cd, shareAnnotationEditor);
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
        this.rowActions = [[{ label: 'btnLabel.edit', value: 'edit' },
        {
          type: LinkType.DROPDOWN,
          icon: 'more',
          options: [
            { label: this.translateService.instant('iconToolTip.codeViewer'), value: 'codeviewer', disableItem: () => false },
            { label: this.translateService.instant('iconToolTip.openDataLineage'), value: 'dataLineage', disableItem: () => false}
          ]
        }
        ]];
        const bulkActions: BulkAction[] = [
          {
            label: this.translateService.instant('btnLabel.delete'),
            isDanger: true,
            action: (ddIds: number[]) => this.userCustomizableTable.bulkDelete(ddIds, this.dataDictionary, this)
          },
          {
            label: this.translateService.instant('btnLabel.generateFB'),
            isDanger: false,
            action: (data: Entity[]) => this.userCustomizableTable.bulkFbGeneration(data, this.dataDictionary, this)
          },
        ];
        this.tableConfig.bulkActions = bulkActions;
        this.allowSelectDeselectRows = true;
      } else {
        this.rowActions = [];
      }
    });
  }

  /**
   * Open the Data Dictionary editor in a drawer.
   * @param rowData data of the row.
   */
  handleSelectedOption(rowData: MiningTableOptionSelected): void {
    const moduleId: number = rowData?.data?.module?.id;
    const modulePath: string = rowData?.data?.module?.path;
    switch (rowData.optionValue) {
      case 'codeviewer':
        this.dataDictionaryControllerService.findDataDictionaryEntryByRecordId(this.projectId, moduleId, rowData.data.uid as string)
          .subscribe((dataDictionaryItem: DataDictionaryPojo) => {
            if (dataDictionaryItem) {
              const path = 'code-viewer?offset=' + dataDictionaryItem.location.offset;
              openInNewTab(this.projectId, moduleId, path, this.$window);
            }
          });
        break;
      case 'edit': {
        let dataDictionaryItem: DataDictionaryPojo;
        this.dataDictionaryControllerService.findDataDictionaryEntryByRecordId(this.projectId, moduleId, rowData.data.uid as string).pipe(
          switchMap((dataDictionaryResp: DataDictionaryPojo) => {
            dataDictionaryItem = dataDictionaryResp;
            if (dataDictionaryResp?.uid) {
              return this.dataDictionaryControllerService.findLinkedBusinessVariables(this.projectId, moduleId, dataDictionaryResp?.uid);
            }
          })).subscribe((codeAnnotationEditorItems: AnnotationPojo[]) => {
            if (dataDictionaryItem) {
              this.refreshCoreTable = false;
              const headerTitle: string = this.translateService.instant('sharedDataDictionaryEditor.headerTitle',
                { moduleName: rowData?.data?.module?.name }
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
                nzWidth: codeAnnotationEditorItems.length > 0 ? '85vw' : '34vw',
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
          };
        break;
      case 'linkedBusinessVariables':
        const selectedAnnotationId: number = rowData?.data?.selectedAnnotationId;
        this.openSharedAnnotationEditorModal(selectedAnnotationId, moduleId);
        break;
      case 'dataLineage':
        this.dataDictionaryControllerService.findDataDictionaryEntryByRecordId(this.projectId, moduleId, rowData.data.uid as string)
        .subscribe((dataDictionaryItem: DataDictionaryPojo) => {
          if (dataDictionaryItem) {
            const path = 'data-lineage?offset=' + dataDictionaryItem.location.offset + '&field=' + dataDictionaryItem.dataElementName;
            openInNewTab(this.projectId, moduleId, path, this.$window);
          }
        });
      break;
    }
  }

  ngOnDestroy(): void {
    this.reloadSubscription?.unsubscribe();
    this.destroy();
  }

  private openSharedAnnotationEditorModal(annotationId: number, moduleId: number): void {
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
          this.refreshCoreTable = false;
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
          }).afterClose.subscribe((editorAction: string) => {
            if (editorAction === AllowedTableActions.UPDATE || editorAction === AllowedTableActions.DELETE) {
              this.refreshCoreTable = true;
            }
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

}
