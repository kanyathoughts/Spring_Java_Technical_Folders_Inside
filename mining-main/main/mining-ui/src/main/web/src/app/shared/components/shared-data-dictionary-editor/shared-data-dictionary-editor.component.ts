import { OnDestroy, Component, OnInit, ViewChild, Input, Optional, Output, EventEmitter, ElementRef, Inject } from '@angular/core';
import { UntypedFormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { Params, Router } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { Observable, Subscription } from 'rxjs';
import { Logger } from '@app/core';
import { CustomPropertyFieldListComponent } from '../custom-property-editor/custom-property-field-list/custom-property-field-list.component';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzDrawerRef, NzDrawerService } from 'ng-zorro-antd/drawer';
import { AllowedTableActions, MiningTableAction } from '../mining-table/mining-table-action.interface';
import { FormResponse, FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { AnnotationElementData } from '@app/modules/mining-code-viewer/mining-code-viewer';
import { CodeAnnotationEditorComponent } from '@app/modules/mining-code-viewer/code-annotation/code-annotation-editor.component';
import { WebAnnotationEditorComponent } from '../web-annotation-editor/web-annotation-editor.component';
import { getHeaderTemplate } from '../shared-editor-header/shared-editor-header.component';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { AnnotationControllerService, AnnotationPojo, DataDictionaryControllerService, DataDictionaryPojo, DataPointControllerService, EntityId,
  MiningDataPointDefinitionWithPath, ModuleControllerService, ModulePojo } from '@innowake/mining-api-angular-client';
import { NzModalService } from 'ng-zorro-antd/modal';
import { IdentifyBusinessRelatedModalComponent } from '../identify-business-related-modal/identify-business-related-modal.component';
import { WindowToken } from '@app/core/utils/window';
import { NzTableFilterValue } from 'ng-zorro-antd/table';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';

const log = new Logger('Shared DataDictionary EditorComponent');
const BUSINESS_RULE = 'Business Rule';
@Component({
  selector: 'app-shared-data-dictionary-editor',
  templateUrl: './shared-data-dictionary-editor.component.html'
})
export class SharedDataDictionaryEditorComponent implements OnInit, OnDestroy {

  @ViewChild(CustomPropertyFieldListComponent) customPropertyList: CustomPropertyFieldListComponent;
  @ViewChild(CodeAnnotationEditorComponent, { static: false }) codeAnnotationComponent!: CodeAnnotationEditorComponent;
  @ViewChild('openEditorElement', { static: false }) openEditorElement!: ElementRef<HTMLButtonElement>;
  @Input() moduleId: EntityId;
  @Input() modulePath = '';
  @Input() dataDictionaryItem: DataDictionaryPojo;
  @Input() isNewEntry: boolean;
  @Input() showButtons = true;
  @Input() codeAnnotationEditorItems: any[] = [];
  @Input() isCodeViewerScreen?: boolean;
  @Output() formResult = new EventEmitter<FormResponse<DataDictionaryPojo>>();
  moduleDetails = { id: 0 };
  projectId: number;
  scope: string;
  accessType: string;
  dataDictionaryForm: UntypedFormGroup;
  isSave = false;
  isCancel = false;
  isDelete = false;
  statusList: string[];
  isBusinessRelated: boolean;
  showDeleteButton = true;
  isElementary: boolean;
  isEclipseLinkAvailable = this.deepLinkService.featureIsActive();
  codeAnnotationElements: AnnotationElementData[] = [];
  businessCodeAnnotationElements: AnnotationElementData[] = [];
  nonBusinessCodeAnnotationElements: AnnotationElementData[] = [];
  isBusinessRelatedOnInit: boolean;
  defaultColumns: MiningDataPointDefinitionWithPath[] = [];
  preFilters: Array<{ key: string, value: NzTableFilterValue }> = [];
  dataPointsList: MiningDataPointDefinitionWithPath[];
  private clientProjectSubscription: Subscription;

  constructor(
    private clientProjectRelationShip: ClientProjectRelationshipService,
    private dataDictionaryController: DataDictionaryControllerService,
    private translateService: TranslateService,
    protected router: Router,
    private fb: UntypedFormBuilder,
    private messageService: NzMessageService,
    private deepLinkService: DeepLinkService,
    private notificationService: NzNotificationService,
    private moduleControllerService: ModuleControllerService,
    private nzDrawerService: NzDrawerService,
    private annotationControllerService: AnnotationControllerService,
    private parameterService: CustomizableTableParametersService,
    private modalService: NzModalService,
    private dataDictionaryControllerService: DataDictionaryControllerService,
    public userCustomizableTable: CustomizableTableColumnService,
    public dataPointControllerService: DataPointControllerService,
    @Optional() private drawerRef: NzDrawerRef<MiningTableAction>,
    @Inject(WindowToken) private $window: Window,
  ) {
    this.clientProjectSubscription = this.clientProjectRelationShip.getClientProjectObservable().subscribe(currentClient => {
      this.projectId = currentClient.getProjectId();
    });
  }

  ngOnInit(): void {
    this.codeAnnotationEditorItems.forEach((annotationEditorItem: AnnotationPojo) => {
      const data: AnnotationElementData = {
        moduleId: this.moduleId,
        projectId: this.projectId,
        typeLabel: annotationEditorItem['type'],
        stateLabel: annotationEditorItem['state'],
        modulePath: this.modulePath,
        annotation: annotationEditorItem,
        isBusinessVariable: annotationEditorItem['categoryName'] === BUSINESS_RULE ? true: false
      };
      this.codeAnnotationElements.push(data);
    });
    this.businessCodeAnnotationElements = this.codeAnnotationElements.
      filter((annotationElement: AnnotationElementData) => annotationElement.isBusinessVariable);
    this.nonBusinessCodeAnnotationElements = this.codeAnnotationElements.
      filter((annotationElement: AnnotationElementData) => !annotationElement.isBusinessVariable);
    if (this.isCodeViewerScreen) {
      this.codeAnnotationElements = [];
    }
    this.moduleDetails['id'] = this.moduleId as number;
    this.isElementary = this.dataDictionaryItem?.format.toLocaleLowerCase() !== 'group';
    this.scope = this.translateService.instant('notAvailable');
    const scopeValues = Object.keys(this.dataDictionaryItem?.scopes);
    if (scopeValues.length) {
      this.scope = scopeValues.join();
      for (const scopeValue of scopeValues) {
        if (this.dataDictionaryItem.scopes[scopeValue] && this.dataDictionaryItem.scopes[scopeValue]['accessType']) {
          const accessTypeForScope = this.dataDictionaryItem.scopes[scopeValue]['accessType'];
          if (this.accessType) {
            this.accessType += `, ${accessTypeForScope}`;
          } else {
            this.accessType = accessTypeForScope;
          }
        }
      }
    }

    if( ! this.accessType) {
      this.accessType = this.translateService.instant('notAvailable');
    }
    this.statusList = Object.keys(AnnotationPojo.StateEnum).map(state => AnnotationPojo.StateEnum[state]);
    this.isBusinessRelated = this.dataDictionaryItem.isBusiness;
    this.isBusinessRelatedOnInit = this.isBusinessRelated;
    const defaultState = this.dataDictionaryItem.isCandidate ? AnnotationPojo.StateEnum.CANDIDATE : AnnotationPojo.StateEnum.IN_ANALYSIS;
    this.dataDictionaryForm = this.fb.group({
      'ddDescription': [this.dataDictionaryItem.description, [Validators.required, Validators.pattern(/^(\s+\S+\s*)*(?!\s).*$/)]],
      'ddBusinessRelated': [this.dataDictionaryItem.isBusiness],
      'ddSourceInput': [this.dataDictionaryItem.sourceInput],
      'ddTargetOutput': [this.dataDictionaryItem.targetOutput],
      'ddFieldTransformation': [this.dataDictionaryItem.fieldTransformation],
      'state': [this.dataDictionaryItem.state ? this.dataDictionaryItem.state : defaultState, [Validators.required]],
      'ddTranslatedFieldName': [this.dataDictionaryItem.translatedFieldValue]
    });
    if (this.isNewEntry) {
      this.showDeleteButton = false;
    }
  }

  /**
   * Emits cancel string when the dd modal is closed.
   */
  onCancel(): void {
    this.isCancel = true;
    this.drawerRef?.close();
    this.formResult.emit({ result: FormResult.Canceled });
  }

  /**
   * Checks value of isReferenced to display in field.
   * @returns string value based on isReferenced value.
   */
  getReferenceValue(): string {
    if (this.dataDictionaryItem?.isReferenced === true) {
      return 'sharedDataDictionaryEditor.yes';
    } else if (this.dataDictionaryItem?.isReferenced === false) {
      return 'sharedDataDictionaryEditor.no';
    } else {
      return 'notAvailable';
    }
  }

  /**
   * Emits delete string when the dd entry is deleted.
   */
  onDelete(): void {
    this.isDelete = true;
    this.formResult.emit({ result: FormResult.Disabled });
    this.dataDictionaryController.deleteDataDictionaryEntry(this.projectId, this.moduleId, this.dataDictionaryItem.id).subscribe(() => {
      this.messageService.success(`${this.translateService.instant('dataDictionary.deleteEntrySuccess')}`);
      this.drawerRef?.close({ action: AllowedTableActions.DELETE, data: this.dataDictionaryItem });
      this.formResult.emit({ result: FormResult.Deleted, returnedObject: this.dataDictionaryItem });
    }, (error: any) => {
      this.formResult.emit({ result: FormResult.Error });
      this.notificationService.error(
        this.translateService.instant('dataDictionary.deleteEntryError') as string,
        this.translateService.instant('contactSupport') as string,
        { nzDuration: 0 }
      );
      log.error(`Error Occured: ${error.message}`);
    });
  }

  /**
   * Returns if the form can be submitted. This is the case if its status is VALID.
   * Otherwise `false` is returned.
   * @returns `true` if the form is valid, otherwise `false`.
   */
  canSubmitForm(): boolean {
    if (this.dataDictionaryForm) {
      return this.dataDictionaryForm.valid && this.dataDictionaryForm.dirty;
    }
  }

  /**
   * Saves data dictionary entry on clicking save button
   */
  onSubmit(): void {
    this.isSave = true;
    this.formResult.emit({ result: FormResult.Disabled });
    this.dataDictionaryItem.customProperties = this.customPropertyList?.getSubmitValue();
    this.dataDictionaryItem.description = this.dataDictionaryForm.get('ddDescription').value;
    this.dataDictionaryItem.state = this.dataDictionaryForm.get('state').value;
    this.dataDictionaryItem.isBusiness = this.dataDictionaryForm.get('ddBusinessRelated').value;
    this.dataDictionaryItem.sourceInput = this.dataDictionaryForm.get('ddSourceInput').value;
    this.dataDictionaryItem.fieldTransformation = this.dataDictionaryForm.get('ddFieldTransformation').value;
    this.dataDictionaryItem.targetOutput = this.dataDictionaryForm.get('ddTargetOutput').value;
    this.dataDictionaryItem.module = this.moduleId as string;
    this.dataDictionaryItem.translatedFieldValue = this.dataDictionaryForm.get('ddTranslatedFieldName').value;
    let submitObservable: Observable<DataDictionaryPojo>;
    if (this.isNewEntry) {
      this.dataDictionaryItem.isCandidate = false;
      submitObservable = this.dataDictionaryController.createDataDictionaryEntry(this.projectId, this.moduleId, this.dataDictionaryItem);
    } else {
      submitObservable = this.dataDictionaryController.updateDataDictionaryEntry(this.projectId, this.moduleId,
        this.dataDictionaryItem.id, this.dataDictionaryItem);
    }
    submitObservable.subscribe((response) => {
      this.getDataPointsForModule();
      this.isSave = false;
      this.resetForm();
      this.messageService.success(`${this.translateService.instant('dataDictionary.updateEntrySuccess')}`);
      this.drawerRef?.close({ action: AllowedTableActions.UPDATE, data: response });
      this.formResult.emit({ result: FormResult.Saved, returnedObject: response });
      if (this.codeAnnotationElements.length > 0) {
        if (this.isBusinessRelatedOnInit === true && this.dataDictionaryItem.isBusiness === false) {
          this.checkIsBusinessRelated(false);
        } else if (this.isBusinessRelatedOnInit === false && this.dataDictionaryItem.isBusiness === true) {
          this.checkIsBusinessRelated(true);
        }
      }
    }, err => {
      this.formResult.emit({ result: FormResult.Error });
      this.isSave = false;
      this.notificationService.error(
        this.translateService.instant('dataDictionary.saveEntryError') as string,
        this.translateService.instant('contactSupport') as string,
        { nzDuration: 0 }
      );
      log.error(err.message);
    });
  }

  /**
   * Call the deepLink service to open the selected module in Eclipse
   */
  openInEclipse(): void {
    const module: ModulePojo = {
      projectId: this.projectId,
      path: this.modulePath
    };
    this.deepLinkService.showModuleInEclipse(module);
  }

  /**
   * Update linked annotaions of Data Dictonary to Business related if switched from NO to YES
   */
  checkIsBusinessRelated(isBusinessRelated: boolean): void {
    this.dataDictionaryControllerService.findLinkedBusinessVariables(this.projectId, this.moduleId, this.dataDictionaryItem.uid)
      .subscribe((linkedAnnotations: AnnotationPojo[]) => {
        let ids: number[];
        const headerTitle: string = this.translateService.instant('sharedDataDictionaryEditor.headerTitle',
          { moduleName: linkedAnnotations[0].moduleName }
        );
        if (isBusinessRelated) {
          ids = linkedAnnotations.filter(ele => ele.categoryName !== BUSINESS_RULE).map(ele => ele.id);
          const modal = this.modalService.create<IdentifyBusinessRelatedModalComponent>({
            nzTitle: getHeaderTemplate(headerTitle, this.modulePath),
            nzClosable: true,
            nzMaskClosable: false,
            nzWrapClassName: 'vertical-center-modal',
            nzKeyboard: true,
            nzBodyStyle: { height: '400px'},
            nzClassName: 'shared-dd-editor__modal',
            nzWidth: '80vw',
            nzContent: IdentifyBusinessRelatedModalComponent
          });
          const instance = modal.getContentComponent();
          instance.filteredAnnotations = linkedAnnotations;
          instance.annotationIds = ids;
          instance.allowSelectDeselectRows = true;
          instance.projectId = this.projectId;
          instance.dataDictionaryName = this.dataDictionaryItem?.dataElementName;
          instance.updateType = this.translateService.instant('identifyBusinessRelated.updateTypeNoToYes');
        } else {
          ids = linkedAnnotations.filter(ele => ele.categoryName === BUSINESS_RULE).map(ele => ele.id);
          this.modalService.create({
            nzTitle: this.translateService.instant('identifyBusinessRelated.nonBusinessVariableModalTitle'),
            nzClosable: true,
            nzMaskClosable: false,
            nzKeyboard: true,
            nzWidth: '40vw',
            nzContent:this.translateService.instant('identifyBusinessRelated.nonBusinessVariableModalContent', {length: ids.length}),
            nzFooter: [
              {
                label: this.translateService.instant('btnLabel.skip'),
                type: 'default',
                onClick: () => {
                  this.modalService.closeAll();
                }
              },
              {
                label: this.translateService.instant('btnLabel.review'),
                type: 'primary',
                onClick: () => {
                  const randomValue = new Uint8Array(5);
                  crypto.getRandomValues(randomValue);
                  const uniqueIdsKeys = `linkedAnnotationIds-${Array.from(randomValue).map(byte => byte.toString(36)).join('')}`;
                  const uniqueIdsValue = { annotationIds: ids };
                  localStorage.setItem(uniqueIdsKeys, JSON.stringify(uniqueIdsValue));
                  const params = this.fetchURLParamsFromData(ids, uniqueIdsKeys);
                  const url = `/project-${this.projectId}/annotations`;
                  const route = this.router.createUrlTree([url], { queryParams: params, relativeTo: null }).toString();
                  this.$window.open('/#' + route, '_blank');
                  this.modalService.closeAll();
                }
              }
            ]
          });
        }
      });
  }

  /**
   * handles annotation code editor component actions
   * @param accepts data to pass type of action and annotations details to perform those actions
   */
  handleAnnotationSelection(data: { type: string, annotation: AnnotationElementData }): void {
    if (data?.type === 'delete') {
      this.deleteAnnotation(data?.annotation);
    } else {
      this.openAnnotationEditor(data);
    }
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.clientProjectSubscription.unsubscribe();
  }

  private fetchURLParamsFromData(ids: number[], uniqueIdsKeys: string): Params {
    if (ids.length > 0) {
      this.defaultColumns = [];
      this.getDefaultTableConfig();
      const queryParams: { [key: string]: any } = {};
      queryParams.columns = [];
      queryParams.filter = [];
      this.findPreFilters();
      if (this.defaultColumns.length) {
        this.defaultColumns.forEach((value: MiningDataPointDefinitionWithPath) => {
          queryParams.columns.push(value.id);
        });
      }
      queryParams.filter = JSON.stringify(this.preFilters);
      queryParams.preFilter = uniqueIdsKeys;
      return queryParams;
    }
  }

  private getDataPointsForModule() {
    this.dataPointControllerService.getDataPointsForType(this.projectId, TypeName.PAGEANNOTATION, [Usages.ANNOTATIONTABLE]).subscribe(
      (dataPointDefinitions: MiningDataPointDefinitionWithPath[]) => {
        this.dataPointsList = dataPointDefinitions;
      }
    );
  }

  private getDefaultTableConfig(): void {
    this.userCustomizableTable.selectedColumns = [];
    this.defaultColumns = [];
    this.dataPointsList.forEach((datPoint) => {
      if (this.userCustomizableTable.checkSelectedColumns(datPoint, Usages.ANNOTATIONTABLE)) {
        this.defaultColumns.push(datPoint);
      }
    });
  }

  private findPreFilters(): void {
    this.preFilters = [];
    this.preFilters.push({ key: 'type', value: ['RULE'] },
      { key: 'categoryName', value: [BUSINESS_RULE] });
  }

  private resetForm(): void {
    const defaultState = this.dataDictionaryItem.isCandidate ? AnnotationPojo.StateEnum.CANDIDATE : AnnotationPojo.StateEnum.IN_ANALYSIS;
    const resetValues = {
      ddDescription: this.dataDictionaryItem.description,
      ddBusinessRelated: this.dataDictionaryItem.isBusiness,
      ddSourceInput: this.dataDictionaryItem.sourceInput,
      ddTargetOutput: this.dataDictionaryItem.targetOutput,
      ddFieldTransformation: this.dataDictionaryItem.fieldTransformation,
      state: this.dataDictionaryItem.state ? this.dataDictionaryItem.state : defaultState,
    };
    this.customPropertyList?.resetCustomPropertiesValue(resetValues);
    this.dataDictionaryForm.reset(resetValues);
  }

  private deleteAnnotation(annotation: AnnotationElementData) {
    this.codeAnnotationElements = this.codeAnnotationElements.filter(annotationDetails => annotationDetails.annotation.uid
      !== annotation.annotation.uid);
  }

  private openAnnotationEditor(data: { type: string, annotation: AnnotationElementData }) {
    this.openSharedAnnotationEditorModal(data?.annotation?.annotation?.id, this.moduleId);
  }

  private openSharedAnnotationEditorModal(annotationId: number, moduleId: EntityId): void {
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
              this.parameterService.setReloadTableDataValue(true);
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
