import { OnDestroy, Output, EventEmitter, Component, Input, OnInit, ViewChild, HostListener, ChangeDetectorRef, AfterContentChecked } from '@angular/core';
import { FormGroup, FormControl, UntypedFormControl, UntypedFormGroup, Validators, UntypedFormBuilder } from '@angular/forms';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ModuleBadgeUpdateService } from '@app/core/services/module-details/module-badge-update.service';
import { CustomPropertyInput } from '@app/shared/interfaces/custom-property-input.interface';
import { BadgeCountUpdateOperation } from '@app/modules/module-details/models/module-complexity-details';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { NzMessageService } from 'ng-zorro-antd/message';
import { Observable, Subscription } from 'rxjs';
import { AnnotationEditor, EDITOR_WIDTH } from '../../interfaces/annotation-editor.interface';
import { sortNaturally } from '@app/core/utils/sort.util';
import { FormResponse, FormResult } from '../../interfaces/annotation-editor.interface';
import { CustomPropertyFieldListComponent } from '../custom-property-editor/custom-property-field-list/custom-property-field-list.component';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { TranslateService } from '@ngx-translate/core';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { switchMap } from 'rxjs/operators';
import { SharedDataDictionaryEditorComponent } from '../shared-data-dictionary-editor/shared-data-dictionary-editor.component';
import { getHeaderTemplate } from '../shared-editor-header/shared-editor-header.component';
import { AllowedTableActions, MiningTableAction } from '../mining-table/mining-table-action.interface';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { ClipboardService, Entity } from '@app/core/services/clipboard.service';
import { SharedAnnotationEditorService } from './shared-annotation-editor.service';
import { AnnotationCategory, AnnotationCategoryControllerService, AnnotationControllerService, AnnotationPojo, AnnotationToFunctionalBlockControllerService,
  CustomPropertyMetadata, DataDictionaryControllerService, DataDictionaryPojo, EntityId, ModuleControllerService, ModulePojo, ProjectPojoCustomProperties,
  ProjectPojoPrototype, GenAiAnnotationRequest } from '@innowake/mining-api-angular-client';

const TYPE = 'type';
const ANNOTATION = 'annotation';
const STATUS = 'status';
const CATEGORY = 'category';
const englishTranslation = 'englishTranslation';
const customPropertiesCategory = 'customPropertiesCategory';

@Component({
  selector: 'app-shared-annotation-editor',
  templateUrl: './shared-annotation-editor.component.html'
})
export class SharedAnnotationEditorComponent implements OnInit, OnDestroy, AfterContentChecked {

  @ViewChild(CustomPropertyFieldListComponent) customPropertyList: CustomPropertyFieldListComponent;

  @Input() annotation: AnnotationPojo;

  @Input() moduleId: EntityId;

  @Input() modulePath?: string;

  @Input() isCreateMode: boolean;

  @Input() parentComponent: AnnotationEditor;

  @Input() canCopyPasteAnnotation?: boolean;

  @Output() formResult = new EventEmitter<FormResponse<AnnotationPojo>>();

  annotationForm: UntypedFormGroup;
  genAiOptionsForm: FormGroup;

  statusList: string[];

  typeList: string[];

  categoryList: AnnotationCategory[] = [];

  categoryListAll: AnnotationCategory[] = [];

  supportedCustomProperties: ProjectPojoCustomProperties[];

  englishTranslation: string;

  functionalGroupNames: string[] = [];

  showAllContent = false;

  // Let the enum available in the template
  dataType: any = CustomPropertyMetadata.DataTypeEnum;

  propertiesMetamodels: CustomPropertyInput[] = [];
  annotationId: number;
  deleteOption: boolean;
  isSave = false;
  isCancel = false;
  isDelete = false;
  isClose = false;
  isGenerating = false;
  hasTypeSpecificCategory = false;
  gptTranslateActive = false;
  clientId: number;
  projectList: ProjectPojoPrototype[];
  projectId: number;
  metaDataList: string[] = [];
  customPropertiesCategoryList: string[] = [];
  selectedCustomPropertyCategory: string;
  nonBusinessVariablesReferenced: DataDictionaryPojo[] = [];
  businessVariablesReferenced: DataDictionaryPojo[] = [];
  selectedCustomPropertiesLabel: string;
  isDataPresentInClipboard = false;
  copyBtnlabel: string;
  pasteBtnlabel: string;
  pastedCustomProperties: { [key: string]: ProjectPojoCustomProperties[]; };
  isGenerationSuccessful = false;
  isGenAIOptionsModalVisible: boolean;
  private currentClient: ClientProjectRelationship;
  private clientProjectSubscription: Subscription;

  constructor(
    private fb: UntypedFormBuilder,
    private annotationController: AnnotationControllerService,
    private annotationToFunctionalBlockController: AnnotationToFunctionalBlockControllerService,
    private annotationCategoryController: AnnotationCategoryControllerService,
    private clientProjectRelationShip: ClientProjectRelationshipService,
    private messageService: NzMessageService,
    private moduleBadgeUpdateService: ModuleBadgeUpdateService,
    private notification: NzNotificationService,
    private translateService: TranslateService,
    private featureToggleService: FeatureToggleService,
    private dataDictionaryControllerService: DataDictionaryControllerService,
    private moduleControllerService: ModuleControllerService,
    private parameterService: CustomizableTableParametersService,
    private nzDrawerService: NzDrawerService,
    private labelMappingService: LabelMappingService,
    public clipboardService: ClipboardService,
    private sharedAnnotationEditorService: SharedAnnotationEditorService,
    public cd: ChangeDetectorRef
  ) {
    this.clientProjectSubscription = this.clientProjectRelationShip.getClientProjectObservable().subscribe(currentClient => {
      this.currentClient = currentClient;
      this.clientId = currentClient.getClientId();
      this.projectId = currentClient.getProjectId();
    });

    // commenting this code until theres is some decision takes on browser back button navigation
    // window.onpopstate = (event: PopStateEvent) => {
    //   if (this.sharedAnnotationEditorService.getEditorState() &&
    //     ! this.isFormDirty() &&
    //     ! confirm('You have unsaved changes. Do you really want to leave this page?')) {
    //     event.preventDefault();
    //     history.pushState(null, null, window.location.href);
    //   }
    // };
    this.genAiOptionsForm = new FormGroup({
      'context': new FormControl('')
    });
  }

   /**
    * Get the name for of the Annotation Category currently selected
    */
   get currentCategoryName(): string {
    if (this.annotationForm && this.categoryListAll.length > 0 && this.annotationForm.get(CATEGORY).value) {
      return this.categoryListAll.find(cat => cat.id === this.annotationForm.get(CATEGORY).value).name;
    }
    return null;
  }

   /**
    * A canDeactivate guard in Angular that listens for two events: 'window:beforeunload' and 'window:popstate'.
    * It checks certain conditions to determine whether navigation should be prevented or allowed.
    * @param $event - The event object that triggers the canDeactivate guard.
    * @returns An Observable of a boolean or a boolean, indicating whether navigation is allowed (true) or prevented (false).
    */
   @HostListener('window:beforeunload', ['$event'])
   canDeactivate(): Observable<boolean> | boolean {
     return ! (this.sharedAnnotationEditorService.getEditorState() && ! this.isFormDirty());
   }

  ngOnInit(): void {
    if (this.annotation && this.annotation.dataDictionaryEntries?.length > 0) {
      this.fetchLinkedBusinessVariablesOfAnnotations();
    }
    this.featureToggleService.isActive('generativeAiTranslations').subscribe((isActive: boolean) => {
      this.gptTranslateActive = isActive;
    });
    const annotationFields: { [k: string]: UntypedFormControl } = {};
    annotationFields[TYPE] = this.fb.control(this.annotation.type, Validators.required);
    annotationFields[TYPE].valueChanges.subscribe((categoryName: AnnotationPojo.TypeEnum) => {
      this.annotationForm.get(CATEGORY).setValue(null);
      this.filterCategories(categoryName);
    });
    annotationFields[ANNOTATION] = this.fb.control(this.annotation.name ? this.annotation.name : '');
    annotationFields[STATUS] = this.fb.control(this.annotation.state ? this.annotation.state : AnnotationPojo.StateEnum.IN_ANALYSIS, Validators.required);
    annotationFields[CATEGORY] = this.fb.control(this.annotation.categoryId);
    this.englishTranslation = this.annotation.englishTranslation;
    if (this.annotation.id) {
      this.annotationToFunctionalBlockController.getFunctionalBlockNamesByAnnotationId(this.currentClient.getProjectId(), this.annotation.id, 'body')
        .subscribe((data: Set<string>) => {
            this.functionalGroupNames = Array.from(data);
          }
        );
    }
    this.annotationId = this.annotation.id;
    annotationFields[englishTranslation] = this.fb.control(this.englishTranslation);
    annotationFields[customPropertiesCategory] = this.fb.control(this.selectedCustomPropertyCategory);
    this.annotationForm = new UntypedFormGroup(annotationFields);

    if (this.isCreateMode) {
      this.deleteOption = true;
    }
    if (this.annotation?.reasons && this.annotation.reasons.length) {
      this.annotation.reasons.forEach((metaData: string) => {
          this.metaDataList.push(this.labelMappingService.mapLabel(LabelType.ANNOTATION_METADATA_REASON, metaData));
      });
    }
    this.annotationCategoryController.findAllAnnotationCategories(this.currentClient.getProjectId()).subscribe((categories: AnnotationCategory[]) => {
      this.categoryListAll = sortNaturally(categories, 'name');
      if (this.annotation.categoryId && this.categoryListAll.findIndex(category => category.id === this.annotation.categoryId) === -1) {
        this.categoryListAll.push({ id: this.annotation.categoryId, name: this.annotation.categoryName });
      }
      this.filterCategories(this.annotation.type);
    });
    this.statusList = Object.keys(AnnotationPojo.StateEnum).map(state => AnnotationPojo.StateEnum[state]);
    this.typeList = this.isCreateMode || this.annotation.type !== 'FUNCTIONAL' ? Object.keys(AnnotationPojo.TypeEnum).filter(type => type !== 'FUNCTIONAL')
      .map(type => AnnotationPojo.TypeEnum[type]) : Object.keys(AnnotationPojo.TypeEnum).map(type => AnnotationPojo.TypeEnum[type]);
    this.copyBtnlabel = this.translateService.instant('btnLabel.copyDetails');
    this.pasteBtnlabel = this.translateService.instant('btnLabel.pasteDetails');
    this.annotationForm.valueChanges.subscribe(() => {
      if ( ! this.isFormDirty()) {
       this.sharedAnnotationEditorService.setEditorFormState(true);
      }
    });
  }

  ngAfterContentChecked(): void {
    this.cd.detectChanges();
  }

  /**
   * Filter selectable Categories based on selected Type.
   * @argument annotationType: Annotation Type based on which to filter avalilable Categories.
   */
  filterCategories(annotationType: AnnotationPojo.TypeEnum): void {
    this.hasTypeSpecificCategory = false;
    this.categoryList = [];
    if (this.categoryListAll && this.categoryListAll.length > 0) {
    for (const category of this.categoryListAll) {
      const isTypeCategory = category.types?.length && category.types.includes(annotationType);
      if (! category.types?.length || isTypeCategory) {
        this.categoryList.push(category);
        if (isTypeCategory) {
          this.hasTypeSpecificCategory = true;
        }
      }
    }
  }
  }

  /**
   * Resets the form on cancel
   */
  resetForm(): void {
    const resetedValues = {
      type: this.annotation.type,
      annotation: this.annotation.name,
      status: this.annotation.state,
      category: this.annotation.categoryId,
      englishTranslation: this.annotation.englishTranslation,
      annotationId: this.annotation.id
    };
    this.customPropertyList?.resetCustomPropertiesValue(resetedValues);
    this.annotationForm.reset(resetedValues);
  }

  /**
   * Callback of the cancel button
   */
  onCancel(): void {
    this.isCancel = true;
    this.resetForm();
    this.formResult.emit({ result: FormResult.Canceled });
  }

  /**
   * Callback of the delete button
   */
  onDelete(): void {
    this.isDelete = true;
    this.formResult.emit({ result: FormResult.Disabled });
    this.annotationController.deleteAnnotation(this.currentClient.getProjectId(), this.annotation.id).subscribe(() => {
      this.formResult.emit({ result: FormResult.Deleted });
      this.moduleBadgeUpdateService.updateAnnotationDataDictionary({operation: BadgeCountUpdateOperation.ANNOTATION_DELETED, count: 1});
      this.messageService.success(this.translateService.instant('sharedAnnotationEditorComponent.deleteSuccessMessage') as string);
      this.isDelete = false;
    }, () => {
      this.formResult.emit({ result: FormResult.Error });
      this.notification.error(
        this.translateService.instant('sharedAnnotationEditorComponent.deleteErrorMessage') as string,
        this.translateService.instant('contactSupport') as string,
        { nzDuration: 0 }
      );
    });
  }

  /**
   * Callback of the submit button, post the annotation to to api-server
   */
  onSubmit(): void {
    this.isSave = true;
    this.formResult.emit({ result: FormResult.Disabled });
    this.annotation.type = this.annotationForm.get(TYPE).value;
    this.annotation.name = this.annotationForm.get(ANNOTATION).value;
    this.annotation.state = this.annotationForm.get(STATUS).value;
    this.annotation.categoryId = this.annotationForm.get(CATEGORY).value;
    this.annotation.categoryName = this.currentCategoryName;
    this.annotation.englishTranslation = this.annotationForm.get(englishTranslation).value;
    if (this.parentComponent === AnnotationEditor.ECLIPSE_EDITOR) {
      this.deleteOption = false;
    }
    if (this.annotation.customProperties === undefined) {
      this.annotation.customProperties = {};
    }
    this.annotation.customProperties = this.customPropertyList?.getSubmitValue();
    let submitObservable: Observable<AnnotationPojo>;
    if (this.isCreateMode) {
      submitObservable = this.annotationController.createAnnotation(this.currentClient.getProjectId(), this.moduleId, this.annotation);
    } else {
      submitObservable = this.annotationController.updateAnnotation(this.currentClient.getProjectId(), this.annotation.id, this.annotation);
    }
    submitObservable.subscribe((resp: AnnotationPojo) => {
      this.isSave = false;
      this.resetForm();
      this.messageService.success(this.translateService.instant('sharedAnnotationEditorComponent.saveSuccessMessage') as string);
      if (this.isCreateMode && this.parentComponent === AnnotationEditor.CODEVIEWER_ANNOTATION_EDITOR) {
        this.formResult.emit({ result: FormResult.Saved, returnedObject: resp });
      } else {
        this.formResult.emit({ result: FormResult.Saved });
      }
    }, () => {
      this.formResult.emit({ result: FormResult.Error });
      this.isSave = false;
      this.notification.error(
        this.translateService.instant('sharedAnnotationEditorComponent.saveErrorMessage') as string,
        this.translateService.instant('contactSupport') as string,
        { nzDuration: 0 }
      );
    });
  }

  /**
   * Callback of translate button, updates annotation with translation generated.
   */
  translateUsingGpt(): void {
    this.isGenerating = true;

    const genAiAnnotationRequest: GenAiAnnotationRequest = {
      annotation: this.annotation,
      options: {
        context: this.genAiOptionsForm.get('context').value,
      }
    };

    this.annotationController.getGptTranslation(this.currentClient.getProjectId(), this.moduleId, genAiAnnotationRequest).subscribe((resp: string[]) => {
      this.isGenerating = false;
      this.isGenerationSuccessful = true;
      this.annotationForm.get(ANNOTATION).setValue(resp);
      this.annotationForm.markAsDirty();
    }, (error) => {
      this.isGenerating = false;
      this.isGenerationSuccessful = false;
      this.notification.error(
        this.translateService.instant('sharedAnnotationEditorComponent.translationErrorMessage') as string,
        error.error.message as string,
        { nzDuration: 0 }
      );
    });
  }

  openGenAIOptionsModal(): void {
    this.isGenAIOptionsModalVisible = true;
  }

  handleGenAIOptionsCancel(): void {
    this.isGenAIOptionsModalVisible = false;
  }

  handleGenAIOptionsOk(): void {
    this.isGenAIOptionsModalVisible = false;
  }

  /**
   * Changes the content view in the view mode on click of Expand/Collapse.
   */
  toggleContentView(): void {
    this.showAllContent = ! this.showAllContent;
  }

  /**
   * Returns if the form can be submitted. This is the case if its status is VALID and if the user made changes.
   * Otherwise `false` is returned.
   * @returns `true` if the form is valid or in case of new form creation. Otherwise `false` for the edit.
   */
  canSubmitForm(): boolean {
    return this.annotationForm.valid && (this.annotationForm.dirty || this.isCreateMode);
  }

  /**
   * Returns if metadata should be displayed.
   * @returns `true` if type is 'Rule' and category is 'Business Rule'.
   */
  showMetaData(): boolean {
    return this.annotationForm.get(TYPE).value === AnnotationPojo.TypeEnum.RULE && this.currentCategoryName === 'Business Rule' && this.metaDataList.length > 0;
  }

  /**
   * opens SharedDataDictionaryEditor from selection of business variables referenced
   */
  openSharedDataDictionaryEditor(businessVariable: DataDictionaryPojo): void {
    let modulePath: string;
    this.moduleControllerService.findModuleById(this.projectId, this.moduleId, true).subscribe((module: ModulePojo) => {
      modulePath = this.translateService.instant('annotationReporting.sharedAnnotationEditorPath', { modulePath: module?.path });
    });
    const moduleId = this.moduleId;
    let dataDictionaryItem: DataDictionaryPojo;
    this.dataDictionaryControllerService.findDataDictionaryEntryByRecordId(this.projectId, this.moduleId, businessVariable?.uid).pipe(
      switchMap((dataDictionaryRes: DataDictionaryPojo) => {
        dataDictionaryItem = dataDictionaryRes;
        if (dataDictionaryRes.annotations.length > 0) {
          return this.dataDictionaryControllerService.findLinkedBusinessVariables(this.projectId, moduleId, businessVariable?.uid);
        }
      })).subscribe((codeAnnotationEditorItems: AnnotationPojo[]) => {
        if (dataDictionaryItem) {
          const headerTitle: string = this.translateService.instant('sharedDataDictionaryEditor.headerTitle',
            { moduleName: this.annotation.moduleName }
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
              this.parameterService.setReloadTableDataValue(true);
            }
          });
        }
      });
  }

  /**
   * handles the custom categories dropdown
   * @param customCategories accepts array of customCategories names
   */
  handleCustomPropertyCategories(customCategories: string[]): void {
    this.customPropertiesCategoryList = customCategories;
    if (customCategories.length === 1) {
      this.selectedCustomPropertyCategory = customCategories[0];
    }
  }

  /**
   * Method to copy the annotation details to clipboard.
   */
  copyAnnotationDetails(): void {
    this.copyBtnlabel = this.translateService.instant('btnLabel.copied');
    this.clipboardService.copyToClipboard(Entity.ANNOTATION, this.annotationId);
  }

  /**
   * Method to fill the annotation details from clipboard.
   */
  fillAnnotationDetails(): void {
    const copiedAnnotationId = this.clipboardService.getFromClipboard(Entity.ANNOTATION) as unknown as number;
    this.annotationController.findAnnotationById(this.projectId, copiedAnnotationId).subscribe(
      (annotationDetails: AnnotationPojo) => {
        if (annotationDetails) {
          const patchedValues = {
            type: annotationDetails['type'],
            annotation: annotationDetails['name'],
            status: annotationDetails['state'],
            category: annotationDetails['categoryId'],
          };
          this.isDataPresentInClipboard = true;
          this.pastedCustomProperties = annotationDetails['customProperties'];
          this.customPropertyList.parentForm = this.annotationForm;
          this.annotationForm.patchValue(patchedValues);
          this.annotationForm.markAsDirty();
          this.messageService.success(this.translateService.instant('annotationFields.detailsAdded') as string);
        }
      }, (error) => {
          // Check if the error response body contains a specific error status
          if (error.status === 404) {
            this.notification.error(
              this.translateService.instant('sharedAnnotationEditorComponent.pasteAnnotationErrorTitle') as string,
              this.translateService.instant('sharedAnnotationEditorComponent.pasteAnnotationErrorContent') as string,
              { nzDuration: 1000 }
            );
          }
      });
  }

  /**
   * Method to check the state of paste button.
   */
  checkStateofPasteBtn(): boolean {
    const annotationPasteId = this.clipboardService.getFromClipboard(Entity.ANNOTATION);
    return this.annotationId === +annotationPasteId || (! annotationPasteId && this.canCopyPasteAnnotation);
  }

  /**
   * Check if the annotation form is dirty or not.
   */
  isFormDirty(): boolean {
    return ! this.annotationForm.dirty;
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.clientProjectSubscription.unsubscribe();
    this.sharedAnnotationEditorService?.setEditorState(false);
    this.sharedAnnotationEditorService?.setEditorFormState(false);
  }

  /**
   * fetch linked Business Variables of selected annotation record
   */
  private fetchLinkedBusinessVariablesOfAnnotations(): void {
    this.annotationController.findLinkedBusinessVariablesById(this.projectId, this.annotation?.id).
    subscribe((linkedDataDictionaries: DataDictionaryPojo[]) => {
      this.businessVariablesReferenced = linkedDataDictionaries.filter((dataDictionary) => dataDictionary.isBusiness);
      this.nonBusinessVariablesReferenced = linkedDataDictionaries.filter((dataDictionary) => ! dataDictionary.isBusiness);
    });
  }


}
