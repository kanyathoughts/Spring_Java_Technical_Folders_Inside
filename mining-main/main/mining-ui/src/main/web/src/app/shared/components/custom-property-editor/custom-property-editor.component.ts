import { Component, OnInit, Output, EventEmitter, Input, OnDestroy } from '@angular/core';
import { UntypedFormBuilder, UntypedFormGroup, Validators, AbstractControl, UntypedFormArray, UntypedFormControl } from '@angular/forms';
import { PropertiesMetaModel } from '@app/shared/interfaces/custom-property-input.interface';
import { TranslateService } from '@ngx-translate/core';
import { checkDuplicateList, checkPostionValidation, getLabelForType, getFieldForType } from './custom-editor.util';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { NzMessageService } from 'ng-zorro-antd/message';
import { FormResponse, FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { forkJoin, Observable, Subscription } from 'rxjs';
import { ScrollEventService } from '@app/core/services/scroll-event/scroll-event.service';
import { AnnotationPojo, CustomPropertyMetadata, MetamodelControllerService, ProjectControllerService } from '@innowake/mining-api-angular-client';
import { AnnotationControllerService } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-custom-property-editor',
  templateUrl: './custom-property-editor.component.html',
})
export class CustomPropertyEditorComponent implements OnInit, OnDestroy {

  @Output() sendtoTable = new EventEmitter<FormResponse<AnnotationPojo>>();
  @Output() hideShowTab: EventEmitter<boolean> = new EventEmitter();
  @Input() selectedCustomProperty: PropertiesMetaModel;
  @Input() propertiesMetaModel: PropertiesMetaModel[];
  @Input() projectId: number;
  @Input() customPropertyClass: string;
  @Input() customCategoryList: string[] = ['Add new'];
  listOfTitle: string[] = [];
  length: number;
  duplicateCheck = false;
  CustomPropertyEditorForm: UntypedFormGroup;
  typeList = ['Input String', 'String Repeater', 'Select Tag', 'URL'];
  deleteOption = false;
  creationMode: boolean;
  editPageTitle: string;
  deleteConfirmText: string;
  saveConfirmText: string;
  isDelete: boolean;
  errorContent: string;
  isSave = false;
  isDefaultPropRemoved: boolean;
  customPropName: string;
  showHide: string;
  scrollEventSubscription: Subscription;
  positionTooltip: string;
  annotationsWithDefValuesResponse: { [key: string]: object } | null = null;
  private previousDefaultValues: Array<{ value: string, index: number, removedDef: boolean }> = [];
  private newDefaultValueIndexList: number[] = [];

  constructor(
    public messageService: NzMessageService,
    private fb: UntypedFormBuilder,
    private translateService: TranslateService,
    private notification: NzNotificationService,
    private projectControllerService: ProjectControllerService,
    private metamodelControllerService: MetamodelControllerService,
    private scrollEventService: ScrollEventService,
    private annotationControllerService: AnnotationControllerService
  ) { }

  get validatetitle(): AbstractControl {
    return this.CustomPropertyEditorForm.get('title');
  }

  get validateType(): AbstractControl {
    return this.CustomPropertyEditorForm.get('type');
  }

  get validatePosition(): AbstractControl {
    return this.CustomPropertyEditorForm.get('position');
  }

  get validateDefaultValues(): UntypedFormArray {
    return this.CustomPropertyEditorForm.get('defaultValues') as UntypedFormArray;
  }

  get validateUrl(): AbstractControl {
    return this.CustomPropertyEditorForm.get('url');
  }

  get getSelectedCustomCategory(): AbstractControl {
    return this.CustomPropertyEditorForm.get('customCategory');
  }

  get validateCustomCategory(): AbstractControl {
    return this.CustomPropertyEditorForm?.get('newCustomCategory');
  }

  get validateMandatory(): AbstractControl {
    return this.CustomPropertyEditorForm?.get('mandatory');
  }

  ngOnInit(): void {
    this.customCategoryList = this.customCategoryList.sort((customCategoryA, customCategoryB) => {
      if (customCategoryA === 'Add new') {
        return 1;
      }
      if (customCategoryB === 'Add new') {
        return -1;
      }
      return customCategoryA.localeCompare(customCategoryB);
    });
    this.CustomPropertyEditorForm = this.fb.group({
      title: ['', [Validators.required, checkDuplicateList(this.listOfTitle, this.selectedCustomProperty?.title)]],
      type: ['', [Validators.required]],
      position: ['', [checkPostionValidation.bind(this)]],
      defaultValues: this.fb.array([this.fb.control('')]),
      url: [''],
      customCategory: [''],
      mandatory: [false],
      newCustomCategory: [{ value: '', disabled: true }, [Validators.required, Validators.pattern(/^(\s+\S+\s*)*(?!\s).*$/),
      checkDuplicateList(this.customCategoryList, this.selectedCustomProperty?.customCategory)
      ]],
    });
    this.positionTooltip = this.translateService.instant('customPropertyTable.editorToolTip', { editorName: this.customPropertyClass });
    if (this.selectedCustomProperty?.defaultValues && typeof this.selectedCustomProperty?.defaultValues !== 'string') {
      for (const defaultValue of this.selectedCustomProperty?.defaultValues) {
        this.validateDefaultValues.push(new UntypedFormControl(defaultValue));
      }
    }
    this.propertiesMetaModel.forEach((element: PropertiesMetaModel) => {
      if (element.title) {
        this.listOfTitle.push(element.title);
      }
    });
    if (this.selectedCustomProperty) {
      this.length = this.propertiesMetaModel.length;
      this.creationMode = false;
      this.validateDefaultValues.removeAt(0);
      if (this.selectedCustomProperty.defaultValues === 'N/A') {
        this.addDefaultValues();
      }
      this.editPageTitle = this.translateService.instant('annotationFields.editAnnotationCustomProperty',
        { propertyTitle: JSON.stringify(this.selectedCustomProperty.title) });
      this.CustomPropertyEditorForm.patchValue({
        title: this.selectedCustomProperty.title,
        type: this.selectedCustomProperty.type,
        customCategory: this.selectedCustomProperty.customCategory,
        position: this.selectedCustomProperty.position,
        defaultValues: typeof this.selectedCustomProperty.defaultValues === 'string' ? [] : this.selectedCustomProperty.defaultValues,
        url: this.selectedCustomProperty.defaultValues,
        mandatory: this.selectedCustomProperty.mandatory
      });
      this.CustomPropertyEditorForm.get('customCategory').patchValue(this.selectedCustomProperty.customCategory);
      this.deleteConfirmText = this.translateService.instant('annotationFields.popConfirmDeleteCustomProperty',
        { propertyTitle: this.selectedCustomProperty.title });
      this.saveConfirmText = this.translateService.instant('annotationFields.popConfirmSaveCustomProperty',
        { propertyTitle: this.selectedCustomProperty.title });
    } else {
      this.creationMode = true;
      this.length = this.propertiesMetaModel.length + 1;
      this.CustomPropertyEditorForm.patchValue({
        position: this.length
      });
    }
    this.scrollEventSubscription =
      this.scrollEventService.getScrollObservable().subscribe((scrollPosition) => {
        this.showHide = 'custom-property-editor__' + scrollPosition;
      });
  }

  ngOnDestroy(): void {
    this.scrollEventSubscription.unsubscribe();
  }

  /**
   * customCategory On change event to enable newCustomCategory validation control
   * @param value value of selected custom category
   */
  onChangeCustomCategory(value: string): void {
    if (value === 'Add new') {
      this.CustomPropertyEditorForm.get('newCustomCategory').enable();
    } else {
      this.CustomPropertyEditorForm.get('newCustomCategory').disable();
    }
  }

  /**
   * Adds new repeater field for default values.
   */
  addDefaultValues(): void {
    this.validateDefaultValues.push(new UntypedFormControl(''));
    this.newDefaultValueIndexList.push(this.validateDefaultValues.length - 1);
  }

  /**
   * Removes the existing repeater field for default values.
   * @param index index of the repeater field to be removed.
   */
  removeDefaultValues(index: number): void {
    const valueToRemove = this.validateDefaultValues.value[index] as string;
    this.previousDefaultValues.push({
      value: valueToRemove,
      index,
      removedDef: this.checkValueInApiResponse(this.getDefValues(index), valueToRemove)
    });
    this.validateDefaultValues.removeAt(index);
    this.CustomPropertyEditorForm.markAsDirty();
  }

  isAnyRemovedValueInResponse(): boolean {
    return this.previousDefaultValues.some(item =>
      this.checkValueInApiResponse(this.annotationsWithDefValuesResponse, item.value)
    );
  }

  /**
   * close the form
   */
  closeEditDetails(): void {
    this.hideShowTab.emit(false);
    this.annotationsWithDefValuesResponse = null;
  }

  /**
   * submit the form
   * @param formData retrieve the form data on Save
   */
  submitForm(formData: UntypedFormGroup): void {
    const tagType = 'Select Tag';
    this.isSave = true;
    const customPropertyName = this.creationMode ? this.transformTextToCamelCase(formData.value.title as string) : this.selectedCustomProperty.name;
    this.customPropName = customPropertyName;
    const request: CustomPropertyMetadata = {
      label: formData.value.title,
      name: customPropertyName,
      mandatory: formData.value.mandatory,
      customViewIndex: formData.value.position,
      dataType: getLabelForType(formData.value.type),
      pluginVisible: false,
      fieldType: getFieldForType(formData.value.type),
      autoCompletionKey: formData.value.type === tagType ? customPropertyName + 'AutoCompletionKey' : null,
      customCategory: (formData.value.customCategory) ? (formData.value.newCustomCategory) ?
        formData.value.newCustomCategory : formData.value.customCategory : null
    };
    this.metamodelControllerService.defineCustomProperty(this.projectId, this.customPropertyClass, customPropertyName, request).subscribe(() => {


      if (formData.value.type === tagType) {
        const filteredDefaultValues: string[] = formData.value.defaultValues?.filter((e: any) => e);
        const subscriptionList: Array<Observable<any>> = [];
        if ( ! this.creationMode) {
          const defaultValues: string[] = this.selectedCustomProperty?.defaultValues && typeof this.selectedCustomProperty?.defaultValues !== 'string' ?
            this.selectedCustomProperty.defaultValues : [];
          this.previousDefaultValues.forEach((deletedDefaultValues) => {
            defaultValues.splice(deletedDefaultValues.index, 1);
          });
          const modifiedList: Array<{ oldValue: string, newValue: string }> = [];
          for (let i = 0; i < defaultValues.length; i++) {
            if (defaultValues[i] && defaultValues[i] !== filteredDefaultValues[i]) {
              modifiedList.push({
                oldValue: defaultValues[i],
                newValue: filteredDefaultValues[i]
              });
            }
          }
          if (this.previousDefaultValues.length > 0) {
            this.previousDefaultValues.forEach((deletedDefaultValue) => {
              subscriptionList.unshift(this.projectControllerService.deleteAutoCompletionValue(
                this.projectId, request.autoCompletionKey, deletedDefaultValue.value));
            });
          }
          if (modifiedList.length > 0) {
            modifiedList.forEach((modifiedDefaultValues) => {
              if (this.previousDefaultValues.findIndex((value) => value.value === modifiedDefaultValues.oldValue) === -1 && modifiedDefaultValues.newValue) {
                subscriptionList.unshift(this.projectControllerService.renameAutoCompletionValue(
                  this.projectId, request.autoCompletionKey, modifiedDefaultValues.oldValue, modifiedDefaultValues.newValue));
              }
            });
          }
        }
        let setList: Observable<any>;
        if (this.creationMode || this.newDefaultValueIndexList.length > 0) {
          setList = this.projectControllerService.setEnumValues(this.projectId, request.autoCompletionKey, new Set(filteredDefaultValues));
        }

        if (subscriptionList.length > 0) {
          forkJoin(subscriptionList).subscribe(() => {
            this.setAutoCompletionList(setList, formData.value.title as string);
          });
        } else {
          this.setAutoCompletionList(setList, formData.value.title as string);
        }
      } else {
        this.onSavingCustomProperty(formData.value.title as string);
      }
    }, () => {
      this.errorNotification(formData);
    });
  }

  /**
   * Deletes the customProperty.
   */
  onDelete(): void {
    this.isDelete = true;
    this.metamodelControllerService.deleteCustomProperty(this.projectId, this.customPropertyClass,
      this.selectedCustomProperty.name).subscribe(() => {
      this.isDelete = false;
      this.closeEditDetails();
      this.sendtoTable.emit({ result: FormResult.Saved });
      const successContent: string = this.translateService.instant('annotationFields.successNotification',
        { propertyTitle: this.selectedCustomProperty.title });
      this.messageService.success(successContent);
    }, () => {
      this.isDelete = false;
      this.errorNotification();
    });
  }

  /**
   * set the position value after checking for validation
   */
  setPositionValue(): void {
    if ((this.validatePosition.invalid && this.validatePosition.touched) || !this.validatePosition.value) {
      this.CustomPropertyEditorForm.patchValue({
        position: this.creationMode ? this.length : this.selectedCustomProperty.position
      });
    }
  }

  getDefValues(index: number): { [key: string]: object } {
    if ( ! this.newDefaultValueIndexList.includes(index)) {
      if ( ! this.annotationsWithDefValuesResponse) {
        this.annotationControllerService.findAnnotationsWithCustomProperty(this.projectId, this.customPropName ?
          this.customPropName : this.selectedCustomProperty.name)
          .subscribe((res: { [key: string]: object }) => {
            this.annotationsWithDefValuesResponse = res;
          });
      }
      return this.annotationsWithDefValuesResponse;
    }
  }

  private errorNotification(formData?: UntypedFormGroup): void {
    let errorTitle: string;
    this.closeEditDetails();
    if (formData) {
      errorTitle = this.translateService.instant('annotationFields.errorNotification.titleSave',
        { propertyTitle: formData.value.title });
    } else {
      errorTitle = this.translateService.instant('annotationFields.errorNotification.title',
        { propertyTitle: this.selectedCustomProperty.title });
    }
    const errorContent: string = this.translateService.instant('contactSupport');
    this.notification.create('error', errorTitle, errorContent);
  }

  private transformTextToCamelCase(str: string): string {
    return str.replace(/(?:^\w|\[A-Z\]|\b\w)/g, (word, index) =>
      index === 0 ? word.toLowerCase() : word.toUpperCase()
    ).replace(/\s+/g, '');
  }

  private onSavingCustomProperty(title: string) {
    this.closeEditDetails();
    this.sendtoTable.emit({ result: FormResult.Saved });
    this.isSave = false;
    const successContent: string = this.translateService.instant('annotationFields.saveSuccess', { propertyTitle: title });
    this.messageService.success(successContent);
  }

  private setAutoCompletionList(autoCompletionList: Observable<any>, title: string) {
    if (autoCompletionList) {
      autoCompletionList.subscribe(() => {
        this.onSavingCustomProperty(title);
      });
    } else {
      this.onSavingCustomProperty(title);
    }
  }

  private checkValueInApiResponse(res: { [key: string]: object }, valueToRemove: string) {
    if (!res || res === null || typeof res === 'undefined') {
      return false;
    }
    return Object.values(res).some((value: object) => {
      if (Array.isArray(value)) {
        return (value as string[]).includes(valueToRemove);
      }
      return false;
    });
  }
}
