import { HttpErrorResponse } from '@angular/common/http';
import { Component, Input, Output, OnChanges, OnInit, EventEmitter, SimpleChanges } from '@angular/core';
import { AbstractControl, UntypedFormArray, UntypedFormBuilder, UntypedFormControl, UntypedFormGroup, Validators } from '@angular/forms';
import { Logger } from '@app/core';
import { CustomPropertiesService, CustomPropertyMetadataWithClass } from '@app/core/services/custom-properties/custom-properties.service';
import { CustomPropertyInput } from '@app/shared/interfaces/custom-property-input.interface';
import { CustomPropertyMetadata, ProjectPojoCustomProperties } from '@innowake/mining-api-angular-client';

const logger = new Logger('SharedAnnotationEditorComponent');
const TAG = 'TAG';

@Component({
  selector: 'app-custom-property-field-list',
  templateUrl: './custom-property-field-list.component.html'
})
export class CustomPropertyFieldListComponent implements OnInit, OnChanges {

  @Input() className: string;

  @Input() projectId: number;

  @Input() customProperties: { [key: string]: object };

  @Input() parentForm: UntypedFormGroup;

  @Input() currentShowWhenValue: string;

  @Input() selectedCustomPropertyCategory: string;

  @Input() isDataPresentInClipboard: boolean;

  @Input() pastedCustomProperties: { [key: string]: object };

  @Output() customPropertyCategories = new EventEmitter<string[]>();

  propertiesMetamodels: CustomPropertyInput[] = [];

  customAnnotationFields: { [k: string]: any } = {};

  autoCompletions: { [k: string]: string[] } = {};

  CustomPropertyMetadataWithClassList: CustomPropertyMetadataWithClass[] = [];


  constructor(
    private fb: UntypedFormBuilder,
    private customPropertiesService: CustomPropertiesService
  ) { }

  ngOnInit(): void {
    this.getCustomPropertiesMetadataForClass();
  }

  ngOnChanges(changes: SimpleChanges): void {
    this.isDataPresentInClipboard = changes.isDataPresentInClipboard?.currentValue ?
      changes.isDataPresentInClipboard.currentValue: this.isDataPresentInClipboard;
    this.propertiesMetamodels = [];
    if (changes?.selectedCustomPropertyCategory?.currentValue) {
      const selectedCustomCategoryList: CustomPropertyMetadataWithClass[] =
        this.CustomPropertyMetadataWithClassList.filter((cp: CustomPropertyMetadataWithClass) => cp.customCategory === this.selectedCustomPropertyCategory);
      this.preparePropertiesMetaModals(selectedCustomCategoryList.length > 0 ? selectedCustomCategoryList : this.CustomPropertyMetadataWithClassList);
    } else if (this.isDataPresentInClipboard) {
      this.customProperties = this.pastedCustomProperties;
      this.getCustomPropertiesMetadataForClass();
      const patchDetails = {};
      this.customAnnotationFields.customProperties.forEach((customProperty: CustomPropertyInput) => {
        const [className, customPropertyName] = customProperty.name.split('.');
        const customPropertyData = this.customProperties[className][customPropertyName];
        if (customPropertyData) {
          patchDetails[customProperty.inputName] = customPropertyData;
        }
      });
      this.setValuesToParentForm(patchDetails);
    }
    this.updateAutoCompletionValues();
  }

  /**
   * prepares custom Properties MetaModals based on selected category
   */
  preparePropertiesMetaModals(customPropertiesRes: CustomPropertyMetadataWithClass[]): void {
    this.propertiesMetamodels = [];
    this.propertiesMetamodels = customPropertiesRes.map((metaModel: CustomPropertyMetadataWithClass) => ({
      ...metaModel,
      inputName: `${metaModel.customPropertyClassName}_${metaModel.name}`,
      name: `${metaModel.customPropertyClassName}.${metaModel.name}`
    }));
    this.setOptionList();
  }

  /**
   * Provide the customProperties object to attach to the class property before submiting
   * @returns customProperty values
   */
  getSubmitValue(): { [key: string]: object } {
    this.propertiesMetamodels.forEach(customProperty => {
      const [className, customPropertyName] = customProperty.name.split('.');
      if ( ! this.customProperties[className]) {
        this.customProperties[className] = {};
      }
      this.customProperties[className][customPropertyName] = this.getValueForCustomProperty(customProperty);
    });
    this.CustomPropertyMetadataWithClassList.forEach(customProp => {
      if (this.customProperties[customProp.customPropertyClassName]) {
        const existingCustomProperty = this.customProperties[customProp.customPropertyClassName][customProp.name];
        if ( ! existingCustomProperty) {
          const emptyValue: any = customProp.dataType === CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST ? [] : '';
          this.customProperties[customProp.customPropertyClassName][customProp.name] = emptyValue;
        }
      }
    });
    return this.customProperties;
  }

  /**
   * Reset the values of the form
   * @param resetedValues Initial values to be resetted
   */
  resetCustomPropertiesValue(resetedValues: object): void {
    this.propertiesMetamodels
      .filter(customPropertyInput => customPropertyInput?.dataType === CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST)
      .forEach(customPropertyInput => {
        const customProperty = this.findCustomProperty(customPropertyInput);
        if (customPropertyInput?.fieldType === TAG) {
          resetedValues[customPropertyInput.inputName] = customProperty || [];
        } else {
          const formArray = this.parentForm.get(customPropertyInput.inputName) as UntypedFormArray;
          const stringArray: string[] = (customProperty as string[]) || [''];
          resetedValues[customPropertyInput.inputName] = stringArray;

          while (formArray.length !== stringArray.length) {
            if (formArray.length > stringArray.length) {
              formArray.removeAt(stringArray.length - 1);
            } else {
              formArray.push(new UntypedFormControl(''));
            }
          }
        }
      });
  }

  /**
   * If the 'customProperty' is a Tag, then return the list of tags for selection.
   * Otherwise return an empty array.
   * @param customProperty The custom property
   * @returns The default tags.
   */
  getAutoCompletionList(customProperty: CustomPropertyMetadata): string[] {
    if (customProperty.fieldType === CustomPropertyMetadata.FieldTypeEnum.TAG) {
      return this.autoCompletions[customProperty.autoCompletionKey];
    }
    return [];
  }

  /**
   * Updates the value of optionList after editing.
   */
  private setOptionList(): void {
    this.customAnnotationFields.customProperties = [];
    this.propertiesMetamodels = this.propertiesMetamodels.filter(property =>
      property.dataType === CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST || property.dataType === CustomPropertyMetadata.DataTypeEnum.STRING);
    const categories = this.propertiesMetamodels.map(property => property.customCategory);
    const removedCategory: CustomPropertyMetadataWithClass[] = this.CustomPropertyMetadataWithClassList.filter(customProperty =>
      ! categories.includes(customProperty.customCategory));
    if (removedCategory.length) {
      removedCategory.forEach(category => {
        this.parentForm?.removeControl(category.customPropertyClassName + '_' + category['name']);
      });
    }
    this.propertiesMetamodels.forEach(prop => {
      this.parentForm?.registerControl(prop.inputName, this.getFormControl(prop));
      // Custom annotation not grouped by category
      if ( ! prop.showWhen.annotationCategoryName) {
        if ( ! this.customAnnotationFields.customProperties) {
          this.customAnnotationFields.customProperties = [];
        }
        if (this.customAnnotationFields.customProperties.indexOf(prop) === -1) {
          this.customAnnotationFields.customProperties.push(prop);
        }
      } else {
        // Custom annotations grouped by category
        if ( ! this.customAnnotationFields[prop.showWhen.annotationCategoryName as any]) {
          this.customAnnotationFields[prop.showWhen.annotationCategoryName as any] = { other: [] };
        }
        if (prop.customViewNames.length === 0) {
          this.customAnnotationFields[prop.showWhen.annotationCategoryName as any].other.push(prop.name);
        } else {
          for (const customViewName of prop.customViewNames) {
            if ( ! this.customAnnotationFields[prop.showWhen.annotationCategoryName as any][customViewName]) {
              this.customAnnotationFields[prop.showWhen.annotationCategoryName as any][customViewName] = [];
            }
            this.customAnnotationFields[prop.showWhen.annotationCategoryName as any][customViewName].push(prop);
          }
        }
      }
      if (this.customAnnotationFields.customProperties) {
        (this.customAnnotationFields.customProperties as any[])
          .filter(customProperty => customProperty.fieldType === TAG)
          .forEach(customProperty => customProperty.optionList = this.getAutoCompletionList(customProperty as CustomPropertyMetadata));
      }
    });
  }

  private getFormControl(customPropertyMetadata: CustomPropertyMetadata): UntypedFormControl | UntypedFormArray {
    const customProperty = this.findCustomProperty(customPropertyMetadata);
    const validators = customPropertyMetadata.mandatory? [Validators.required, Validators.pattern(/^(\s+\S+\s*)*(?!\s).*$/)] : [];
    if (customPropertyMetadata?.dataType === CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST) {
      if (customPropertyMetadata?.fieldType === TAG) {
        return this.fb.control(customProperty || [], validators);
      }
      if (customProperty) {
        const field = this.fb.array([]);
        (customProperty as string[]).forEach(val => field.push(this.fb.control(val, validators)));
        return field;
      }
      return this.fb.array([this.fb.control('', validators)]);
    } else {
      return this.fb.control(customProperty || '', validators);
    }
  }

  private findCustomProperty(metadata: CustomPropertyMetadata): ProjectPojoCustomProperties {
    if ( ! this.customProperties) {
      this.customProperties = {};
    }
    let className = '';
    let customPropertyName = '';
    if (metadata?.name) {
      [className, customPropertyName] = metadata.name.split('.');
    }

    return this.customProperties?.[className]?.[customPropertyName];
  }

  /**
   * Updates the `autoCompletionValues` after editing.
   */
  private updateAutoCompletionValues(): void {
    this.customPropertiesService.getAutoCompletionValues(this.projectId).subscribe(
      (response: { [key: string]: any }) => {
        if (response) {
          this.autoCompletions = { ...this.autoCompletions, ...response };
          Object.entries(this.autoCompletions).forEach(entry => this.autoCompletions[entry[0]] =
            /* filter duplicates and sort tag array */
            entry[1].filter((value, index) => entry[1].indexOf(value) === index)
              .sort((a, b) => a.toLowerCase().localeCompare(b.toLowerCase())));
        }
        this.setOptionList();
      },
      (error: any) => {
        logger.error('Error while loading auto-completion values from server', error);
      }
    );
  }

  private getValueForCustomProperty(customProperty: CustomPropertyInput) {
    if ( ! customProperty.showWhen.annotationCategoryName || customProperty.showWhen.annotationCategoryName as any === this.currentShowWhenValue) {
      if (customProperty.dataType === CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST) {
        let formArray: string[] = this.parentForm?.controls[customProperty.inputName]?.value;
        formArray = formArray ? formArray.filter(field => field.trim() !== '') : [];
        return formArray.length > 0 ? formArray : null;
      } else if (this.parentForm?.controls[customProperty.inputName]?.value) {
        return this.parentForm.controls[customProperty.inputName].value;
      }
    } else {
      return null;
    }
  }

  private getCustomPropertiesMetadataForClass(): void {
    this.customPropertiesService.getCustomPropertiesMetadataForClass(this.className, this.projectId).subscribe((res) => {
      if (res) {
        this.propertiesMetamodels = [];
        this.CustomPropertyMetadataWithClassList = res;
        let customCategoryList = res.map(({ customCategory }) => customCategory);
        customCategoryList = [...new Set(customCategoryList.filter(customCategory => customCategory !== null))];
        this.customPropertyCategories.emit(customCategoryList);
        this.preparePropertiesMetaModals(this.CustomPropertyMetadataWithClassList);
      }
    }, (error: HttpErrorResponse) => {
      logger.error('Error while loading custom annotation properties from server', error);
    });
  }

  private setValuesToParentForm(patchDetails: { [key: string]: any }): void {
    Object.keys(patchDetails).forEach(key => {
      const control = this.parentForm.get(key);
      const formattedKey = key.replace('_', '.');
      const isEmbeddedList = this.customAnnotationFields.customProperties.some(
        (prop: any) => prop.name === formattedKey && prop.dataType === 'EMBEDDEDLIST'
      );
      if (control instanceof UntypedFormControl) {
        if (isEmbeddedList) {
          const newValue = patchDetails[key] || []; // default to empty array if null
          const newFormArray = new UntypedFormArray(newValue.map((detail: any) => new UntypedFormControl(detail)) as AbstractControl[]);
          this.parentForm.setControl(key, newFormArray);
        } else {
          control.patchValue(patchDetails[key] || ''); // Use an empty string if null
        }
      } else if (control instanceof UntypedFormArray) {
        control.clear();
        (patchDetails[key] || [])?.forEach((detail: any) => {
          control.push(new UntypedFormControl(detail));
        });
      }
    });
  }
}
