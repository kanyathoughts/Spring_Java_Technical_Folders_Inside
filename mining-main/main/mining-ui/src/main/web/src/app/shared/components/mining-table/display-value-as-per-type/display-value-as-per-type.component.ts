import { Component, ElementRef, EventEmitter, Input, OnChanges, OnInit, Output, SimpleChanges, ViewChild } from '@angular/core';
import { UntypedFormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { dateFormatter } from '@app/core/utils/date-formatter.utils';
import { TranslateService } from '@ngx-translate/core';
import { Column, ViewMode } from '../mining-table-config.interface';
import { ColumnEditOption } from '../mining-table-action.interface';
import { convertToTag } from '@app/core/utils/tag-conversion.utils';
import { Params } from '@angular/router';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';

@Component({
  selector: 'app-display-value-as-per-type',
  templateUrl: './display-value-as-per-type.component.html',
})
export class DisplayValueAsPerTypeComponent implements OnInit, OnChanges {

  @ViewChild('textFieldInput') textFieldInput: ElementRef;
  @Input() column: Column;
  @Input() stringList: string | string[];
  @Input() displayAs: string;
  @Input() data: Record<any, any>;
  @Input() editOptions: ColumnEditOption;
  @Input() currentEditingColumnId: string;
  @Input() isBulkInsertEnabled: boolean;
  @Input() renderAsLink = false;
  @Input() linkDisplayText = '';
  @Output() selectedBusinessVariable: EventEmitter<Record<any, any>> = new EventEmitter();
  iconDetails: any;
  showInputType = true;
  isEditable: ColumnEditOption;
  fieldForm: UntypedFormGroup;
  checkArray = true;
  viewMode = ViewMode;
  notAllowed = false;
  labels: { [key: string]: { [key: string]: string } } ;
  placeholder: string;
  listOfAnnotationCategories: any;
  formSubmit = false;
  showIcon: boolean;
  url: string;
  params: Params;
  linkNotApplicable: string;

  constructor(
    private formBuilder: UntypedFormBuilder,
    private translateService: TranslateService,
    private numberFormatter: NumberFormatter
  ) { }

  ngOnInit(): void {
    this.linkNotApplicable = this.translateService.instant('notAvailable');
    if (this.column) {
      this.displayAs = this.column?.displayAsCallback ? this.column?.displayAsCallback(this.data) : this.column?.displayAs;
      this.url = this.column?.columnAction?.resolveURL(this.data);
      this.params = this.column?.columnAction?.resolveURLParams ? this.column?.columnAction?.resolveURLParams(this.data) : null;
    }
    if (this.data?.isEditable === undefined || this.data?.isEditable) {
      this.isEditable = { enableEditing: this.editOptions?.enableEditing };
    }
    if(this.column?.displayAs === this.viewMode.ICON){
      this.checkArray = false;
      if( typeof(this.stringList) !== 'string') {
        this.iconDetails = this.stringList;
        this.showIcon = true;
      } else {
        this.showIcon = false;
      }
    }
    if (Array.isArray(this.stringList)) {
      if (this.column?.displayAs === ViewMode.TAG || this.column?.displayAs === ViewMode.LINK) {
        this.stringList = this.naturalSort(this.stringList);
      }
    } else {
      if (this.column?.displayAs === ViewMode.DATE && this.stringList !== this.translateService.instant('notAvailable')) {
        this.stringList = dateFormatter(new Date(this.stringList));
      } else if (this.column?.displayAs === ViewMode.LINK && this.data?.showCountAsText) {
        this.displayAs = ViewMode.DISABLEDLINK;
      }
      this.checkArray = false;
    }
    this.placeholder = '';
    let validations = this.editOptions?.validations;
    this.placeholder = this.data?.isNewRecord ? this.translateService.instant('configuration.placeHolder')
    : this.translateService.instant('configuration.taxonomyEditPlaceHolder');
    if (this.data?.isNewRecord) {
      if ( ! this.isBulkInsertEnabled) {
        this.placeholder = '';
      }
      if ( ! this.showInputType && ! this.data?.isEditable && this.isBulkInsertEnabled) {
        this.stringList = [];
        validations = [Validators.required];
      }
      this.changeEditableState(false);
    }
    this.fieldForm = this.formBuilder.group({
      textField: [this.stringList, validations]
    });
  }

  ngOnChanges(changes: SimpleChanges): void {
    this.checkArray = Array.isArray(this.stringList) ? true :  false;
    const currentValue = changes.currentEditingColumnId?.currentValue;
    if ( ! currentValue || currentValue === this.data.id) {
      this.notAllowed = false;
    } else {
      this.notAllowed = true;
    }
    if (this.stringList === null || this.stringList === undefined || this.stringList === '') {
      this.stringList = this.translateService.instant('notAvailable');
    }
    if (Array.isArray(this.stringList) && this.stringList.every(value => value === null)) {
      this.stringList = [];
    }
    if (typeof this.column?.getLabel === 'function') {
      if(this.checkArray) {
        this.stringList = (this.stringList as string[]).map((value: string) => this.column.getLabel(value));
      } else {
        this.stringList = this.column.getLabel(this.stringList as string);
      }
    }
  }

  /**
   * Changes the state of enableEditing and passes the edited data to the parent component.
   */
  onFormSubmit(): void {
    this.formSubmit = true;
    if (this.isBulkInsertEnabled && this.data?.isNewRecord && this.showInputType) {
      this.fieldForm.controls.textField.setValue(this.convertToTagOnEnter(this.fieldForm.controls.textField.value as string));
    }
    if (this.fieldForm?.valid) {
      this.changeEditableState(false);
      if (this.fieldForm.controls.textField.value !== this.stringList || this.data?.isNewRecord) {
        this.editOptions?.onSubmit({
          actualValue: this.stringList,
          newValue: this.fieldForm.controls.textField.value,
          isNewRecord: this.data?.isNewRecord,
          type: this.data?.type,
          parent: this.data.parent,
          expand: this.data.expand
        });
      }
    }
  }

  /**
   * convert the into the tags if it is new records
   */
  checkNewRecord(): void {
    if (this.data.isNewRecord && this.isBulkInsertEnabled) {
      this.convertToTagOnEnter(this.fieldForm.controls.textField.value as string);
    } else {
      if (this.fieldForm?.valid) {
        this.onFormSubmit();
      }
    }
  }

  /**
   * Convert the number into formatted manner
   * @param stringList input as string
   * @returns string as number in formatter manner
   */
  formatNumber(stringList: string | string[]): string | string[] {
    // Note: We are using the transformIntegerOnly as there are some custom property where string is formatting as no if it contains only integer.
    return (typeof stringList === 'string' && !isNaN(Number(stringList))) ? this.numberFormatter.transformIntegerOnly(stringList) : stringList;
  }

  /**
   * divides the string based on commas and double quotes
   * @param formValue input as string
   *
   * @return string value in some cases.
   */
  convertToTagOnEnter(formValue: string): void | string[] {
    this.showInputType = false;
    const finalBrokenTags = convertToTag(formValue);
    if (this.formSubmit) {
      return finalBrokenTags;
    } else {
      this.fieldForm.controls.textField.setValue(finalBrokenTags);
    }
  }

  /**
   * Changes the state of enableEditing when clicked.
   * @param resetForm flag to reset form
   */
  changeEditableState(resetForm = true): void {
    if (this.isEditable?.enableEditing !== undefined && (this.currentEditingColumnId === this.data.id || ! this.currentEditingColumnId)) {
      this.isEditable.enableEditing = ! this.isEditable.enableEditing;
      if (this.isEditable.enableEditing) {
        setTimeout(() => {
          this.textFieldInput?.nativeElement.focus();
          this.fieldForm.controls['textField'].markAsDirty();
          this.fieldForm.controls['textField'].updateValueAndValidity();
        }, 0);
        this.editOptions.onEditingStart(this.data);
      } else {
        this.editOptions.onCancel(this.data);
      }
      if (resetForm) {
        this.fieldForm.reset({ textField: this.stringList });
      }
    }
  }

  /**
   * emits the selected Business Variable referenced stringList
   * @param businessVariable contains selected businessVariable
   */
  showDetailsOfBusinessVariableReferenced(index: number, dataElementName: string): void {
    if (this.data?.linkedBusinessDataDictionary?.length > 0 || this.data?.linkedNonBusinessDataDictionary?.length > 0) {
      const matchedBusinessElement = this.data?.linkedBusinessDataDictionary?.find((item: any ) => item.name ===  dataElementName);
      const matchedNonBusinessElement = this.data?.linkedNonBusinessDataDictionary?.find((item: any) => item.name === dataElementName);
      if (matchedBusinessElement) {
        this.data['selectedDDId'] = matchedBusinessElement.id;
      } else if (matchedNonBusinessElement) {
        this.data['selectedDDId'] = matchedNonBusinessElement.id;
      }
      this.selectedBusinessVariable.emit({ data: this.data });
    } else {
      this.data['selectedAnnotationId'] = this.data.linkedBusinessRules[index]?.id;
      this.selectedBusinessVariable.emit({ data: this.data });
    }
  }

  getDisplayText(item: string | string[]): string | string[] {
    return (this.renderAsLink) ? this.linkDisplayText :  item;
  }

  private naturalSort(values: string[]): string[] {
    const sortAlphaNum = (a: string, b: string) => a.localeCompare(b, 'en', { numeric: true });
    return [...values].sort(sortAlphaNum);
  }

}
