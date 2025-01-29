import { Component, Input, OnInit } from '@angular/core';
import { UntypedFormArray, UntypedFormControl, UntypedFormGroup } from '@angular/forms';
import { CustomPropertyInput } from '@app/shared/interfaces/custom-property-input.interface';
import { CustomPropertyMetadata } from '@innowake/mining-api-angular-client';

@Component({
 selector: 'custom-property-field',
 templateUrl: './custom-property-field.component.html'
})
export class CustomPropertyFieldComponent implements OnInit {

    @Input() customProperty: CustomPropertyInput;

    @Input() optionList: string[];

    @Input() form: UntypedFormGroup;

    /* Let the enum available in the template */
    public dataType: any = CustomPropertyMetadata.DataTypeEnum;
    finalType: CustomPropertyMetadata.DataTypeEnum | CustomPropertyMetadata.FieldTypeEnum;

    ngOnInit(): void {
        if (this.customProperty.fieldType && this.customProperty.fieldType !== CustomPropertyMetadata.FieldTypeEnum.DEFAULT) {
            this.finalType = this.customProperty.fieldType;
        } else {
            this.finalType = this.customProperty.dataType;
        }
    }

    customPropertyFormArray(customProperty: CustomPropertyInput): UntypedFormArray {
        return this.form.get(customProperty.inputName) as UntypedFormArray;
    }

    addField(customProperty: CustomPropertyInput): void {
        const formArray = this.customPropertyFormArray(customProperty);
        formArray.push(new UntypedFormControl(''));
    }

    removeField(customProperty: CustomPropertyInput, index: number): void {
      const formArray = this.customPropertyFormArray(customProperty);
      formArray.removeAt(index);
      this.form.markAsDirty();
    }

    onModelChange(items: string[]): void {
        let changed = false;
        const newItems: string[] = [];
        items?.forEach(value => {
            const tmp = value.trim();
            changed = changed || tmp.length !== value.length;

            /* ignore empty values */
            if (tmp.length !== 0) {
                /* remove duplicates like nzSelect does */
                const index = newItems.indexOf(tmp);
                if (index === -1) {
                    newItems.push(tmp);
                } else {
                    newItems.splice(index, 1);
                }
            }
        });

        if (changed) {
            this.form.get(this.customProperty.inputName).setValue(newItems);
        }
    }
}
