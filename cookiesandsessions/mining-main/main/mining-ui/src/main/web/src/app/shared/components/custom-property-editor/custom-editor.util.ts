/**
 * Defines the key value pair, checkduplicates and position validation.
 */

import { AbstractControl, ValidationErrors } from '@angular/forms';
import { CustomPropertyMetadata } from '@innowake/mining-api-angular-client';

export const URL_REGEX = '^(https?|ftp)://[A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]';

const editTypeLabels: { [key: string]: string } = {
  'Input String': 'STRING',
  'String Repeater': 'EMBEDDEDLIST',
  'Select Tag': 'EMBEDDEDLIST',
  'URL': 'STRING'
};

const fieldTypeLabels: { [key: string]: string } = {
  'Input String': 'DEFAULT',
  'String Repeater': 'DEFAULT',
  'Select Tag': 'TAG',
  'URL': 'URL'
};

/**
 * returns value for corresponding key
 * @param type is the key.
 */
export const getLabelForType = (type: any | string): CustomPropertyMetadata.DataTypeEnum => editTypeLabels[type] ?? type;

/**
 * Maps the select value to a corresponding field type.
 * @param type The selected value.
 * @returns The field type.
 */
export const getFieldForType = (type: any | string): CustomPropertyMetadata.FieldTypeEnum => fieldTypeLabels[type] ?? type;


/**
 * checkDuplicateList is used to check the duplicate value.
 * @param listOfValues List of unique value to use as reference.
 * @param initialValue initial value in case we update an existing value of the list.
 * @returns key value pair with duplicate as true if conditions are met.
 */
export const checkDuplicateList = (listOfValues: string[], initialValue: string) =>
  (control: AbstractControl): ValidationErrors | null => {
    const finalListOfValues = [...new Set(listOfValues)];
    if (initialValue) {
      const initialValueIndex = listOfValues.findIndex(existingTitle => existingTitle.toLowerCase() === initialValue?.toLowerCase());
      finalListOfValues.splice(initialValueIndex, 1);
    }
    const titleIndex = finalListOfValues.findIndex(existingTitle => existingTitle.toLowerCase() === control.value.toLowerCase());
    return (titleIndex !== -1) ? { 'duplicate': true } : null;
  };

/**
 * checkPostionValidation is used to check the position validation.
 * @param checkValue the input value for input number for position.
 */
export function checkPostionValidation(control: AbstractControl): { [key: string]: boolean } | null {
  if (control.value > this.length || control.value < 0) {
    return { 'invalidPosition': true };
  } else {
    return null;
  }
}
