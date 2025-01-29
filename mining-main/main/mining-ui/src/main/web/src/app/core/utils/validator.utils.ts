import { UntypedFormControl, ValidationErrors } from '@angular/forms';

/**
 * Validator that checks if a string only contains whitespace
 * @param control FormControl
 * @returns ValidationErrors | null
 * @example
 * ```ts
 * const formGroup = this.fb.group({
 *   name: ['', [Validators.required, noWhitespaceValidator]],
 *   description: ['']
 * });
 * ```
 */
export const noWhitespaceValidator = (control: UntypedFormControl): ValidationErrors | null => {
    // check if string only contains whitespace
    const isWhitespace = (control && control.value && control.value.toString() || '').trim().length === 0;
    // return null if valid, otherwise return error object
    const isValid = !isWhitespace;
    return isValid ? null : { 'whitespace': true };
};


/**
 * Validator that checks if a string contains at least 5 characters.
 * This is used to check the semantic search query since the endpoint only accepts queries with mininum 5 characters.
 * @param control the FormControl
 * @return a ValidationError if the string has less than 5 characters, null otherwise
 */
export const minimumCharactersValidator = (control: UntypedFormControl): ValidationErrors | null => {
    const enoughCharacters = (control && control.value && control.value.toString() || '').trim().length >= 5;
    return enoughCharacters ? null : { 'notEnoughCharacters': true };
};
