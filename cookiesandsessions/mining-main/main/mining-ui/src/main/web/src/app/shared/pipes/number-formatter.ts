import { formatNumber } from '@angular/common';
import { Pipe, PipeTransform } from '@angular/core';
import { I18nService } from '@app/core';

/**
 * Transforms the number value to a formatted value as per the selected language.
 * Shows minimum of 1 digit before decimal and max of 0-2 digits after decimal.
 */
@Pipe({ name: 'numberFormatter' })
export class NumberFormatter implements PipeTransform {
  constructor(
    private i18nService: I18nService,
  ) { }

  /**
   * Formats the number, only upto 2 decimals will be shown in the formatted Number
   * @param value value to be formatted
   * @returns formatted string
   */
  transform(value: number | string): string {
    if(isNaN(+value)){
      return value + '';
    }
    return value ? formatNumber(value as number, this.i18nService.language, '1.0-2') : '0';
  }

  /**
   * Formats only the integer before the period, the fractional part is kept as is.
   * @param value value to be formatted
   * @returns formatted string
   */
  transformIntegerOnly(value: number | string): string {
    if (value) {
      const stringValue = typeof value === 'string' ? value : value.toString();
      const parts = stringValue.split('.');
      const formattedNumber = formatNumber(Number(parts[0]), this.i18nService.language, '1.0-2');
      return (parts[1] === undefined) ? formattedNumber : `${formattedNumber}.${parts[1]}`;
    } else {
      return '0';
    }
  }
}
