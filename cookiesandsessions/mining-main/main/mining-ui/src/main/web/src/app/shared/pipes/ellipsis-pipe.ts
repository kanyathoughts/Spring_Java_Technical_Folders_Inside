import { Pipe, PipeTransform } from '@angular/core';


@Pipe({ name: 'ellipsisPipe' })
export class EllipsisPipe implements PipeTransform {
  transform(stringData: string, maxLength: number): string {
    if (stringData && stringData.length > maxLength) {
      return `${stringData.slice(0, maxLength)}...`;
    }
    return stringData;
  }
}
