/**
 * Interface for providing optionList to customproperties while adding annotation tags.
 */
import { TemplateRef } from '@angular/core';
import { CustomPropertyMetadataWithClass } from '@app/core/services/custom-properties/custom-properties.service';
import { CustomPropertyMetadata } from '@innowake/mining-api-angular-client';

export interface CustomPropertyInput extends CustomPropertyMetadataWithClass {
    optionList?: string[];
    inputName?: string;
  }

/**
 * Interface for custom properties table data.
 */
export interface PropertiesMetaModel {
  id: number,
  position: number,
  title: string,
  name: string,
  type: string,
  tags?: string,
  defaultValueKey?: string,
  customCategory: string,
  defaultValues: string | string[],
  disabledActions?: string[],
  buttonToolTip?: TemplateRef<void>,
  mandatory?: boolean
}

/**
 * Interface for custom properties card data.
 */
export interface CustomPropertyDetails extends CustomPropertyMetadata {
  value?: string | string[];
}

