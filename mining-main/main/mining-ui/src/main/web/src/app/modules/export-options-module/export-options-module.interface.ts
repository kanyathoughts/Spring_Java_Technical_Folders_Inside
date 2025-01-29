import { CustomPropertyInput } from '@app/shared/interfaces/custom-property-input.interface';
import { ExportFormatDescription, ParameterDescription, UploadDescription } from '@innowake/mining-api-angular-client';

/* Interface for the Export & Grouped Extension details.
*/
export interface groupedDetail {
    id: string;
    extensionType: ExportFormatDescription;
    label: string;
    parameterDescriptions: ParameterDescription[];
    uploadDescription: UploadDescription
}

export interface exportDetails {
    key: string;
    value: groupedDetail[];
}

/**
 * Interface for data points used in graph ML
 */
export interface DataPoint {
    label: string,
    id: string,
    checked: boolean
  }

/**
 * Interface for the export form data parameters.
 */
export interface ExportParameter extends CustomPropertyInput {
  id?: string,
  jobLabel?: string,
  extensionType?: ExportFormatDescription,
  descriptionType?: string
}
