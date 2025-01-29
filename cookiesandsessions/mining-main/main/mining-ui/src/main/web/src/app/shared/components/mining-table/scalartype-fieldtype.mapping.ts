import { MiningDataPointDefinitionWithPath } from '@innowake/mining-api-angular-client';
import { FieldTypeEnum } from './mining-table-config.interface';

const ScalarTypeEnum = MiningDataPointDefinitionWithPath.ScalarTypeEnum;

const fieldType: { [key in keyof typeof ScalarTypeEnum]: any } = {
  'Int': FieldTypeEnum.NUMBER,
  'Long': FieldTypeEnum.NUMBER,
  'Float': FieldTypeEnum.NUMBER,
  'String': FieldTypeEnum.STRING,
  'Boolean': FieldTypeEnum.STRING,
  'DateTime': FieldTypeEnum.STRING,
  'Timestamp': FieldTypeEnum.STRING,
  'UUID': undefined,
  'JSON': undefined,
  'EntityId': undefined,
  'empty': undefined
};

export const getFieldType = (scalarType: string): any => {
  if (scalarType) {
    const type = scalarType[0] + scalarType.substring(1).toLowerCase();
    return  fieldType[type] ?? scalarType;
  }
};
