import { Injectable } from '@angular/core';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { LinkType } from '@app/shared/components/mining-table/mining-table-action.interface';
import { DEFAULT_NUMBER_OF_ROWS,
  FieldTypeEnum,
  MiningTableConfig,
  ViewMode } from '@app/shared/components/mining-table/mining-table-config.interface';
import { MetricsTableType } from '../../modules/metrics/shared/components/metrics-card/metrics-card.interface';

export const ColumnField = {
  NAME: 'name',
  PATH: 'path',
  TECHNOLOGY: 'technology',
  TYPE: 'type',
  IN_CODEBASE: 'inCodebase',
  COMPLEXITY: 'complexityLevel',
  LOC: 'sourceMetrics.codeLines',
  COMMENT_LOC: 'sourceMetrics.commentLines',
  METRICS_DATE: 'metricsDate',
  UTILITY_NAME: 'utilityName',
  NUMBER_OF_INVOCATIONS: 'numberOfInvocations',
  CATEGORY: 'category',
  PROTOCOL: 'Protocol',
  INBOUND: 'Inbound',
  OUTBOUND: 'Outbound',
  STATEMENT: 'statementTypeLink',
  LENGTH: 'textLength',
  DISTINCTTABLES: 'distinctTables',
  CUSTOMCOMPLEXITY: 'customComplexity',
  HALSTEADCOMPLEXITY: 'halsteadComplexity',
  HALSTEADDIFFICULTY: 'halsteadDifficulty',
  TEXT: 'text',
  TABLES: 'tables',
  MODULE_NAME: 'moduleName',
  Function: 'method',
  DATABASE_NAME: 'databaseName',
  SEGMENT_NAME: 'segmentName',
  IMS_STATEMENT: 'statement',
  CLUSTER: 'cluster'
};

/**
 * Configuration for the Chart details.
 */
@Injectable({
  providedIn: 'root'
})
export class ChartDetailConfigService {

  constructor(
    private labelMappingService: LabelMappingService
  ) {}
  /**
   * get table config for the chart details as per the table type.
   * @param tableType the type of the table
   * @param projectId id of the current project
   * @returns table config as per the table type
   */
  getChartDataConfigByTableType(tableType: MetricsTableType, projectId: number): MiningTableConfig {
    const defaultConfig = {
      moduleName: {
        field: ColumnField.NAME,
        header: 'moduleName',
        columnAction: { type: LinkType.HYPERLINK, resolveURL: (data: any) => this.navigateToDetails(data.id as number, projectId) },
        path: 'content.name',
        displayAs: ViewMode.LINK
      },
      path: {
        field: ColumnField.PATH,
        header: 'path',
        path: 'content.path'
      },
      technology: {
        field: ColumnField.TECHNOLOGY,
        header: 'technology',
        path: 'content.technology',
        getLabel: (value: string) => this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, value)
      },
      type: {
        field: ColumnField.TYPE,
        header: 'type',
        path: 'content.type',
        getLabel: (value: string) => this.labelMappingService.mapLabel(LabelType.TYPE, value)
      },
      inCodebase: {
        field: ColumnField.IN_CODEBASE,
        header: 'module.characteristics.inCodebase',
        path: 'content.inCodebase'
      },
      complexity: {
        field: ColumnField.COMPLEXITY,
        header: 'module.characteristics.complexity',
        path: 'content.complexityLevel'
      },
      loc: {
        field: ColumnField.LOC,
        header: 'sourceLinesOfCode',
        fieldType: FieldTypeEnum.NUMBER,
        path: 'content.sourceMetrics.codeLines'
      },
      commentLoc: {
        field: ColumnField.COMMENT_LOC,
        header: 'commentLinesOfCode',
        fieldType: FieldTypeEnum.NUMBER,
        path: 'content.sourceMetrics.commentLines'
      },
      lastScan: {
        field: ColumnField.METRICS_DATE,
        header: 'lastScan.header',
        path: 'content.metricsDate',
        options: [
          {
            value: ColumnField.METRICS_DATE,
            icon: 'info-circle',
            title: 'lastScan.title',
            styleClass: 'ant-helper-secondary-text',
            disableItem: () => false
          }
        ]
      }
    };

    const utilityConfig = {
      utilityName: {
        field: ColumnField.UTILITY_NAME,
        header: 'utilityName'
      },
      numberOfInvocations: {
        field: ColumnField.NUMBER_OF_INVOCATIONS,
        header: 'numberOfInvocations',
        fieldType: FieldTypeEnum.NUMBER
      }
    };

    const tableConfig: MiningTableConfig = {
      columnMap: {},
      paginator: true,
      rows: DEFAULT_NUMBER_OF_ROWS,
      serverSidePagination: true,
      showTotalCount: true,
      loading: false
    };

    const interfaceConfig = {
      Protocol: {
        field: ColumnField.PROTOCOL,
        header: 'protocol',
      },
      Inbound: {
        field: ColumnField.INBOUND,
        header: 'inbound',
        fieldType: FieldTypeEnum.NUMBER
      },
      utilityName: {
        field: ColumnField.OUTBOUND,
        header: 'outbound',
        fieldType: FieldTypeEnum.NUMBER
      },
    };

    const SQLConfig = {
      moduleName: {
        field: ColumnField.NAME,
        header: 'moduleName',
        columnAction: { type: LinkType.HYPERLINK, resolveURL: (data: any) => this.navigateToDetails(data.moduleId as number, projectId) },
        path: 'content.module.name',
        displayAs: ViewMode.LINK
      },
      Statement: {
        field: ColumnField.STATEMENT,
        header: 'statementType',
        path: 'content.type'
      },
      Length: {
        field: ColumnField.LENGTH,
        header: 'length',
        path: 'content.textLength',
        fieldType: FieldTypeEnum.NUMBER
      },
      Tables: {
        field: ColumnField.TABLES,
        header: 'codeViewer.tables',
        path: 'content.tables',
        fieldType: FieldTypeEnum.NUMBER
      },
      distinctTables: {
        field: ColumnField.DISTINCTTABLES,
        header: 'distinctTables',
        path: 'content.distinctTables',
        fieldType: FieldTypeEnum.NUMBER
      },
      customComplexity: {
        field: ColumnField.CUSTOMCOMPLEXITY,
        header: 'customComplexity',
        path: 'content.customComplexity',
        fieldType: FieldTypeEnum.NUMBER
      },
      halsteadComplexity: {
        field: ColumnField.HALSTEADCOMPLEXITY,
        header: 'halsteadComplexity',
        path: 'content.halsteadComplexity',
        fieldType: FieldTypeEnum.NUMBER
      },
      halsteadDifficulty: {
        field: ColumnField.HALSTEADDIFFICULTY,
        header: 'halsteadDifficulty',
        path: 'content.halsteadDifficulty',
        fieldType: FieldTypeEnum.NUMBER
      },
      Text: {
        field: ColumnField.TEXT,
        header: 'text',
        path: 'content.text'
      }
    };

    const IMSConfig = {
      ModuleName: {
        field: ColumnField.MODULE_NAME,
        header: 'moduleName',
        columnAction: { type: LinkType.HYPERLINK, resolveURL: (data: any) => this.navigateToDetails(data.moduleId as number, projectId) },
        displayAs: ViewMode.LINK,
        path: 'content.name'
      },
      Function: {
        field: ColumnField.Function,
        header: 'function',
        path: 'content.dependencies.properties'
      },
      DatabaseName: {
        field: ColumnField.DATABASE_NAME,
        header: 'dataBaseName',
        path: 'content.dependencies.module.name'
      },
      SegmentName: {
        field: ColumnField.SEGMENT_NAME,
        header: 'segmentName',
      },
      Statement: {
        field: ColumnField.IMS_STATEMENT,
        header: 'statement',
      },
    };

    switch (tableType) {
      case MetricsTableType.ModuleDetailsTable:
        tableConfig.columnMap = defaultConfig;
        break;
      case MetricsTableType.UtilitiesTable:
        tableConfig.columnMap = utilityConfig;
        break;
      case MetricsTableType.InterfacesTable:
        tableConfig.columnMap = interfaceConfig;
        break;
      case MetricsTableType.SqlDetailsTable:
        tableConfig.columnMap = SQLConfig;
        break;
      case MetricsTableType.IMSTable:
        tableConfig.columnMap = IMSConfig;
        break;
      default:
        tableConfig.columnMap = defaultConfig;
    }
    return tableConfig;
  }

  navigateToDetails(moduleId: number, projectId: number): string {
    return RouteBuilder.buildModuleRoute(projectId, moduleId, 'details/overview');
  }
}
