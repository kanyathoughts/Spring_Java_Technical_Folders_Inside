import { Component, OnDestroy, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { Subscription } from 'rxjs';
import { MiningTableConfig, DEFAULT_NUMBER_OF_ROWS, FieldTypeEnum } from '@app/shared/components/mining-table/mining-table-config.interface';
import { ModuleControllerService, AggregationRequestModuleFieldName, AggregationResultModuleFieldName } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { SummaryTableData, SummaryTableObj } from './summary.interface';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { forkJoin } from 'rxjs';
import { Logger } from '@app/core';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { HttpErrorResponse } from '@angular/common/http';
import { LocalShowcaseMode } from '@app/core/utils/local-showcase.util';

const log = new Logger('Summary');
const infoIcon = 'info-circle';
const iconStyleClass = 'ant-helper-secondary-text';

@Component({
  selector: 'app-summary',
  templateUrl: './summary.component.html'
})

export class SummaryComponent implements OnInit, OnDestroy {
  clientProjectSubscription: Subscription;
  summaryTableValue: SummaryTableData[];
  localShowcaseMode = new LocalShowcaseMode();
  tableConfig: MiningTableConfig = {
    columnMap: {
      technology: { field: 'technology', header: 'technology', fieldType: FieldTypeEnum.STRING,
        getLabel: (value: string) => this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, value)
      },
      modules: { field: 'modules', header: 'projectDashboard.modules', fieldType: FieldTypeEnum.NUMBER },
      physicalFiles: {
        field: 'physicalFiles', header: 'metrics.summary.summaryTable.physicalFiles', fieldType: FieldTypeEnum.NUMBER,
        options: [
          {
            icon: infoIcon,
            title: 'metrics.summary.summaryTable.physicalFilesDefinition',
            styleClass: iconStyleClass,
          }
        ]
      },
      physicalLinesOfCode: {
        field: 'physicalLinesOfCode', header: 'physicalLinesOfCode', fieldType: FieldTypeEnum.NUMBER,
        options: [
          {
            icon: infoIcon,
            title: 'metrics.summary.summaryTable.plocDefinition',
            styleClass: iconStyleClass,
          }
        ]
      },
      linesOfCode: {
        field: 'linesOfCode', header: 'sourceLinesOfCode', fieldType: FieldTypeEnum.NUMBER, sortOrder: 'descend',
        options: [
          {
            icon: infoIcon,
            title: 'metrics.summary.summaryTable.slocDefinition',
            styleClass: iconStyleClass,
          }
        ]
      },
      lineOfComment: {
        field: 'lineOfComment', header: 'commentLinesOfCode', fieldType: FieldTypeEnum.NUMBER,
        options: [
          {
            icon: infoIcon,
            title: 'metrics.summary.summaryTable.clocDefinition',
            styleClass: iconStyleClass,
          }
        ]
      },
      lineOfDeadCode: {
        field: 'lineOfDeadCode', header: 'metrics.summary.summaryTable.deadCode', fieldType: FieldTypeEnum.NUMBER,
        options: [
          {
            icon: infoIcon,
            title: 'metrics.summary.summaryTable.deadCodeDefinition',
            styleClass: iconStyleClass,
          }
        ]
      },
      missingModules: {
        field: 'missingModules', header: 'metrics.summary.summaryTable.missingModules', fieldType: FieldTypeEnum.NUMBER,
        options: [
          {
            icon: infoIcon,
            title: 'metrics.summary.summaryTable.missingModulesDefinition',
            styleClass: iconStyleClass,
          }
        ]
      },
      errors: {
        field: 'errors', header: 'metrics.summary.summaryTable.errors', fieldType: FieldTypeEnum.NUMBER,
        options: [
          {
            icon: infoIcon,
            title: 'metrics.summary.summaryTable.errorsDefinition',
            styleClass: iconStyleClass,
          }
        ]
      },
    },
    paginator: false,
    rows: DEFAULT_NUMBER_OF_ROWS,
    showTotalCount: false,
    tableBorder: true,
    isExportVisible: false
  };
  constructor(
    private moduleControllerService: ModuleControllerService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private numberFormatter: NumberFormatter,
    private translateService: TranslateService,
    private labelMappingService: LabelMappingService
  ) {
    if (this.localShowcaseMode.isInLocalShowcaseMode) {
      delete this.tableConfig.columnMap.errors;
    }
  }

  ngOnInit(): void {
    const requestModules: AggregationRequestModuleFieldName =
    {
      groupBy: new Set<AggregationRequestModuleFieldName.GroupByEnum>([AggregationRequestModuleFieldName.GroupByEnum.TECHNOLOGY]),
      fields: {
        'ID': 'COUNT'
      }
    };

    const filterObject = {
      'REPRESENTATION': {
        'eq': 'PHYSICAL' as any
      }
    };

    const greaterThanZeroFilter = {
      'gte': '0' as any
    };

    const requestLineOfCodes: AggregationRequestModuleFieldName = {
      ...requestModules,
      filterObject: {...filterObject, 'LINES_OF_CODE': greaterThanZeroFilter },
      fields: {
        'LINES_OF_CODE': 'SUM',
      }
    };
    const requestLineOfDeadCode: AggregationRequestModuleFieldName = {
      ...requestModules,
      filterObject: {...filterObject, 'LINES_OF_DEAD_CODE': greaterThanZeroFilter },
      fields: {
        'LINES_OF_DEAD_CODE': 'SUM',
      }
    };
    const requestLineOfComments: AggregationRequestModuleFieldName = {
      ...requestModules,
      filterObject: {...filterObject, 'LINES_OF_COMMENT': greaterThanZeroFilter },
      fields: {
        'LINES_OF_COMMENT': 'SUM'
      }
    };
    const requestPhysicalFiles: AggregationRequestModuleFieldName = {
      ...requestModules,
      filterObject: {'REPRESENTATION': { 'eq': 'PHYSICAL' as any } }
    };
    const requestPhysicalLineOfCode: AggregationRequestModuleFieldName = {
      ...requestModules,
      filterObject: {...filterObject, 'PHYSICAL_LINES_OF_CODE': greaterThanZeroFilter },
      fields: { 'PHYSICAL_LINES_OF_CODE': 'SUM' }
    };
    const requestMissingModules: AggregationRequestModuleFieldName = {
      ...requestModules,
      filterObject: { 'IDENTIFICATION': { 'eq': 'MISSING' as any }}
    };
    const requestErrors: AggregationRequestModuleFieldName = {
      ...requestModules,
      filterObject: {...filterObject, 'ERRORS': greaterThanZeroFilter },
      fields: { 'ERRORS': 'SUM'}
    };

    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.tableConfig.loading=true;
      const technologyObj: SummaryTableObj = {};
      const summaryTableData: SummaryTableData[] = [];
      const modulesSum = this.moduleControllerService.getAggregatedValues2(response.getProjectId(), requestModules);
      const linesOfCode = this.moduleControllerService.getAggregatedValues2(response.getProjectId(), requestLineOfCodes);
      const linesOfDeadCode = this.moduleControllerService.getAggregatedValues2(response.getProjectId(), requestLineOfDeadCode);
      const linesOfComments = this.moduleControllerService.getAggregatedValues2(response.getProjectId(), requestLineOfComments);
      const physicalFiles = this.moduleControllerService.getAggregatedValues2(response.getProjectId(), requestPhysicalFiles);
      const physicalLinesOfCode = this.moduleControllerService.getAggregatedValues2(response.getProjectId(), requestPhysicalLineOfCode);
      const missingModules = this.moduleControllerService.getAggregatedValues2(response.getProjectId(), requestMissingModules);
      const errors = this.moduleControllerService.getAggregatedValues2(response.getProjectId(), requestErrors);

      forkJoin([modulesSum, linesOfCode, linesOfDeadCode, linesOfComments, physicalFiles, physicalLinesOfCode, missingModules, errors])
        .subscribe((response: AggregationResultModuleFieldName[][]) => {
          this.tableConfig.loading=false;
          if (response.length) {
            response[0].forEach((item) => {
              const techno: string = item.group.TECHNOLOGY as any;
              if ( ! technologyObj[techno]) {
                const unKnownService = this.translateService.instant('messageService.unknown');
                technologyObj[techno] = { modules: this.numberFormatter.transform(item?.fields?.ID as unknown as string),
                  physicalFiles: unKnownService,
                  linesOfCode: unKnownService,
                  lineOfDeadCode: unKnownService,
                  lineOfComment: unKnownService,
                  physicalLinesOfCode: unKnownService,
                  missingModules: unKnownService,
                  errors: unKnownService
                };
              }
            });
            this.technoObjectCreator(technologyObj, 'linesOfCode', 'LINES_OF_CODE', response[1]);
            this.technoObjectCreator(technologyObj, 'lineOfDeadCode', 'LINES_OF_DEAD_CODE', response[2]);
            this.technoObjectCreator(technologyObj, 'lineOfComment', 'LINES_OF_COMMENT', response[3]);
            this.technoObjectCreator(technologyObj, 'physicalFiles', 'ID', response[4]);
            this.technoObjectCreator(technologyObj, 'physicalLinesOfCode', 'PHYSICAL_LINES_OF_CODE', response[5]);
            this.technoObjectCreator(technologyObj, 'missingModules', 'ID', response[6]);
            this.technoObjectCreator(technologyObj, 'errors', 'ERRORS', response[7]);

            Object.keys(technologyObj).forEach((technology: string, index: number) => {
              summaryTableData.push({ id: index, technology, ...technologyObj[technology] } as SummaryTableData);
            });
            this.summaryTableValue = [...summaryTableData];
          }
        }, (err: HttpErrorResponse) => {
          log.error('Error while fetching Summary data: ' + err.message);
          this.tableConfig.loading=false;
        });
    });
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
  }

  /** method to decide particular table cell value
   * @param  item complete table row data
   * @param  property individual table cell field type
   * @returns can be number or string(N/A)
   */
  private technoObjectValueDecider(item: {[key: string]: any}, property: string): string | number {
    const unKnown = this.translateService.instant('messageService.unknown');
    return item[property] >= 0 ? this.numberFormatter.transform(item[property] as string) : unKnown;
  }

  /**
   * method to create table object property depending upon the table property and object property
   * @param  technologyObj technoObject representing the table row
   * @param  tableProperty table object property
   * @param  technoObjectProperty representing techno Object property
   * @param  AggregationResultstring[] individual response from backend
   */
  private technoObjectCreator(
    technologyObj: SummaryTableObj,
    tableProperty: string,
    technoObjectProperty: string,
    response: AggregationResultModuleFieldName[]
  ): void {
    if (response.length) {
      response.forEach((item) => {
        const techno: string = item.group.TECHNOLOGY as any;
        technologyObj[techno][tableProperty] = this.technoObjectValueDecider(item?.fields, technoObjectProperty);
      });
    }
  }
}
