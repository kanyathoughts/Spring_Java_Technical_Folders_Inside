import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { Logger } from '@app/core';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { LinkType } from '@app/shared/components/mining-table/mining-table-action.interface';
import { FieldTypeEnum, FilterType, MiningTableConfig, ViewMode } from '@app/shared/components/mining-table/mining-table-config.interface';
import { LoaderState } from '@app/shared/loader/loader.directive';
import {
  AggregationRequestRelationshipFieldName,
  AggregationResultRelationshipFieldName,
  ModulePojo,
  ReferenceControllerService
} from '@innowake/mining-api-angular-client';
const log = new Logger('IMSCardComponent');

@Component({
  selector: 'mn-ims-card',
  templateUrl: './ims-card.component.html'
})

export class IMSCardComponent implements OnInit {
  @Input() projectId: number;
  @Output() hideImsCard: EventEmitter<boolean> = new EventEmitter();
  baseFilter = {
    DST_TECHNOLOGY: { 'eq': 'IMS' as any },
    DST_TYPE: { 'eq': 'DBD' as any},
    SRC_TECHNOLOGY: { 'notEq': 'IMS' as any },
  };
  tableConfig: MiningTableConfig = {
    columnMap: {
      databaseName: {
        field: 'databaseName',
        header: 'Database Name',
        filterProperties: {
          filterType: FilterType.freeText,
        },
        columnAction: { type: LinkType.HYPERLINK, resolveURL: (data: ModulePojo) => this.navigateToDetails(data) },
        fieldType: FieldTypeEnum.STRING,
        displayAs: ViewMode.LINK
      },
      readWriteCount: {
        field: 'readWriteCount',
        header: 'Read Write Count',
        filterProperties: {
          filterType: FilterType.numberValue
        },
        fieldType: FieldTypeEnum.NUMBER
      }
    },
    rows: 10
  };
  value: Array<{ linkHash: string, databaseName: string, readWriteCount: number }>;
  loadState: LoaderState;

  constructor(private referenceControllerService: ReferenceControllerService) { }

  ngOnInit(): void {
    this.tableConfig.loading = true;
    this.loadState = LoaderState.loading;
    this.tableConfig.projectId = this.projectId;
    const request: AggregationRequestRelationshipFieldName = {
      filterObject: this.baseFilter,
      groupBy: new Set([
        AggregationRequestRelationshipFieldName.GroupByEnum.DST_NAME,
        AggregationRequestRelationshipFieldName.GroupByEnum.DST_ID,
        AggregationRequestRelationshipFieldName.GroupByEnum.DST_LINKHASH
      ]),
      orderBy: [],
      fields: {
        SRC_ID: 'COUNT_DISTINCT'
      }
    };
    this.referenceControllerService.getAggregatedValues1(this.projectId, request, 'REFERENCES').subscribe((response) => {
      this.value = this.getTableData(response);
      if (this.value.length) {
        this.tableConfig.loading = false;
        this.loadState = LoaderState.success;
      }
      this.hideImsCard.emit( !! this.value.length);
    }, (error) => {
      log.error(error);
      this.loadState = LoaderState.error;
    });
  }

  /**
   * Navigates to the module detail page.
   * @param module The module data to navigate to detail page.
   * @returns link for detail page.
   */
  navigateToDetails(module: ModulePojo): string {
    return RouteBuilder.buildModuleRoute(this.projectId, module.linkHash, 'details/overview');
  }

  private getTableData(response: AggregationResultRelationshipFieldName[]): Array<{
    id: number; linkHash: string; databaseName: string; readWriteCount: number; }> {
    const data: Array<{ id: number; linkHash: string; databaseName: any; readWriteCount: any; }> = response.map((value, index) => ({
      'id': index,
      'linkHash': value.group[AggregationRequestRelationshipFieldName.GroupByEnum.DST_LINKHASH] + '',
      'databaseName': value.group[AggregationRequestRelationshipFieldName.GroupByEnum.DST_NAME],
      'readWriteCount': value.fields[AggregationRequestRelationshipFieldName.GroupByEnum.SRC_ID]
    }));
    data.sort((a, b) => b.readWriteCount - a.readWriteCount);
    return data;
  }
}
