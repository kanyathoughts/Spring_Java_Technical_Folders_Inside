import { HttpClient, HttpParams, HttpResponse, HttpUrlEncodingCodec } from '@angular/common/http';
import { Component, OnInit, Input, Inject } from '@angular/core';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { getBasePath } from '@app/core/utils/base-path.utils';
import { getValueAtPath, graphQlQuery, selectDataPointFilterType } from '@app/core/utils/graphql.util';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { DataPointEditorComponent } from '@app/shared/components/datapoint-editor/datapoint-editor.component';
import {
  MiningTableConfig,
  DEFAULT_NUMBER_OF_ROWS,
  FieldTypeEnum,
  Column,
  MiningTableRow,
  FilterType
} from '@app/shared/components/mining-table/mining-table-config.interface';
import { LinkType } from '@app/shared/components/mining-table/mining-table-action.interface';
import { FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { DataPointControllerService, EntityId, MiningDataPointDefinitionWithPath, UsagesModel } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { Observable } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { WindowToken } from '@app/core/utils/window';
import { DataLineageNode } from '@app/modules/module-data-lineage/models/data-lineage.node';

const DATA_TYPE_NAME = 'FieldInfo';
const QUERY_NAME = 'fieldInfos';

@Component({
  selector: 'mn-module-schema-info',
  templateUrl: './module-schema-info.component.html'
})
export class ModuleSchemaInfoComponent implements OnInit {
  @Input() projectId: number;
  @Input() moduleId: EntityId;
  @Input() isSqlFromDL: boolean;
  @Input() highlightedNodes: DataLineageNode[];
  schemaInfo: any[] = [];
  loadState: LoaderState = LoaderState.loading;
  errorMessage: string;
  configSchemaRoute: string;
  tableConfig: MiningTableConfig;
  private dataPoints: MiningDataPointDefinitionWithPath[] = [];

  constructor(private readonly dataPointController: DataPointControllerService,
              private readonly graphQlService: GraphQlControllerService,
              private readonly httpClient: HttpClient,
              private translateService: TranslateService,
              private nzDrawerService: NzDrawerService,  @Inject(WindowToken) private $window: Window) {}


  ngOnInit(): void {
    this.dataPointController.getDataPointsForType(this.projectId, DATA_TYPE_NAME, [UsagesModel.UsagesEnum.miningUi_tableColumnsTable])
      .pipe(switchMap((response: MiningDataPointDefinitionWithPath[]) => {
        this.configureTableWithDataPoints(this.orderDataPointsByColumnIndex(response));

        return this.graphQlService.graphQl({query: graphQlQuery(QUERY_NAME, { projectId: this.projectId, moduleId: this.moduleId }, this.dataPoints, [])});
      }))
      .subscribe((response: any) => {
        this.getTableData(response.data.fieldInfos as Array<Record<any, any>>);
      });
  }

  /**
   * method to check if the content needs to be shown
   * @returns true or false
   */
  isContentAvailable(): boolean {
    return this.loadState !== LoaderState.nocontent;
  }

  /**
   * Opens drawer when the option is selected.
   * @param option has the option value and row data.
   */
  onOptionSelected(option: {optionValue: string, data: MiningTableRow}): void {
    switch (option.optionValue) {
      case 'edit': {
        /* there is only the 'edit' option currently */
        this.nzDrawerService.create({
          nzTitle: this.translateService.instant('dataPointEditor.drawerTitle'),
          nzContent: DataPointEditorComponent,
          nzContentParams: {
            dataTypeName: DATA_TYPE_NAME,
            data: this.schemaInfo[option.data.id],
            context: { projectId: this.projectId, moduleId: this.moduleId }
          },
          nzWidth: '40vw',
          nzPlacement: 'right',
          nzClosable: true,
          nzMaskClosable: false,
          nzWrapClassName: 'shared-side-viewer__drawer-editor--open'
        }).afterClose.subscribe((formResult: FormResult) => {
          if (formResult === FormResult.Saved) {
            this.graphQlService.graphQl({ query: graphQlQuery(QUERY_NAME, { projectId: this.projectId, moduleId: this.moduleId }, this.dataPoints, []) })
              .subscribe((response: any) => {
                this.getTableData(response.data.fieldInfos as Array<Record<any, any>>);
              });
          }
        });
        break;
      }
      case 'dataLineage': {
        const path = 'data-lineage?offset=-1&name=' + option.data.name + '&type=DATABASE_TABLE';
        openInNewTab(this.projectId, this.moduleId, path, this.$window);
        break;
      }
    }
  }

  private orderDataPointsByColumnIndex(dataPoints: MiningDataPointDefinitionWithPath[]): MiningDataPointDefinitionWithPath[] {
    return dataPoints
      .filter(dp => this.getDefaultColumnIndex(dp) !== undefined)
      .sort((a, b) => this.getDefaultColumnIndex(a) - this.getDefaultColumnIndex(b));
  }

  private getDefaultColumnIndex(dataPoint: MiningDataPointDefinitionWithPath): number | undefined {
    return +dataPoint.usageAttributes?.[UsagesModel.UsagesEnum.miningUi_tableColumnsTable]?.[UsagesModel.TableAttributesEnum.defaultColumnIndex];
  }

  private configureTableWithDataPoints(dataPoints: MiningDataPointDefinitionWithPath[]) {
    this.dataPoints = this.isSqlFromDL ? dataPoints.filter(dp => dp.name !== 'comment') : dataPoints;
    const columnMap: { [key: string]: Column } = {};
    this.dataPoints.forEach(dp => {
      const key = dp.path.split('.').join('_');
      columnMap[key] = {
        field: key,
        header: dp.displayName,
        fieldType: dp.scalarType === 'Int' || dp.scalarType === 'Long' || dp.scalarType === 'Float' ? FieldTypeEnum.NUMBER : FieldTypeEnum.STRING,
        filterProperties: this.isSqlFromDL ? { filterType: this.setFilterTypeForSqlTable(dp) }
          : { filterType: selectDataPointFilterType(dp, UsagesModel.UsagesEnum.miningUi_tableColumnsTable) },
        sortFn: this.setSortFnForSqlTable(dp),
      };
      this.tableConfig = {
        columnMap,
        isExportVisible: this.isSqlFromDL ? false : true,
        isClientSideExport: false,
        paginator: true,
        rows: this.isSqlFromDL ? 10 : DEFAULT_NUMBER_OF_ROWS,
        showTotalCount: this.isSqlFromDL ? false : true,
        actionsWidth: '100px',
        scroll: this.isSqlFromDL ? {x: '1000px'}: null,
        actions: this.isSqlFromDL ? [] : [[{ label: this.translateService.instant('btnLabel.edit'), value : 'edit'}, {
          type: LinkType.DROPDOWN,
          icon: 'more',
          options: [
             { label: this.translateService.instant('iconToolTip.openDataLineage'), value: 'dataLineage', disableItem: () => false}
          ]
        }]],
        externalExportCallback: () => this.exportCsv(),
      };
    });
  }

  private extractColumnData(d: Record<any, any>, index: number): Record<any, any> {
    const ret: any = {};
    for (const dp of this.dataPoints) {
      ret[dp.path.split('.').join('_')] = getValueAtPath(d, dp.path);
    }
    ret.id = index; /* required by MiningTableComponent */
    if (this.highlightedNodes && this.highlightedNodes.length) {
      const rowHighlightedNodeNames = this.highlightedNodes.map(node => node.name);
      ret.rowClassName = rowHighlightedNodeNames.includes(ret.name as string) ? 'mining-table__data-lineage-fields' : null;
    }
    return ret;
  }

  private exportCsv(): Observable<HttpResponse<Blob>> {
    const url = getBasePath() + `/api/v1/projects/${this.projectId}/export/datapoint-csv`;
    let queryParameters = new HttpParams({ encoder: new HttpUrlEncodingCodec() });
    queryParameters = queryParameters.set('$query', QUERY_NAME);
    queryParameters = queryParameters.set('projectId', this.projectId);
    queryParameters = queryParameters.set('moduleId', this.moduleId);
    for (const dp of this.dataPoints) {
      queryParameters = queryParameters.append('$columns', dp.path);
    }

    return this.httpClient.get(url, {
      observe: 'response',
      params: queryParameters,
      responseType: 'blob',
    });
  }

  private getTableData(data: Array<Record<any, any>>): void {
    if (data.length) {
      this.loadState = LoaderState.success;
      this.schemaInfo = data.map((d, index) => this.extractColumnData(d, index));
    } else {
      this.loadState = LoaderState.nocontent;
      this.configSchemaRoute = RouteBuilder.buildProjectRoute(this.projectId, 'configuration/schema');
    }
  }

  private setFilterTypeForSqlTable(dataPoint: MiningDataPointDefinitionWithPath): FilterType {
    if (dataPoint.name === 'name') {
      return selectDataPointFilterType(dataPoint, UsagesModel.UsagesEnum.miningUi_tableColumnsTable);
    } else {
      return null;
    }
  }

  private setSortFnForSqlTable(dataPoint: MiningDataPointDefinitionWithPath): boolean {
    return dataPoint.name === 'name' || dataPoint.name === 'ordinal' ? undefined : false;
  }

}
