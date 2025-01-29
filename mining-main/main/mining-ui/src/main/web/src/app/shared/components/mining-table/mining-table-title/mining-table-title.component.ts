import { HttpClient, HttpParams } from '@angular/common/http';
import { Component, ElementRef, Input, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { getBasePath } from '@app/core/utils/base-path.utils';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { Observable } from 'rxjs';
import { Subscription } from 'rxjs/internal/Subscription';
import { BulkAction, Column, MiningTableConfig, MiningTableRow } from '../mining-table-config.interface';
import FileSaveSupport from '@app/core/utils/file-save-support.utils';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { ActivatedRoute } from '@angular/router';
import { JobControllerService, MiningDataPointDefinitionWithPath } from '@innowake/mining-api-angular-client';
import { formatDate } from '@angular/common';

@Component({
  selector: 'mining-table-title',
  templateUrl: './mining-table-title.component.html'
})
export class MiningTableTitleComponent implements OnInit, OnDestroy {

  /**
   * The configuration of the parent table, used for the export functionality and the customize button
   */
  @Input() tableConfig: MiningTableConfig;

  @Input() moduleIdForPreFiltering: number[];

  /**
   * The columns of the parent table, used for the export
   */
  @Input() columns: Column[] = [];

  /**
   * The columns of the parent table, used for the export
   */
  @Input() rows: MiningTableRow[] = [];

  /**
   * The total of items in the table to be displayed in the title
   */
  @Input() total: number;

  @Input() selectedRecords: Set<number>;

  @Input() allowSelectDeselectRows: boolean;

  @Input() enableOrderedBusinessRuleCsv: boolean;

  @Input() preFiltersObject: { [key: string]: string | string[]; };

  @Input() usage: string;

  @Input() selectedReachabilityRow: string[];

  @ViewChild('toolbar', { static: true }) toolbar: ElementRef;

  searchValue = '';
  showBusinessRuleCsvButton: boolean;
  enableTaxonomyPropagationButton: boolean;
  userCustomizableDataPoint: Subscription;
  originalTooltips: BulkAction[] = [];
  private clientProjectSubscription: Subscription;
  private preFilterFromCallChain: string[];
  constructor(
    private featureToggle: FeatureToggleService,
    private translateService: TranslateService,
    private messageService: NzMessageService,
    private httpClient: HttpClient,
    private userCustomizableTableService: CustomizableTableColumnService,
    private customizableTableParameterService: CustomizableTableParametersService,
    private jobManager: JobManagerService,
    private jobController: JobControllerService,
    private route: ActivatedRoute
  ) { }

  ngOnInit(): void {
    this.featureToggle.isActive('orderedAnnotationRuleCsvExporter').subscribe((status: boolean) => {
      this.showBusinessRuleCsvButton = status;
    });
  }

  /**
   * Export data to a specific file format.
   * @param fileFormat the format of the file which needs to be exported
   */
  export(fileFormat: string): void {
    if (this.tableConfig.isClientSideExport) {
      this.clientSideExport(fileFormat);
    } else if (this.tableConfig.isCustomizable) {
      const format = `datapoint-${fileFormat}`;
      this.jobController.submitJobExtension(this.tableConfig.projectId, format, this.buildParams()).subscribe((jobId) => {
        const remoteJob = {
          jobId: jobId as unknown as string,
          uiToken: 'export-options-' + format,
          autoDownloadResult: true,
          foreground: true
        };
        this.jobManager.register(remoteJob);
      });
    } else {
      const loadingMessageId = this.messageService.loading(
        this.translateService.instant('exportOptionsComponent.exportInProgress') as string,
        { nzDuration: 0 }
      ).messageId;

      let exportCallback: Observable<any>;
      if (this.tableConfig.externalExportCallback) {
        exportCallback = this.tableConfig.externalExportCallback();
      } else {
        const url = getBasePath() + '/api/v1/projects/' + this.tableConfig.projectId + '/export/' + fileFormat + '?';
        const paramsObject = this.buildParams();
        const queryParam = new HttpParams({ fromObject: {...paramsObject} });
        exportCallback = this.httpClient.get(url, {
          observe: 'response',
          params: queryParam,
          responseType: 'blob',
        });
      }
      this.createExportFileLink(exportCallback, loadingMessageId);
    }
  }

  exportBusinessRules(): void {
    const queryParam = this.buildParamsForBusinessRules();
    this.jobManager.invokeExportJob('ordered-annotation-rule-csv', queryParam, this.tableConfig.projectId);
  }

  /**
   * clears the tool tip on clicking on the element
   * @param option the clicked option details
   */
  clearTooltip(option: BulkAction): void {
    if (this.tableConfig?.bulkActions?.length) {
      this.originalTooltips = this.tableConfig.bulkActions;
      option.tooltip = '';
    }
  }

  /**
   * Restores the tool tip on clicking or hovering the element again for multiple subActions.
   * @param bukAction The hovered option details
   */
  restoreTooltip(bukAction: BulkAction): void {
    if (bukAction.label === this.translateService.instant('bulkActionButtonLbls.identify') && bukAction?.subActions?.length > 0) {
      const identifyCandidatesLabel = this.translateService.instant('bulkActionButtonLbls.identifyCandidatesbuttonLabel');
      const identifyCandidatesTooltip = this.translateService.instant('bulkActionButtonLbls.identifyCandidatesbuttonTooltipText');
      const identifyDeadCodeLabel = this.translateService.instant('bulkActionButtonLbls.identifyDeadCodebuttonLabel');
      const identifyDeadCodeTooltip = this.translateService.instant('bulkActionButtonLbls.identifyDeadCodeButtonTooltipText');
      bukAction.subActions.forEach((subAction: BulkAction) => {
        if (subAction.label === identifyCandidatesLabel) {
          subAction.tooltip = identifyCandidatesTooltip;
        } else if (subAction.label === identifyDeadCodeLabel) {
          subAction.tooltip = identifyDeadCodeTooltip;
        }
      });
    }
  }

  ngOnDestroy(): void {
    this.userCustomizableDataPoint?.unsubscribe();
    this.clientProjectSubscription?.unsubscribe();
  }

  private clientSideExport(fileFormat: string) {
    let csvContent = this.columns.map((column: Column) => this.translateService.instant(column.header) as string).join(',') + '\n';
    csvContent += this.rows.map((row: any) =>
      this.columns.map((col: Column) =>
        this.getRowValue(row, col)).join(',')
    ).join('\n');
    this.messageService.success(this.translateService.instant('exportOptionsComponent.exportSuccess') as string);
    void FileSaveSupport.save(csvContent, this.getExportFileName(this.tableConfig.exportType, this.tableConfig.projectId, fileFormat));
  }

  private getRowValue(row: any, val: Column) {
    return row[val.field];
  }

  private getExportFileName(exportType: string, projectId: number, fileFormat: string) {
    const formattedDate = formatDate(new Date(), 'yyyyMMddhhmmss', this.translateService.currentLang);
    return exportType + '_' + projectId + '_' + formattedDate + '.' + fileFormat;
  }

  private buildParams(): { [key: string]: string[] } {
    let params = {};
    if (this.tableConfig.isCustomizable) {
      // To make the params object compatible with the request object, we need to convert to arrays.
      params['$query'] = [];
      params['$columns'] = [];
      params['filterObject'] = [];
      params['$query'].push(this.tableConfig.exportType);
      this.userCustomizableDataPoint = this.userCustomizableTableService.getSelectedDataPoints().subscribe((res: MiningDataPointDefinitionWithPath[]) => {
        res.forEach(element => {
          if (typeof params['$columns'] == 'undefined') {
            params['$columns'] = [];
          }
          params['$columns'].push(element.path);
        });
        const isPreFilterFromQuery: string = this.route.snapshot.queryParams['preFilter'] || '';
        const idsFromLocalStorage = localStorage.getItem(isPreFilterFromQuery);
        if (isPreFilterFromQuery && idsFromLocalStorage) {
          this.preFilterFromCallChain = JSON.parse(idsFromLocalStorage).moduleIds;
        }
        const filterObject = this.customizableTableParameterService.handleFilters(res, this.preFilterFromCallChain);
        params['filterObject'].push(JSON.stringify(filterObject));
        if (this.preFiltersObject) {
          params['filterObject'] = [JSON.stringify(this.preFiltersObject)];
        }
        const sort = this.customizableTableParameterService.getSortObject();
        if (sort) {
          params['sortObject']= [sort.replace(/(\w+): (\w+)/g, '"$1": "$2"')];
        }
      });
    } else {
      params['type'] = this.tableConfig.exportType;
      this.columns.forEach((column) => {
        if (typeof params['columns'] == 'undefined') {
          params['columns'] = [];
        }
        params['columns'].push(this.translateService.instant(column.header) as string);
      });
      if (this.tableConfig.filters) {
        params['filters'] = this.tableConfig.filters;
      }
    }

    if (this.tableConfig?.exportParameters) {
      if (this.tableConfig?.isCustomizable) {
        // To Make sure that all values are arrays.
        for (const [key, value] of Object.entries(this.tableConfig.exportParameters)) {
          if ( ! Array.isArray(value)) {
            this.tableConfig.exportParameters[key] = [value];
          }
        }
      }
      params = {...params, ...this.tableConfig.exportParameters};
    }
    return params;
  }

  private buildParamsForBusinessRules(): object {
    const params = {};
    params['moduleId'] = this.moduleIdForPreFiltering.join(',');
    params['$query'] = this.tableConfig.exportType;
    this.userCustomizableDataPoint = this.userCustomizableTableService.getSelectedDataPoints().subscribe((res: MiningDataPointDefinitionWithPath[]) => {
      res.forEach(element => {
        if (typeof params['$columns'] == 'undefined') {
          params['$columns'] = [];
        }
        params['$columns'].push(element.path);
      });
      const hasSourceAttachmentColumn = params['$columns'].some((column: string) => column === 'content.sourceAttachment');
      if ( ! hasSourceAttachmentColumn) {
        params['$columns'].push('content.sourceAttachment');
      }
      const filterObject = this.customizableTableParameterService.handleFilters(res, this.preFilterFromCallChain);
      params['filterObject'] = JSON.stringify(filterObject);
    });
    return params;
  }

  private createExportFileLink(exportCallback: Observable<any>, loadingMessageId: string) {
    exportCallback.subscribe(
      (blob) => {
        this.messageService.remove(loadingMessageId);
        this.messageService.success(this.translateService.instant('exportOptionsComponent.exportSuccess') as string);
        const downloadLink = document.createElement('a');
        const objectUrl = URL.createObjectURL(blob.body as Blob | MediaSource);
        downloadLink.href = objectUrl;
        downloadLink.download = blob.headers
          .get('content-disposition')
          .split(';')[1]
          .split('filename')[1]
          .split('=')[1]
          .trim();
        downloadLink.click();
        URL.revokeObjectURL(objectUrl);
      }, () => {
        this.messageService.remove(loadingMessageId);
        this.messageService.error(this.translateService.instant('exportOptionsComponent.exportFailure') as string);
      });
  }
}

