import { Component, EventEmitter, Inject, Input, OnChanges, OnInit, Output } from '@angular/core';
import { DNAAnalysisService, buttonLabels } from '@app/core/services/dna-analysis.service';
import { MiningTableRow, OptionItem } from '../mining-table/mining-table-config.interface';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { MiningTableOptionSelected } from '../mining-table/mining-table-option-selected.interface';
import { WindowToken } from '@app/core/utils/window';
import { LinkType } from '../mining-table/mining-table-action.interface';
import { TranslateService } from '@ngx-translate/core';
import { Usages, TypeName } from '@app/shared/interfaces/datapoints-labels.interface';
import { AbstractControl, UntypedFormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { ChartData } from '@app/modules/dna-analysis/dna-card.interface';
import { DiscoveryControllerService, EntityId } from '@innowake/mining-api-angular-client';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';

@Component({
  selector: 'dna-chart-details',
  templateUrl: './dna-chart-details.component.html',
  providers: [CustomizableTableParametersService, CustomizableTableColumnService]
})
export class DnaChartDetailsComponent implements OnChanges, OnInit {
  @Output() showChartDetails: EventEmitter<boolean> = new EventEmitter();
  @Input() selectedClusterDetails: ChartData[] = [];
  @Input() filters: Record<string, string>;
  @Input() chartTitle: string;
  @Input() projectId: number;
  @Input() moduleId: EntityId;
  @Input() algorithm: string;
  @Input() updatedTime: string;
  @Input() isModuleList = false;
  loadSave = false;
  additionalGraphQlParams = {};
  usage: Usages =  Usages.DNATABLE;
  pageType = TypeName.PAGEDNA;
  totalRecords: number;
  graphQlType = 'dnaModulesInCluster';
  rowActions: OptionItem[][];
  disableEdit = true;
  SubmitForm: UntypedFormGroup;
  filterResult: (data: MiningTableRow) => boolean;
  internalDataPoints: any[] = [
    { name: 'id', path: 'content.module.id' },
    { name: 'storage', path: 'content.module.storage' },
    { name: 'inCodebase', path: 'content.module.inCodebase' },
    { name: 'identification', path: 'content.module.technology' },
    { name: 'technology', path: 'content.module.identification' },
    { name: 'type', path: 'content.module.type' },
  ];

  constructor(
    @Inject(WindowToken) private $window: Window,
    private fb: UntypedFormBuilder,
    private dnaAnalysis: DNAAnalysisService,
    private translateService: TranslateService,
    private discoveryControllerService: DiscoveryControllerService) { }

  get validateName(): AbstractControl {
    return this.SubmitForm.get('title');
  }

  ngOnInit(): void {
    if(this.selectedClusterDetails.length) {
      const clusterData = this.selectedClusterDetails[0];
      this.SubmitForm = this.fb.group({
        name: [clusterData.title ? clusterData.title : clusterData.index, Validators.required],
        description: [clusterData.description ? clusterData.description : '']
      });
    }
  }

  ngOnChanges(): void {
    if (this.isModuleList && this.moduleId) {
      this.filterResult = (data: MiningTableRow) => this.filterDNAData(data);
    }
    const clusterIndex = this.filters['cluster'];
    const filteredCluster = clusterIndex ? clusterIndex === 'Unassigned' ? -1 : +clusterIndex : null;
    this.additionalGraphQlParams['algorithm'] = this.algorithm;
    if (this.updatedTime) {
      this.additionalGraphQlParams['updatedTime'] = this.updatedTime;
    }
    if (filteredCluster) {
      this.additionalGraphQlParams['clusterIndex'] = filteredCluster;
    }
    this.rowActions = [[
    {
      type: LinkType.DROPDOWN,
      icon: 'more',
      options: [
        {
          label: this.translateService.instant('iconToolTip.codeViewer'), value: buttonLabels.codeViewer,
          optionToolTip: (data: MiningTableRow) => this.translateService.instant(this.dnaAnalysis.disableTableAction(buttonLabels.codeViewer, data).toolTip),
          disableItem: (data: MiningTableRow) => this.dnaAnalysis.disableTableAction(buttonLabels.codeViewer, data).disableButton
        },
        {
          label: this.translateService.instant('iconToolTip.controlFlow'), value: buttonLabels.controlFlow,
          optionToolTip: (data: MiningTableRow) => this.translateService.instant(this.dnaAnalysis.disableTableAction(buttonLabels.controlFlow, data).toolTip),
          disableItem: (data: MiningTableRow) => this.dnaAnalysis.disableTableAction(buttonLabels.controlFlow, data).disableButton
        },
        { label: this.translateService.instant('iconToolTip.depGraph'), value: 'dependencies', disableItem: () => false },
      ]
    },
    ]];
  }

  /**
   * Closes the chart detail drawer.
   */
  closeChartDetails(): void {
    this.showChartDetails.emit(false);
  }

  /**
   * Enable editing of form on clicking edit
   */
  enableEdit(): void {
    this.disableEdit = false;
  }

  /**
   * method to submit DNA title and description
   * @param formData: Details containing DNA title and description
   */
  onSubmit(formData: UntypedFormGroup): void {
    this.loadSave = true;
    this.discoveryControllerService
    .updateDnaCommunity(this.projectId,
      this.selectedClusterDetails[0].uuid, formData.value.name as string ,formData.value.description as string)
      .subscribe(() => {
      this.disableEdit = true;
      this.loadSave = false;
    });
  }

  /**
   * Handles selected Option of screen navigations.
   * @param rowData gives the details of screen
   */
  handleSelectedOption(rowData: MiningTableOptionSelected): void {
    const moduleId: number = rowData.data?.module.id;
    openInNewTab(this.projectId, moduleId, rowData.optionValue, this.$window);
  }

  private filterDNAData(data: MiningTableRow): boolean {
    return data.clusterIndex === this.additionalGraphQlParams['clusterIndex'] && data.module.id !== this.moduleId;
  }
}
