import { Component, OnInit, Input, Inject } from '@angular/core';
import { MiningTableConfig, MiningTableRow } from '@app/shared/components/mining-table/mining-table-config.interface';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { HotSpotVO } from '@app/shared/models/hotspot-vo.model';
import { LinkType } from '@app/shared/components/mining-table/mining-table-action.interface';
import { TranslateService } from '@ngx-translate/core';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { WindowToken } from '@app/core/utils/window';
import { handleCodeViewerButton } from '@app/shared/components/code-viewer-button/code-viewer-button.component';
import { buttonLabels } from '@app/core/services/dna-analysis.service';
import { HotSpot, HotSpotControllerService } from '@innowake/mining-api-angular-client';
import { CfgSupportedTypeService } from '@app/core/services/cfg-supported-type.service';

export class HotSpotConfig {
  type: string;
  rows: number;
  header: string;
  modules: HotSpotVO[];
  loadState: LoaderState = LoaderState.loading;
  tableConfig: MiningTableConfig;
  noContentMessage = 'hotspots.config.noContentMsg';
  errorMessage = 'hotspots.config.errorMsg';

  constructor(header: string, type: string, rows: number, tableConfig: MiningTableConfig) {
    this.type = type;
    this.rows = rows;
    this.header = header;
    this.tableConfig = tableConfig;
  }
}

@Component({
  selector: 'mn-hotspot-card',
  templateUrl: './hotspot-card.component.html'
})
export class HotSpotCardComponent implements OnInit {
  @Input() clientProjectRelationship: ClientProjectRelationship;
  @Input() config: HotSpotConfig;
  loadState = LoaderState;

  constructor(
    private hotSpotControllerService: HotSpotControllerService,
    private translateService: TranslateService,
    private cfgSupportedTypesService: CfgSupportedTypeService,
    @Inject(WindowToken) private $window: Window,
  ) {
  }

  ngOnInit(): void {
    if (this.clientProjectRelationship) {
      const actions = [[
        {
          type: LinkType.DROPDOWN,
          icon: 'more',
          options: [
            {
              label: this.translateService.instant('iconToolTip.codeViewer'), value: buttonLabels.codeViewer,
              optionToolTip: (data: MiningTableRow) => this.translateService.instant(this.disableTableAction(buttonLabels.codeViewer, data).toolTip),
              disableItem: (data: MiningTableRow) => this.disableTableAction(buttonLabels.codeViewer, data).disableButton
            },
            {
              label: this.translateService.instant('iconToolTip.controlFlow'), value: buttonLabels.controlFlow,
              optionToolTip: (data: MiningTableRow) => this.disableTableAction(buttonLabels.controlFlow, data).toolTip,
              disableItem: (data: MiningTableRow) => this.disableTableAction(buttonLabels.controlFlow, data).disableButton
            },
            { label: this.translateService.instant('iconToolTip.depGraph'), value: 'dependencies', disableItem: () => false},
          ]
        },
      ]];
      this.config.tableConfig.isExportVisible = false;
      this.config.tableConfig.projectId = this.clientProjectRelationship.getProjectId();
      this.config.tableConfig.exportType = this.config.type.toLowerCase();
      this.config.tableConfig.actions = actions;
      this.hotSpotControllerService
        .getHotspots(this.clientProjectRelationship.getProjectId(), this.config.type, this.config.rows)
        .subscribe((response: HotSpot[]) => {
          this.config.tableConfig.loading = true;
          this.config.modules = this.getHotSpotsFromResponse(response);
          if (this.config.modules.length) {
            this.config.loadState = LoaderState.success;
          } else {
            this.config.loadState = LoaderState.nocontent;
          }
          this.config.tableConfig.loading = false;
        },
          error => {
            this.config.loadState = LoaderState.error;
            this.config.errorMessage = error.message;
          }
        );
    }
  }

  /**
   * handles selected Option of screen navigations.
   * @param rowData gives the details of screen
   */
  handleSelectedOption(rowData: MiningTableOptionSelected): void {
    let moduleId: number = rowData.data.id;
    if (rowData.data.optionValue === buttonLabels.codeViewer) {
      moduleId = rowData.data.containingModuleId !== null ? rowData.data.containingModuleId : rowData.data.id;
    }
    openInNewTab(this.config.tableConfig.projectId, moduleId, rowData.optionValue, this.$window);
  }

  private getHotSpotsFromResponse(response: HotSpot[]): HotSpotVO[] {
    if (response && response.length) {
      return response.map(
        hotSpot => new HotSpotVO(
          hotSpot.module.id,
          hotSpot.module.parent,
          hotSpot.module.name,
          hotSpot.module.technology,
          hotSpot.module.type,
          hotSpot.count.toString(),
          hotSpot.module.identification,
          hotSpot.module.storage,
          hotSpot.module.sourceCodeAvailable,
          hotSpot.module.linkHash
        )
      );
    } else {
      return [];
    }
  }

  private disableTableAction(butttonLabel: string, data: MiningTableRow): { disableButton: boolean; toolTip: string } {
    switch (butttonLabel) {
      case buttonLabels.codeViewer:
        return handleCodeViewerButton(data);
      case buttonLabels.controlFlow:
        const actuallySupported = this.cfgSupportedTypesService.checkIfSupported(data.language as string, data.type as string);
        return {
          disableButton: ! (actuallySupported && data.sourceCodeAvailable),
          toolTip: this.translateService.instant('cfgNotAvailable')
        };
    }
  }
}
