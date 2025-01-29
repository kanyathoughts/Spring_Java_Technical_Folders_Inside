import { Injectable } from '@angular/core';
import { ChartData, DnaCard } from '@app/modules/dna-analysis/dna-card.interface';
import { ChartFilters } from '@app/modules/metrics/shared/components/metrics-card/metrics-card.interface';
import { MiningTableRow } from '@app/shared/components/mining-table/mining-table-config.interface';
import { handleCodeViewerButton } from '@app/shared/components/code-viewer-button/code-viewer-button.component';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateService } from '@ngx-translate/core';
import { ModelAlgorithmOption, ModelCluster, ModelClustering, ModelDna, ModelDnaCluster } from '@innowake/mining-api-angular-client';
import { CfgSupportedTypeService } from './cfg-supported-type.service';

export const buttonLabels = {
  codeViewer: 'code-viewer',
  controlFlow: 'control-flow'
};

@Injectable({
  providedIn: 'root'
})
export class DNAAnalysisService {
  constructor(
    private numberFormatter: NumberFormatter,
    private translateService: TranslateService,
    private cfgSupportedService: CfgSupportedTypeService
  ) { }

  /**
   * Creates the data to display the chart.
   *
   * @param dnaChartsData DNA data received from response.
   * @param clusters clusters the module belongs to.
   * @returns array of DNA chart data.
   */
  createChartData(dnaChartsData: ModelDna, clusters?: ModelDnaCluster[], showCustomToolTip = false): DnaCard[] {
    const dnaCardList: DnaCard[] = [];
    dnaChartsData.clusterings.sort((a: ModelClustering, b: ModelClustering) => a.algorithm.Sequencer.localeCompare(b.algorithm.Sequencer));
    dnaChartsData.clusterings.forEach((dnaChartData: ModelClustering) => {
      const chartData: ChartData[] = [];
      const algorithmProperties: ModelAlgorithmOption[] = [];
      Object.keys(dnaChartData.algorithm).forEach((algorithmPropertiesItem: string) => {
        if (algorithmPropertiesItem !== 'Sequencer') {
          algorithmProperties.push({
            title: algorithmPropertiesItem,
            value: dnaChartData.algorithm[algorithmPropertiesItem]
          });
        }
      });
      dnaChartData.options = [...algorithmProperties, ...dnaChartData.options];
      chartData.length = 0;
      let sequencerModuleCount = 0;
      let clustersLength = 0;
      let assignedModuleCount = 0;
      let similarModuleCount = 0;
      dnaChartData.clusters.forEach((clusterItem) => {
        sequencerModuleCount = clusterItem.moduleCount + sequencerModuleCount;
        if (clusterItem.clusterIndex !== -1) {
          clustersLength++;
          assignedModuleCount = clusterItem.moduleCount + assignedModuleCount;
          clusters?.forEach((cluster) => {
            if (clusterItem.clusterIndex === cluster.clusterIndex) {
              similarModuleCount = clusterItem.moduleCount;
            }
          });
        }
      });
      dnaChartData.clusters.forEach((cluster: ModelCluster) => {
        const {clusterIndex , clusterDescription,} = cluster;
        let clusterTitle = clusterIndex === -1 ? 'dnaAnalysis.unAssignedChartToolTipTxt' : 'dnaAnalysis.assignedChartToolTipTxt';
        if (cluster.clusterTitle && showCustomToolTip && clusterIndex !== -1) {
          clusterTitle = 'dnaAnalysis.assignedChartToolTipTxtWithDesc';
        }
        let key = this.translateService.instant(clusterTitle,
          {
            clusterPercentage: this.numberFormatter.transform((cluster.moduleCount / sequencerModuleCount) * 100),
            noOfModules: cluster.moduleCount, clusterIndex: clusterIndex ? clusterIndex : ''
          });
        if(clusterIndex !== -1) {
          key = cluster.clusterTitle && showCustomToolTip ? cluster.clusterTitle + key : key;
          key = clusterDescription && showCustomToolTip ? `${key}:${clusterDescription}` : key;
        }
        const clusterItem: ChartData = {
          index: clusterIndex,
          key: key+':'+clusterIndex,
          value: cluster.moduleCount,
          description : cluster.clusterDescription,
          title : cluster.clusterTitle,
          uuid : cluster.communityId
        };
        clusterIndex === -1 ? chartData.push(clusterItem): chartData.splice(clusterIndex, 0, clusterItem);
      });
      // Move the unassigned cluster at the end
      chartData.push(chartData.splice(chartData.findIndex(element => element.index === -1), 1)[0]);
      const cardObj = {
        chartData,
        options: dnaChartData.options,
        title: dnaChartData.algorithm.Sequencer,
        clusterModuleCount: sequencerModuleCount,
        assignedModuleCount,
        similarModuleCount,
        clustersLength,
        chartFilterData: {
          filterArgs: ['index'],
          queryFilterBuilder: (chartValues?: Record<string, number>): ChartFilters => this.generateFilterQuery(chartValues)
        }
      };
      if (clusters?.length > 0) {
        clusters.forEach((cluster) => {
          const algorithumCheck =
            Object.keys(cluster.algorithm).length === Object.keys(dnaChartData.algorithm).length &&
            (Object.keys(cluster.algorithm) as Array<keyof typeof cluster.algorithm>).every((key) =>
              dnaChartData.algorithm.hasOwnProperty(key) && cluster.algorithm[key] === dnaChartData.algorithm[key]
            );
          if (algorithumCheck) {
            const cardItem = { ...cardObj };
            cardItem.title = dnaChartData.algorithm['Sequencer'];
            cardItem['selectedIndex'] = cluster.clusterIndex;
            dnaCardList.push(cardItem);
          }
        });
      } else {
        dnaCardList.push(cardObj);
      }
    });
    return dnaCardList;
  }

  /**
   * Opens the detail table in the drawer for the specific card.
   *
   * @param filter filter string used to fetch the data from the server.
   * @param dnaCardData DNA card data.
   * @returns Chart Data with filterData, chartTableconfig, requestQuery, totalRecords.
   */
  openChartDetailsTable(filter: Record<string, number>, dnaCardData: DnaCard): ChartFilters {
    if (dnaCardData) {
      return dnaCardData.chartFilterData?.queryFilterBuilder(filter);
    }
  }

  /**
   * Disables DNA Table action.
   *
   * @param butttonLabel button label.
   * @param data row data of the table.
   * @returns flag to disable button and respective tooltip.
   */
  disableTableAction(butttonLabel: string, data: MiningTableRow): { disableButton: boolean; toolTip: string } {
    switch (butttonLabel) {
      case 'code-viewer':
        data['storage'] = data.module.storage;
        data['sourceCodeAvailable'] = data.module.inCodebase;
        data['identification'] = data.module.identification;
        return handleCodeViewerButton(data);
      case 'control-flow':
        const actuallySupported = this.cfgSupportedService.checkIfSupported(data.module.technology + '', data.module.type + '');
        return { disableButton: ! (actuallySupported && data.module.inCodebase), toolTip: this.translateService.instant('cfgNotAvailable') };
    }
  }

  private generateFilterQuery(chartValues: Record<string, number>): ChartFilters {
    const filters = {};
    if (Object.keys(chartValues).length !== 0) {
      if (chartValues['index'] !== -1) {
        filters['cluster'] = chartValues['index'];
      } else {
        filters['cluster'] = this.translateService.instant('unassigned');
      }
    }
    return { filters };
  }
}
