import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { Logger } from '@app/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { DnaCard } from './dna-card.interface';
import { Subscription } from 'rxjs';
import { HttpErrorResponse } from '@angular/common/http';
import { DNAAnalysisService } from '@app/core/services/dna-analysis.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { TranslateService } from '@ngx-translate/core';
import { dateFormatter } from '@app/core/utils/date-formatter.utils';
import { NzMessageService } from 'ng-zorro-antd/message';
import { ChartFilters } from '../metrics/shared/components/metrics-card/metrics-card.interface';
import { ChartData } from '@app/modules/dna-analysis/dna-card.interface';
import { DiscoveryControllerService, JobInformation, ModelDna, ProjectRole } from '@innowake/mining-api-angular-client';


const log = new Logger('DnaAnalysisComponent');

@Component({
  selector: 'app-dna-analysis-module',
  templateUrl: 'dna-analysis.component.html'
})
export class DnaAnalysisComponent implements OnInit, OnDestroy {

  projectId: number;
  isShowDnaChart = true;
  loadState: LoaderState;
  dnaChartsData: ModelDna;
  dnaCardList: DnaCard[] = [];
  selectedCard: DnaCard;
  chartDataFilters: ChartFilters;
  showChartDetails = false;
  canExecuteDNA = false;
  selectedDnaSnapshot: string;
  selectedClusterDetails: ChartData[] = [];
  snapshotList: Array<{ value: string, label: string }> = [];
  private clientProjectSubscription: Subscription;

  constructor(
    public translateService: TranslateService,
    private discoveryControllerService: DiscoveryControllerService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private jobManagerService: JobManagerService,
    private authorizationService: KeycloakAuthorizationService,
    private dnaAnalysis: DNAAnalysisService,
    private messageService: NzMessageService,
    public route: ActivatedRoute,
    public router: Router) { }

  ngOnInit(): void {
    this.loadState = LoaderState.loading;
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.canExecuteDNA = this.authorizationService.hasUserRole(response, ProjectRole.UserRoleEnum.MANAGER);
      this.projectId = response.getProjectId();
      this.route.queryParams.subscribe((param) => {
        if (Object.keys(param).length === 0) {
          this.showChartDetails = false;
          this.navigateToDnaUrl();
          this.getDnaSnapshots();
        } else {
          this.selectedDnaSnapshot = param.snapshot;
          if ( ! param.sequencer) {
            this.getDnaSnapshots(false);
          }
          this.getChartData(param.sequencer as string, +param.cluster);
        }
      });

    });
  }

  /**
   * Opens the detail table in the drawer for the specific card.
   *
   * @param filter filter string used to fetch the data from the server.
   * @param index index of the card in the card list.
   */
  openChartDetailsTable(filter: Record<string, number>, index: number): void {
    this.showChartDetails = true;
    this.selectedCard = this.dnaCardList[index];
    this.selectedClusterDetails.length = 0;
    if( filter.index && filter.index !== -1) {
      this.selectedClusterDetails.push(this.selectedCard.chartData[filter.index-1]);
    }
    let queryParams: { [key: string]: any } = {};
    queryParams = {
      snapshot: this.selectedDnaSnapshot,
      sequencer: this.selectedCard?.title,
      cluster: filter.index,
    };
    this.navigateToDnaUrl(queryParams);
    this.getChartDetails(filter, this.dnaCardList[index]);
  }

  /**
   *Triggers Job Manager on Clicking and updating the DNA data
   */
   analyzeDNA(): void {
    this.discoveryControllerService.discoverDNA(this.projectId).subscribe((response: string[]) => {
      this.jobManagerService.register({ jobId: response.toString(), foreground: true, cancellable: true }).status$.subscribe((result: string) => {
        if (result === JobInformation.StatusEnum.SUCCESS) {
          this.loadState = LoaderState.loading;
          this.getDnaSnapshots();
        }
      });
    }, (err: HttpErrorResponse) => {
      log.error(err);
    });
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription.unsubscribe();
  }

  /**
   * Gets DNA chart data based on the selected DNA snapshot.
   * @param sequencer selected DNA sequencer.
   * @param cluster selected DNA cluster.
   */
  getChartData(sequencer?: string, cluster?: number): void {
    this.showChartDetails = sequencer ? true : false;
    this.discoveryControllerService.modelDNAForSelectedTimestamp(this.projectId, this.selectedDnaSnapshot).subscribe((dnaChartsData: ModelDna) => {
      if (dnaChartsData && dnaChartsData.clusterings) {
        this.loadState = LoaderState.success;
        this.isShowDnaChart = true;
        this.dnaChartsData = dnaChartsData;
        this.dnaCardList = this.dnaAnalysis.createChartData(this.dnaChartsData, [], true);
        this.fetchDataBasedOnURLParams(cluster, sequencer);
      } else {
        this.loadState = LoaderState.error;
        this.isShowDnaChart = false;
      }
    }, (err: HttpErrorResponse) => {
      this.navigateToDnaUrl({}, 'snapshot');
      log.error(err);
      this.loadState = LoaderState.error;
      this.isShowDnaChart = false;
    });
  }

  /**
   * Add snapshot to DNA URL.
   * @param selectedSnapshot selected snapshot through dropdown.
   */
  addSnapshotToUrl(selectedSnapshot?: string): void {
    this.showChartDetails = false;
    this.selectedDnaSnapshot = selectedSnapshot ? selectedSnapshot : this.selectedDnaSnapshot;
    let queryParams: { [key: string]: any } = {};
    queryParams = {
      snapshot: this.selectedDnaSnapshot
    };
    this.navigateToDnaUrl(queryParams);
  }

  /**
   * Navigates to the respective DNA URL.
   * @param queryParams route parameters.
   * @param errorMessage error message to be displayed.
   */
  private navigateToDnaUrl(queryParams?: Params, errorMessage?: string): void {
    const route = ['project-'+this.projectId+'/dna'];
    this.router.navigate(
      route,
      {
        queryParams: queryParams ? queryParams : {}
      }
    ).catch(() => { });
    if (errorMessage) {
      this.messageService.error(this.translateService.instant('dnaAnalysis.dnaErrorMessage', { errorComponent: errorMessage }) as string);
    }
  }

  private getDnaSnapshots(showChartData: boolean = true) {
    this.discoveryControllerService.listTheTimestampsOfDNASnapshots(this.projectId).subscribe((snapshots) => {
      if (snapshots.length) {
        this.snapshotList = snapshots.map(dnaSnapshot => ({
          value: dnaSnapshot,
          label: dateFormatter(new Date(+dnaSnapshot))
        }));
        if (showChartData) {
          this.selectedDnaSnapshot = snapshots[0];
          this.getChartData();
        }
      } else {
        this.isShowDnaChart = false;
      }
    });
  }

  private getChartDetails(filter: Record<string, number>, dnaCard: DnaCard) {
    this.chartDataFilters = this.dnaAnalysis.openChartDetailsTable(filter, dnaCard);
  }

  private fetchDataBasedOnURLParams(cluster: number, sequencer: string): void {
    if (this.showChartDetails) {
      const filter = cluster ? { index: +cluster } : {};
      const dnaCard = this.dnaCardList.find((card) => card.title === sequencer);
      const index = dnaCard?.chartData.findIndex((data) => data.index === cluster);
      if ( ! dnaCard) {
        this.navigateToDnaUrl({}, 'sequencer');
      } else if (cluster && index === -1) {
        this.navigateToDnaUrl({}, 'cluster');
      } else {
        this.selectedCard = dnaCard;
        this.selectedClusterDetails.length = 0;
        if( filter.index && filter.index !== -1) {
          this.selectedClusterDetails.push(this.selectedCard.chartData[filter.index-1]);
        }
        this.getChartDetails(filter, this.selectedCard);
      }
    }
  }
}
