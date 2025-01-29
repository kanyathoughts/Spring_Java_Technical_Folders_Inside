import { Component, Input, OnChanges, OnInit, SimpleChanges,OnDestroy, EventEmitter, Output } from '@angular/core';
import { ReachabilityCallchainData } from '../utils/reachability-interface';
import { DependencyGraph, ReachabilityBlockGraphFilterRequest, FunctionalBlockControllerService, ReachabilityNetworkGraphFilterRequest }
 from '@innowake/mining-api-angular-client';
import { ReachabilityService } from '../utils/reachability.service';
import { ActivatedRoute } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';

let wrongBlockIdMessage = '';

@Component({
  selector: 'app-reachability-block-graph',
  templateUrl: './reachability-block-graph-component.html'
})
export class ReachabilityBlockGraphComponent implements OnInit, OnChanges, OnDestroy {

  @Input() networkUid = '';
  @Input() networkFilter: ReachabilityNetworkGraphFilterRequest;
  @Input() isGraphUpdated = true;
  @Output() isNetworkGraphVisible = new EventEmitter<boolean>();
  reachabilityBlock: ReachabilityCallchainData;
  reachabilityGraph: DependencyGraph;
  blockId: string;
  projectId: number;
  selectedDepth = -1;
  breadcrumbItems: string[] = [];
  noGraphDataText: string;
  showGraph = false;
  filterParams: ReachabilityBlockGraphFilterRequest;
  updateSubscription: Subscription;

  constructor(private reachabilityService: ReachabilityService,
    public route: ActivatedRoute,
    private functionalBlockControlerService: FunctionalBlockControllerService,
    private translateService: TranslateService) { }

  ngOnInit(): void {
    this.projectId = this.route.snapshot.params['projectId']?.split('-')[1];
    if (this.networkUid) {
      this.fetchNetworkGraphDetails(this.networkUid, this.networkFilter as { [key: string]: object; });
    } else {
      this.route.queryParams.subscribe((params) => {
        this.blockId = JSON.stringify(this.route.snapshot.params['blockId']);
        this.breadcrumbItems.push(`${this.translateService.instant('reachability.reachabilityBlocks')}`,
          params?.name as string);
        wrongBlockIdMessage = this.translateService.instant('reachability.wrongBlockId', { wrongId: this.blockId }) as string;
        if (Object.values(params.type as string[]).includes('MERGE_PARENT')) {
          this.fetchBlockGraphDetails(this.route.snapshot.params['blockId'] as string);
        } else {
          this.fetchReachabilityBlockDetails();
        }
      });
    };
   this.updateSubscription =  this.reachabilityService.getUpdateGraph().subscribe((res: boolean) => {
      if (this.networkUid && res) {
        this.fetchNetworkGraphDetails(this.networkUid, this.networkFilter as { [key: string]: object; });
      }
    });
  }

  ngOnChanges(simpleChange: SimpleChanges): void {
    if (this.projectId && simpleChange.networkFilter && simpleChange.networkFilter.currentValue) {
      this.fetchNetworkGraphDetails(this.networkUid, simpleChange.networkFilter.currentValue as { [key: string]: object });
    }
  }


  /**
   * Method to fetch the reachability block call chain details.
   */
  fetchReachabilityBlockDetails(): void {
    this.reachabilityService.fetchReachabilityCallChainDetails(this.projectId, this.blockId).subscribe((callChainData: ReachabilityCallchainData) => {
      if (callChainData && callChainData?.callChain?.content.length) {
        this.fetchBlockGraphDetails(callChainData.callChain.content[0].uid);
      } else {
        this.noGraphDataText = wrongBlockIdMessage;
      }
    });
  }

  /**
   * Method to capture the filter params from the reachability graph component
   * @param filterParams - filter params from the reachability graph component
   */
  graphFilterChange(filterParams: ReachabilityBlockGraphFilterRequest): void {
    this.filterParams = filterParams;
    this.fetchReachabilityBlockDetails();
  }

  ngOnDestroy(): void {
    this.updateSubscription?.unsubscribe();
  }

  private fetchBlockGraphDetails(callChainBlockId: string) {
    this.showGraph = false;
    this.functionalBlockControlerService.getFunctionalBlockAsDependencyGraph(this.projectId, callChainBlockId, this.filterParams).subscribe((graphData) => {
      if (graphData && graphData?.modules?.length) {
        this.reachabilityGraph = graphData;
        this.isNetworkGraphVisible.emit(true);
      } else if ( ! graphData?.modules?.length) {
        this.noGraphDataText = this.translateService.instant('reachability.noGraphData', { reachabilityBlockName: this.breadcrumbItems[1] });
      } else {
        this.noGraphDataText = wrongBlockIdMessage;
      }
      this.showGraph = true;
    }, () => {
      this.noGraphDataText = wrongBlockIdMessage;
      this.reachabilityGraph = null;
      this.isNetworkGraphVisible.emit(false);
      this.showGraph= true;
    });
  }

  private fetchNetworkGraphDetails(networkUid: string, networkFilter: ReachabilityNetworkGraphFilterRequest): void {
    this.showGraph = false;
    this.noGraphDataText = '';
    this.functionalBlockControlerService.getReachabilityNetworkGraph(this.projectId, networkUid, networkFilter).subscribe((graphData) => {
      if (graphData && graphData?.modules?.length) {
        this.reachabilityGraph = graphData;
        this.isNetworkGraphVisible.emit(true);
      } else {
        this.noGraphDataText = this.translateService.instant('noDataFound');
        this.reachabilityGraph = null;
        this.isNetworkGraphVisible.emit(false);
      }
      this.showGraph = true;
    }, () => {
      this.noGraphDataText = this.translateService.instant('noDataFound');
      this.reachabilityGraph = null;
      this.isNetworkGraphVisible.emit(false);
      this.showGraph = true;
    });
  }
}
