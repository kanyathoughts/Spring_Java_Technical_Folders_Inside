import { AfterViewInit, Component, EventEmitter, Input, OnChanges, OnInit, Output } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { GraphUtility } from '../../dependency/utils/dependency-graph-utility';
import { TranslateService } from '@ngx-translate/core';
import { YFileGraphInfo, FilterParameters } from '@app/modules/graph/models/yfile-graph-info.model';
import { DependencyGraph, ModulePojo } from '@innowake/mining-api-angular-client';
import { ReachabilityGraphFilter } from '@app/modules/reachability-ui-product-vision/utils/reachability-interface';

@Component({
  selector: 'reachability-explorer',
  templateUrl: './reachability-explorer.component.html'
})
export class ReachabilityExplorerComponent implements OnInit, AfterViewInit, OnChanges {
  @Input() breadcrumbItems: string[] = [];
  @Input() networkUid = '';
  @Input() reachabilityGraph: DependencyGraph;
  @Input() layout = 'HIERARCHIC_LAYOUT';
  @Input() projectId: number;
  @Input() depth: number;
  @Input() showExports = true;
  @Output() filterRBGraph= new EventEmitter<any>();
  moduleId: number;
  sliderVal = 1;
  dependencyLinks: DependencyGraph;
  loadState: LoaderState = LoaderState.loading;
  graphInfo: YFileGraphInfo;
    filterSelectedOptions: FilterParameters;
  errorMsg: string;

  constructor(
    private activatedRoute: ActivatedRoute,
    translateService: TranslateService
  ) {
    this.errorMsg = translateService.instant('unableToRenderGraph');
  }

  ngOnInit(): void {
    this.activatedRoute.data.subscribe((data: { module: ModulePojo }) => {
      if (data?.module) {
        this.moduleId = data.module.id;
        this.projectId = data.module.projectId;
        const graphDepth = localStorage.getItem(`${this.moduleId}-depth`);
        if (graphDepth) {
          this.sliderVal = parseInt(graphDepth, 10);
          // @yjs:keep
          localStorage.removeItem(`${this.moduleId}-depth`);
        }
      }
    });
  }

  ngAfterViewInit(): void {
    this.loadGraphData();
  }

  ngOnChanges(): void {
    if (this.dependencyLinks) {
      this.loadState = LoaderState.loading;
      this.loadGraphData();
    }
  }

  /**
   * Method to capture the filter params from the reachability graph component
   * @param filterParams - filter params from the reachability graph component
   */
  reachabilityFilterChange(filterParams: ReachabilityGraphFilter): void {
    this.filterRBGraph.emit(filterParams);
  }

  private loadGraphData() {
    if (this.reachabilityGraph) {
      this.dependencyLinks = this.reachabilityGraph;
      this.graphInfo = GraphUtility.getYFilesGraphInfo(this.dependencyLinks);
    }
    this.loadState = LoaderState.success;
  }
}
