import { AfterViewInit, ChangeDetectorRef, Component, Input, OnChanges, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { GraphUtility } from '../utils/dependency-graph-utility';
import { TranslateService } from '@ngx-translate/core';
import { YFileGraphInfo, FilterParameters } from '@app/modules/graph/models/yfile-graph-info.model';
import { EclipseService } from '@app/core/authentication/eclipse.service';
import { DependencyGraph, ModuleControllerService, ModulePojo } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'dependency-explorer',
  templateUrl: './dependency-explorer.component.html'
})
export class DependencyExplorerComponent implements OnInit, AfterViewInit, OnChanges {
  @Input() showDepthControl = true;
  @Input() showGraphFilters = true;
  @Input() showBreadCrumb = true;
  @Input() showExports = true;
  @Input() breadcrumbItems: string[] = [];
  /**
   * Whether the graph will be displayed on the call-chain page. If true the style will be different.
   */
  @Input() callChainGraph = false;
  @Input() networkUid = '';
  @Input() dependencyGraph: DependencyGraph;
  @Input() layout = 'HIERARCHIC_LAYOUT';
  @Input() projectId: number;
  @Input() depth: number;
  moduleId: number;
  sliderVal = 1;
  dependencyLinks: DependencyGraph;
  loadState: LoaderState = LoaderState.loading;
  eclipseView: boolean;
  graphInfo: YFileGraphInfo;
  isRedirected: boolean;
  filterSelectedOptions: FilterParameters;
  errorMsg: string;

  constructor(
    private activatedRoute: ActivatedRoute,
    private moduleControllerService: ModuleControllerService,
    private eclipseService: EclipseService,
    private cdref: ChangeDetectorRef,
    translateService: TranslateService
  ) {
    this.errorMsg = translateService.instant('unableToRenderGraph');
  }

  ngOnInit(): void {
    this.eclipseView = this.eclipseService.isEclipseView;
    if (this.callChainGraph) {
      return;
    }
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

  depthChange(level: number): void {
    this.sliderVal = level;
    this.loadState = LoaderState.loading;
    const filterQuery = this.getFilterQueryFromParams(this.filterSelectedOptions);
    this.loadGraphData(filterQuery);
  }

  changeLayout(selectedLayout: string): void {
    this.layout = selectedLayout;
  }

  changeFilter(filterParams: FilterParameters): void {
    if (filterParams && (filterParams.moduleFilter || filterParams.relationshipFilter)) {
      this.filterSelectedOptions = filterParams;
      const query = this.getFilterQueryFromParams(filterParams);
      this.loadState = LoaderState.loading;
      this.loadGraphData(query);
    }
  }

  private getFilterQueryFromParams(filterParams: FilterParameters): string {
    let query: string;
    const queryBuilder: string[] = [];
    if (filterParams) {
      if (filterParams.moduleFilter && filterParams.moduleFilter.length) {
        const moduleQuery = 'modules=in=(' + filterParams.moduleFilter.join(',') + ')';
        queryBuilder.push(moduleQuery);
      }
      if (filterParams.relationshipFilter && filterParams.relationshipFilter.length) {
        const relationshipQuery = 'relationships=in=(' + filterParams.relationshipFilter.join(',') + ')';
        queryBuilder.push(relationshipQuery);
      }
      if (queryBuilder.length > 1) {
        query = queryBuilder.join(' and ');
      } else {
        query = queryBuilder[0];
      }
    }
    return query;
  }

  private loadGraphData(filterQuery?: string) {
    if (this.dependencyGraph) {
      this.dependencyLinks = this.dependencyGraph;
      this.graphInfo = GraphUtility.getYFilesGraphInfo(this.dependencyLinks);
      this.loadState = LoaderState.success;
    } else if (this.breadcrumbItems.length) {
      this.loadState = LoaderState.success;
    } else {
      this.moduleControllerService.traverseModuleDependencies(this.projectId, this.moduleId, this.sliderVal, GraphUtility.MAX_GRAPH_NODES, filterQuery, true)
        .subscribe(response => {
          if (response.modules.length) {
            this.dependencyLinks = response;
            this.graphInfo = GraphUtility.getYFilesGraphInfo(this.dependencyLinks);
            this.loadState = LoaderState.success;
          } else {
            this.loadState = LoaderState.error;
          }
          this.cdref.detectChanges();
        }, () => {
          this.loadState = LoaderState.error;
          this.cdref.detectChanges();
        });
    }
  }
}
