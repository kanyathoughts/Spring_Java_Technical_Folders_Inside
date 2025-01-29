import { AfterViewChecked, ChangeDetectorRef, Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';
import { ModuleBadgeUpdateService } from '@app/core/services/module-details/module-badge-update.service';
import { BadgeCountUpdateOperation } from '@app/modules/module-details/models/module-complexity-details';
import { ScrollEventService } from '@app/core/services/scroll-event/scroll-event.service';
import { EclipseService } from '@app/core/authentication/eclipse.service';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { CustomTableExtensionDescription, MiningUiExtensionsControllerService, ModuleControllerService,
  WebUiExtensionDescription } from '@innowake/mining-api-angular-client';

@Component({
  templateUrl: './project-shell.component.html'
})
export class ProjectShellComponent implements OnInit, AfterViewChecked, OnDestroy {
  isCollapsed: boolean;
  currentClient: ClientProjectRelationship = null;
  eclipseView: boolean;
  routeDashboard: string;
  routeModules: string;
  routeFunctionalBlocks: string;
  routeFunctionalBlockUiProductVision: string;
  routeAnnotations: string;
  functionalAnalysis: string;
  routeExport: string;
  routeSummary: string;
  routeTechnologies: string;
  routeInterfaces: string;
  routeSqlDecomposition: string;
  routeAppDecomposition: string;
  routeRuleCandidates: string;
  routeCodeQuality: string;
  routeUtilities: string;
  routeDnaAnalysis: string;
  routeCallChain: string;
  routeConfiguration: string;
  routeDataDictionary: string;
  routeIMS: string;
  routeReachability: string;
  routeSemanticSearch: string;
  customTables: CustomTableExtensionDescription[] = [];
  externalLinks: WebUiExtensionDescription[] = [];
  subMenuEnable: string;
  modulesWarningToolTipText = '';
  isShowLastScanBadge = false;
  compositionEnabled: boolean;
  semanticSearchEnabled: boolean;
  showHide: string;
  private scrollEventSubscription: Subscription;
  private clientProjectSubscription: Subscription;
  private deleteAnnotation: Subscription;

  constructor(
    public clientProjectRelationshipService: ClientProjectRelationshipService,
    public miningUiExtensionService: MiningUiExtensionsControllerService,
    private eclipseService: EclipseService,
    private route: ActivatedRoute,
    private moduleControllerService: ModuleControllerService,
    private translateService: TranslateService,
    private moduleBadgeUpdateService: ModuleBadgeUpdateService,
    private router: Router,
    private cdr: ChangeDetectorRef,
    private featureToggleService: FeatureToggleService,
    private scrollEventService: ScrollEventService) { }

  ngOnInit(): void {
    this.subMenuEnable = this.route.snapshot.data.subMenu;
    this.isCollapsed = localStorage.getItem('isSidebarCollapsed') === 'true';
    this.eclipseView = ! this.eclipseService.isEclipseView;
    this.featureToggleService.isActive('functionalBlockUiProductVision').subscribe((isActive: boolean) => this.compositionEnabled = isActive);
    this.featureToggleService.isActive('semanticSearch').subscribe((isActive: boolean) => this.semanticSearchEnabled = isActive);
    this.populateCustomTables();
    this.populateExternalLinks();
    this.clientProjectSubscription = this.clientProjectRelationshipService.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      if (response && response.getClientId() != null && response.getProjectId() != null) {
        this.currentClient = response;
        this.routeDashboard = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'dashboard');
        this.routeModules = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'modules');
        this.routeAnnotations = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'annotations');
        this.routeExport = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'export');
        this.routeSummary = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'metrics/summary');
        this.routeTechnologies = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'metrics/technologies');
        this.routeSqlDecomposition = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'metrics/sql');
        this.routeAppDecomposition = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'metrics/decomposition');
        this.routeInterfaces = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'metrics/interfaces');
        this.routeUtilities = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'metrics/utilities');
        this.routeRuleCandidates = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'metrics/candidates');
        this.routeCodeQuality = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'metrics/code-quality');
        this.routeCallChain = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'reachability/call-chain');
        this.routeDnaAnalysis = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'dna');
        this.functionalAnalysis = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'functional-analysis');
        this.routeConfiguration = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'configuration');
        this.routeDataDictionary = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'data-dictionary');
        this.routeIMS = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'metrics/ims');
        this.routeFunctionalBlockUiProductVision = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'composition/tiles');
        this.routeReachability = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'reachability/overview');
        this.routeSemanticSearch = RouteBuilder.buildProjectRoute(this.currentClient.getProjectId(), 'semantic-search');
      }
    });
    this.deleteAnnotation = this.moduleBadgeUpdateService.getModuleToBeReviewed().subscribe(response => {
      if (response === BadgeCountUpdateOperation.DELETE_ALL_WARNINGS) {
        this.isShowLastScanBadge = false;
      }
      if (response === BadgeCountUpdateOperation.ANNOTATION_DELETED) {
        this.addNavigationBadge();
      }
    });
    this.addNavigationBadge();
    this.scrollEventSubscription = this.scrollEventService.getScrollObservable().subscribe((scrollPosition) => {
      this.showHide = 'project-shell__nav-sider__content-' + scrollPosition;
    });
  }

  ngAfterViewChecked(): void {
    this.cdr.detectChanges();
  }

  /**
   * changes the sidebar State and sets the localStorage.
   */
  changeSidebarState(): void {
    this.isCollapsed = ! this.isCollapsed;
    localStorage.setItem('isSidebarCollapsed', this.isCollapsed.toString());
  }

  /**
   * changes the Sub Menu state when we redirect to the matrices or to some other modules.
   * @returns boolean value for the submenu that need to be open or closed.
   */
  handleSubMenuOpen(subMenu: string): boolean {
    return (this.subMenuEnable && this.subMenuEnable === subMenu) ? true : false;
  }

  /**
   * checks if the current route is related to parent module or not.
   * @returns boolean value for navigation item to be selected or not.
   */
  isCurrentRouteModuleRelated(): boolean {
    return this.router.url.includes(`project-${this.currentClient.getProjectId()}/module`);
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
    this.deleteAnnotation?.unsubscribe();
    this.scrollEventSubscription?.unsubscribe();
  }

  private populateCustomTables() {
    this.miningUiExtensionService.getTableExtensions().subscribe((response: CustomTableExtensionDescription[]) => {
      this.customTables = response;
      this.customTables.forEach((item: CustomTableExtensionDescription) => {
        item.identifier = RouteBuilder.buildCustomTableRoute(this.currentClient.getProjectId(), item.identifier);
      });
    });
  }

  private populateExternalLinks() {
    this.miningUiExtensionService.getWebUiExtensions().subscribe((response: WebUiExtensionDescription[]) => {
      this.externalLinks = response;
      this.externalLinks.forEach((item: WebUiExtensionDescription) => {
        item.pageIdentifier = RouteBuilder.buildExternalRoute(this.currentClient.getProjectId(), item.pageIdentifier);
      });
    });
  }

  private addNavigationBadge(): void {
    this.moduleControllerService.countRequiresReview(this.currentClient.getProjectId()).subscribe((count: number) => {
      if (count > 0) {
        this.isShowLastScanBadge = true;
        let modules;
        let wasWere;
        if (count === 1) {
          modules = this.translateService.instant('projectShell.module');
          wasWere = this.translateService.instant('projectShell.was');
        } else {
          modules = this.translateService.instant('projectShell.modules');
          wasWere = this.translateService.instant('projectShell.were');
        }
        const numberOfModules = '' + count;
        this.modulesWarningToolTipText = this.translateService.instant('projectShell.navigationBadgeToolTipTxt', { numberOfModules, modules, wasWere });
      } else {
        this.isShowLastScanBadge = false;
      }
    });
  }
}
