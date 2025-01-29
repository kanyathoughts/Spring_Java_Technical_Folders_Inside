import { AfterContentChecked, ChangeDetectorRef, Component, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { NzTableSortOrder } from 'ng-zorro-antd/table';
import { Observable, Subject, Subscription } from 'rxjs';
import { concatMap, map, tap } from 'rxjs/operators';
import { OptionItem } from '../mining-table/mining-table-config.interface';
import { MiningTableOptionSelected } from '../mining-table/mining-table-option-selected.interface';
import { SavedSearchMode } from './saved-search/saved-search.interface';
import { SavedSearchComponent } from './saved-search/saved-search.component';
import { SharedAnnotationEditorService } from '../shared-annotation-editor/shared-annotation-editor.service';
import {
  DataPointControllerService,
  MiningDataPointDefinitionWithPath,
  SavedSearchPojo,
  SavedSearchControllerService
} from '@innowake/mining-api-angular-client';

const savedSearchButtonTitle = 'savedSearchModal.buttonTitle';
const reachabilityIntermediateColumn = 'ReachabilityData.intermediateModuleName';
const reachabilityPathTaxonomiesColumn = 'ReachabilityData.path_taxonomies';


@Component({
  selector: 'app-base-reporting-page',
  templateUrl: './base-reporting-page.component.html'
})
export class BaseReportingPageComponent implements AfterContentChecked {

  @ViewChild(SavedSearchComponent) saveSearch: SavedSearchComponent;
  pageTitle: string;
  pageType: TypeName;
  projectId: number;
  savedSearchMode = SavedSearchMode;
  tableConfig: any = {};
  rowActions: OptionItem[][];
  internalDataPoints: MiningDataPointDefinitionWithPath[] = [];
  totalRecords: number;
  value: any[] = [];
  usage: Usages;
  graphQlType: string;
  dataChangeEvent: Subject<any> = new Subject<any>();
  clientProjectSubscription: Subscription;
  showWarning: boolean;
  annotationConfirmVisible: boolean;
  moduleConfirmVisible: boolean;
  explainAnnotationsOverwrite: boolean;
  generateModuleDescriptionsOverwrite: boolean;
  alertText: string;
  alertPopconfirmTitle: string;
  alertButtonText: string;
  alertOkText: string;
  alertCancelText: string;
  errorMessage: string;
  selectedSavedSearchName: string;
  initialTableConfig: { [key: string]: any };
  afterSelectionTableConfig: { [key: string]: any };
  defaultTableConfig: { [key: string]: any };
  createNewSavedSearch = false;
  createSavedSearchTitle = this.translateService.instant(savedSearchButtonTitle);
  savedSearchList: SavedSearchPojo[] = [];
  setOfselectedId: Set<number>;
  confirmModal: NzModalRef;
  refreshCoreTable = false;
  disableSortHandling = false;
  loadTable = false;
  allowSelectDeselectRows: boolean;
  moduleName: string;
  mergeRowsByProperty: string;
  breadcrumbItems: string[];
  additionalGraphQLParams: { [key: string]: any } = {};
  switchColumns: { id: string, column: string };
  columnForRowSelection: string;
  /** commenting the below code as select all creating issue if data is like 930k records
   * will be resolve in coming up tickets
   */
  // allIds: number[]=[];
  reloadSubscription: Subscription;
  preFilterDetails: string[];
  initialized = false;
  switchPathDetails = false;
  showReachabilityAlert = false;
  private eventsSubscriptions: Subscription;
  private subscription: Subscription;

  constructor(
    public messageService: NzMessageService,
    public relationshipService: ClientProjectRelationshipService,
    public userCustomizableTable: CustomizableTableColumnService,
    public graphQlControllerService: GraphQlControllerService,
    public route: ActivatedRoute,
    public dataPointControllerService: DataPointControllerService,
    public savedSearchControllerService: SavedSearchControllerService,
    public translateService: TranslateService,
    public parametersService: CustomizableTableParametersService,
    public router: Router,
    public modalService: NzModalService,
    public notification: NzNotificationService,
    public cd: ChangeDetectorRef,
    public sharedAnnotationEditorService: SharedAnnotationEditorService,
  ) {
    this.userCustomizableTable.resetTableColumnAndDataPoints();
    this.parametersService.resetCurrentTableParameters();
  }

  ngAfterContentChecked(): void {
    this.cd.detectChanges();
  }

  handleAlertOnConfirm?(): void;

  /**
   * Method to update the outdated RB from table view.
   */
  updateOutdatedRb?(): void;

  handleSelectedOption?(rowData: MiningTableOptionSelected): void;

  handleAnnotationsModalOk?(): void;

  handleModulesModalOk?(): void;

  /**
   * Initialize the Customisable table page by fetching the needed Data (Datapoints, Saved Searches, ...)
   * @param typeName type to use to fetch the Datapoints and for GraphQl queries.
   * @returns the client project obeservable so the parent component can subscribe to it.
   */
  initialize(): Observable<ClientProjectRelationship> {
    this.getSavedSearch();
    const isPreFilterFromQuery: string = this.route.snapshot.queryParams['preFilter'] || '';
    const idsFromLocalStorage = localStorage.getItem(isPreFilterFromQuery);
    if (idsFromLocalStorage) {
      const graphIdsInfo = JSON.parse(idsFromLocalStorage);
      this.savedSearchList.length = 0;
      if (isPreFilterFromQuery?.includes('call-chain')) {
        this.preFilterDetails = graphIdsInfo?.moduleIds;
        this.pageTitle = `${this.translateService.instant('module.callChain')} ${graphIdsInfo?.createdTime}`;
      } else if (isPreFilterFromQuery?.includes('reachabilityModules')) {
        this.preFilterDetails = graphIdsInfo?.moduleIds;
        this.pageTitle = graphIdsInfo?.blockName;
      } else if (isPreFilterFromQuery?.includes('linkedDDIds')) {
        this.preFilterDetails = graphIdsInfo?.DDIds;
      } else if (isPreFilterFromQuery?.includes('MissingErrorModules')) {
        this.preFilterDetails = graphIdsInfo?.moduleIds;
      } else {
        this.preFilterDetails = graphIdsInfo?.annotationIds;
      }
    }
    return this.relationshipService.getClientProjectObservable().pipe(
      tap(currentClient => {
        this.projectId = currentClient.getProjectId();
      }),
      concatMap((currentClient) => this.savedSearchControllerService.findByUsage(this.projectId, this.usage).pipe(
        map((savedSearches) => {
          this.savedSearchList = savedSearches;
          this.savedSearchList.sort((a, b) => (a.name.localeCompare(b.name.toLowerCase())));
          if (this.selectedSavedSearchName) {
            const selectedSavedSearch = this.savedSearchList.find(savedSearch => savedSearch.name === this.selectedSavedSearchName);
            if (selectedSavedSearch) {
              this.saveSearch.setSavedSearchParameter(selectedSavedSearch);
            } else {
              this.setUrlParameters();
              this.selectedSavedSearchName = '';
            };
          } else {
            this.setUrlParameters();
          }
          this.initialized = true;
          return currentClient;
        })
      ))
    );
  }

  navigate(name: string): string {
    if (name === this.translateService.instant('reachability.reachabilityBlocks')) {
      return '/project-' + this.projectId + '/reachability/overview';
    }
  }

  handleAnnotationsModalCancel(): void {
    this.annotationConfirmVisible = false;
  }

  handleModulesModalCancel(): void {
    this.moduleConfirmVisible = false;
  }

  loadCustomizableTable(): void {
    this.loadTable = true;
  }

  /**
   * Subscribe to the Datapoint selection and fetch tabl data on update
   */
  fetchTableDataAsPerDataPoints(): void {
    if (! this.defaultTableConfig) {
      this.getDefaultTableConfig(this.userCustomizableTable.dataPointsList);
    }
    if (this.userCustomizableTable.selectedColumns?.length && this.selectedSavedSearchName === '') {
      const queryParams = this.parametersService.getTableParametersForUrl();
      queryParams.columns = this.userCustomizableTable.selectedColumns;
      this.initialTableConfig = this.saveSearch.getTableConfig(queryParams.columns as string[], queryParams.filter as string, queryParams.sort as string);
      if (JSON.stringify(this.initialTableConfig) !== JSON.stringify(this.defaultTableConfig)) {
        this.createNewSavedSearch = false;
        this.createSavedSearchTitle = this.createNewSavedSearch ? this.translateService.instant(savedSearchButtonTitle) : 'Saved';
        this.initialTableConfig = this.defaultTableConfig;
      }
    }
  }

  /**
   * method toggle the intermediate column and accessing module.
   * operation: hide/show intermediate column and accessing module
   */
  togglePathDetails(operation: string): void {
    let currentlySelectedDataPoints = this.userCustomizableTable.retainedDataPointId;
    this.refreshCoreTable = false;
    if (operation === 'showDetails') {
      this.switchPathDetails = true;
      const reachabilityIntermediateColumnDataPoint = this.userCustomizableTable.dataPointsList.find((item) => item.id === reachabilityIntermediateColumn);
      const reachabilityPathTaxonomiesColumnDataPoint = this.userCustomizableTable.dataPointsList.find((item) => item.id === reachabilityPathTaxonomiesColumn);
      if (reachabilityIntermediateColumnDataPoint && ! currentlySelectedDataPoints.includes(reachabilityIntermediateColumnDataPoint)) {
        currentlySelectedDataPoints.push(reachabilityIntermediateColumnDataPoint);
      }
      if (reachabilityPathTaxonomiesColumnDataPoint && ! currentlySelectedDataPoints.includes(reachabilityPathTaxonomiesColumnDataPoint)) {
        currentlySelectedDataPoints.push(reachabilityPathTaxonomiesColumnDataPoint);
      }
      this.messageService.info(this.translateService.instant('reachability.notificationTitleAfterSplit') as string, { nzDuration: 5000 });
    } else {
      this.switchPathDetails = false;
      currentlySelectedDataPoints = currentlySelectedDataPoints.filter(
        (item) => item.id !== reachabilityIntermediateColumn && item.id !== reachabilityPathTaxonomiesColumn);
    }
    this.additionalGraphQLParams = {
      ...this.additionalGraphQLParams, includeIntermediateModulesPerAccessModule: ! this.additionalGraphQLParams.includeIntermediateModulesPerAccessModule
    };
    this.userCustomizableTable.updateSelectedDataPoints(currentlySelectedDataPoints);
  }

  /**
   * Unsubscribe to all subscribtion, to be called in the ngOnDestroy method
   */
  destroy(): void {
    this.clientProjectSubscription.unsubscribe();
    this.eventsSubscriptions?.unsubscribe();
    this.subscription?.unsubscribe();
    this.reloadSubscription?.unsubscribe();
  }

  /**
   * assigning the route, selectedSavedSearchName, initialTableConfig which is changed
   * from the saved search
   * @param event of type object contains saved search name, initialTableConfig and route
   */
  getSavedSearchDetails(event: { savedSearchName: string; initialTableConfig: { [key: string]: any; }; route: ActivatedRoute; }): void {
    this.initialTableConfig = event.initialTableConfig;
    this.selectedSavedSearchName = event.savedSearchName;
    this.disableSortHandling = true;
    this.route = event.route;
    this.createNewSavedSearch = false;
  }

  /**
   * Perform Actions based on the event emitted from core table.
   * @param action the action to be performed.
   */
  performAction(action: string): void {
    if (action === 'setRoute') {
      this.saveSearch.setRoute();
    } else if (action === 'fetchTable') {
      this.fetchTableDataAsPerDataPoints();
    } else if (action === 'makeServerSideCall') {
      this.serverSidePaginationCall();
      this.disableSortHandling = false;
    }
  }

  /**
   * Fetch the data for the table
   * @param moduleIdsFromCallChain contains module ids from preFilter in Call Chain
   */
  serverSidePaginationCall(): void {
    const queryParams = this.parametersService.getTableParametersForUrl();
    queryParams.columns = this.userCustomizableTable.selectedColumns;
    this.afterSelectionTableConfig = this.saveSearch.getTableConfig(queryParams.columns as string[],
      queryParams.filter as string, queryParams?.sort as string);
    this.createNewSavedSearch = JSON.stringify(this.initialTableConfig) !== JSON.stringify(this.afterSelectionTableConfig);
    this.createSavedSearchTitle = this.createNewSavedSearch ? (this.route.snapshot.queryParams.savedSearch ?
      this.translateService.instant('savedSearchModal.saveTitle') : this.translateService.instant(savedSearchButtonTitle)) : 'Saved';
  }

  /**
   * Check if the annotation form is dirty or not.
   */
  isFormDirty(): boolean {
    return !this.sharedAnnotationEditorService.getEditorFormState();
  }

  /**
   * Closes the currently open annotation editor.
   */
  onCancel(): void {
    this.sharedAnnotationEditorService.closeDrawer();
  }

  /**
   * Initialise the table configuration with the URL parameters
   */
  setUrlParameters(): void {
    const pageIndex: number = Number.parseInt(this.route.snapshot.queryParams['page'] as string, 10);
    const filterString: string = this.route.snapshot.queryParams['filter'];
    const filter: Array<{ key: string, value: NzTableSortOrder }> = filterString ? JSON.parse(filterString) : [];
    const sort: string = this.route.snapshot.queryParams['sort'] ?? this.parametersService.currentTableParameters.sort;
    const uniquekeyFromCallChain: string = this.route.snapshot.queryParams['preFilter'] ?? '';
    this.parametersService.setParameters(filter, pageIndex, sort, uniquekeyFromCallChain);
    const selectedColumns: string[] = this.route.snapshot.queryParams['columns'];
    if (selectedColumns) {
      this.userCustomizableTable.setColumnIdsSelection(selectedColumns.toString().match(/[^,]+/g));
    } else {
      this.refreshCoreTable = true;
    }
  }

  /**
   * Show Last View State When clicking on Modules in Navigation Menu
   */
  private getSavedSearch(): void {
    const savedSearchKey = this.graphQlType.concat('SavedSearch');
    if (Object.keys(this.route.snapshot.queryParams).length === 0 && JSON.parse(sessionStorage.getItem(savedSearchKey))) {
      this.selectedSavedSearchName = JSON.parse(sessionStorage.getItem(savedSearchKey));
    } else {
      this.selectedSavedSearchName = this.route.snapshot.queryParams.savedSearch ?? '';
    }
    this.userCustomizableTable.dataPointsList = [];
  }

  private getDefaultTableConfig(dataPointsList: MiningDataPointDefinitionWithPath[]): void {
    const defaultColumns: MiningDataPointDefinitionWithPath[] = [];
    dataPointsList.forEach((element) => {
      if (element.usageAttributes?.[this.usage]?.defaultColumnIndex) {
        defaultColumns.push(element);
      }
    });
    defaultColumns.sort((a, b) => (a.usageAttributes?.[this.usage]?.defaultColumnIndex < b.usageAttributes?.[this.usage]?.defaultColumnIndex ? -1 : 1));
    this.defaultTableConfig = {
      columns: defaultColumns.map(node => node.id).sort((a: string, b: string) => a.localeCompare(b)).join(','),
      filter: '[]',
      sort: this.userCustomizableTable.getDefaultSortBy(this.usage, dataPointsList)
    };
  }
}
