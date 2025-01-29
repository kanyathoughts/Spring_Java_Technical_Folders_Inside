import { Component, OnInit, ViewChild } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { BlockViewFilter, MergeBlockData, ModuleSearchFilter, ReachabilityBlocks, ReachabilityFilter } from '../utils/reachability-interface';
import { TranslateService } from '@ngx-translate/core';
import { NzModalService } from 'ng-zorro-antd/modal';
import { NzMessageService } from 'ng-zorro-antd/message';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import {
  FunctionalBlockControllerService,
  ProjectRole,
  ReachabilityAnalysisRequest,
  ReachabilityNetworkGraphFilterRequest,
  TechnologyType,
} from '@innowake/mining-api-angular-client';
import { ReachabilityService } from '../utils/reachability.service';
import { Router } from '@angular/router';
import { JobControllerService } from '@innowake/mining-api-angular-client';
import { HttpErrorResponse } from '@angular/common/http';
import { Logger } from '@app/core/services/logger.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { last } from 'rxjs/internal/operators/last';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { sortNaturally } from '@app/core/utils/sort.util';
import { TaxonomyFilterComponent } from '@app/shared/components/taxonomy-filter/taxonomy-filter.component';
import { ModulesGQL } from '@app/graphql/generated/generated';
import { InitiateReachabilityComponent } from '../initiate-reachability/initiate-reachability.component';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { Subject, Subscription, debounceTime, distinctUntilChanged } from 'rxjs';

const log = new Logger('BlockView');
const defaultDependencyFilterValue = 'resource';

@Component({
  selector: 'block-view',
  templateUrl: './block-view.component.html'
})
export class BlockViewComponent implements OnInit {

  @ViewChild('taxonomyFilter') taxonomyFilterComponent: TaxonomyFilterComponent;
  projectId: number;
  pageSize = 50;
  pageIndex = 0;
  numberOfReachabilityBlocksInProject = 0;
  totalElements: number;
  clientProjectRelationship: ClientProjectRelationship;
  reachabilityBlocks: ReachabilityBlocks[] = [];
  showBlocks = false;
  commonParent: string;
  blockDetails: ReachabilityBlocks;
  canEditRB = false;
  showInactiveBlocks = false;
  allBlocksChecked = false;
  genAiTranslateActive = false;
  generateConfirmVisible = false;
  overwriteExistingDescriptions = false;
  generateModuleDescriptions = false;
  toggleFilter: boolean;
  listSearchModule: ModuleSearchFilter[] = [];
  selectedModuleIds: string[] = [];
  blockNameSearch: string;
  selectedTaxonomyIds: number[] = [];
  listOfFunctionalBlocks: string[] = [];
  selectedFunctionalBlock: string[] = [];
  isFiltersActive = false;
  isOutdatedModules = false;
  allReachabilityIds: string[] = [];
  switchStateChange = false;
  canUpdateOrDeleteRB = false;
  isOutdatedRbUpdated = false;
  isReachabilityInfoVisible = false;
  switchBlockGraph = 'block';
  enableCreateReachability = false;
  filterAppliedNoData = false;
  reachabilityAnalysisInitialized = false;
  selectedLwrBoundTypeTechnology: string[] = [];
  listOfTypeTech: Array<{label: string; value: string}> = [];
  selectTypeTechnology: string[] = [];
  referenceType: Array<{ label: string; value: string }> = [{ label: this.translateService.instant('reachability.read'), value: 'READ' },
  { label: this.translateService.instant('reachability.write'), value: 'WRITE' }];
  selectedReferenceType: string[] = [];
  dependencyFilterValue = defaultDependencyFilterValue;
  blockViewFilter: BlockViewFilter;
  networkViewFilter: ReachabilityNetworkGraphFilterRequest;
  moduleSearchText = new Subject<string>();
  moduleStringSubscription: Subscription;
  moduleSearchSubscription: Subscription;
  isGraphUpdated = true;
  isNetworkGraphVisible: boolean;

  constructor(
    private clientProjectRelationshipService: ClientProjectRelationshipService,
    private graphQlControllerService: GraphQlControllerService,
    private modalService: NzModalService,
    private functionalBlockControllerService: FunctionalBlockControllerService,
    private messageService: NzMessageService,
    private translateService: TranslateService,
    private authorizationService: KeycloakAuthorizationService,
    private reachabilityService: ReachabilityService,
    private router: Router,
    private featureToggleService: FeatureToggleService,
    private jobManagerService: JobManagerService,
    private jobController: JobControllerService,
    private moduleService: ModulesGQL,
    private labelMappingService: LabelMappingService
    ) { }

  ngOnInit(): void {
    this.featureToggleService.isActive('generativeAiTranslations').subscribe((isActive) => {
      this.genAiTranslateActive = isActive;
    });
    this.clientProjectRelationshipService.getClientProjectObservable()
    .subscribe((response: ClientProjectRelationship) => {
      if (response) {
        this.clientProjectRelationship = response;
        this.projectId = this.clientProjectRelationship.getProjectId();
        this.setModuleTypeOptions();
        this.canEditRB = this.authorizationService.hasUserRole(this.clientProjectRelationship, ProjectRole.UserRoleEnum.EDITOR);
        this.canUpdateOrDeleteRB = this.authorizationService.hasUserRole(this.clientProjectRelationship, ProjectRole.UserRoleEnum.EDITOR);
        this.enableCreateReachability = this.authorizationService.hasUserRole(this.clientProjectRelationship, ProjectRole.UserRoleEnum.MANAGER) ||
          this.authorizationService.hasUserRole(this.clientProjectRelationship, ProjectRole.UserRoleEnum.ADMIN);
        localStorage.setItem(`${this.projectId}-updateRB`, `${this.canUpdateOrDeleteRB}`);
      }
    });
    this.getReachabiityBlockCount(0, 1);
    this.reachabilityService.getUpdateGraph().subscribe((state: boolean) => {
      if (state) {
        this.fetchReachabilityBlocks();
      }
    });
    Object.keys(localStorage).forEach((key) => {
      if (key.includes('reachabilityIds') || key.includes('reachabilityDetails')) {
        localStorage.removeItem(key);
      }
    });
    this.setFiltersFromUrl();
    if (this.reachabilityService.getUpdateOutdatedState()) {
      this.onActionClicked('update');
    } else {
      this.fetchReachabilityBlocks();
    }

    if ( ! this.commonParent) {
      this.getCommonParent();
    }
    this.moduleStringSubscription = this.moduleSearchText.pipe(
      debounceTime(300),
      distinctUntilChanged()).subscribe((searchTxt: string) => {
          this.moduleSearchSubscription?.unsubscribe();
          return this.onModuleNameSearch(searchTxt);
      });
  }

  /**
   * Method to update the outdated RB or remove deleted RB based on user action.
   * @param event user action to update or delete RB.
   */
  onActionClicked(event: string): void {
    this.showBlocks = false;
    this.isGraphUpdated = false;
    if (event === 'delete') {
      this.functionalBlockControllerService.removeFunctionalBlocksWithoutUpperBoundModule(this.projectId).subscribe((removedResult: string[]) => {
        if (removedResult) {
          this.getJobStatus(removedResult.toString(), true);
        }
      });
    }
    if (event === 'update') {
      this.functionalBlockControllerService.recalculateOutDatedFunctionalBlocks(this.projectId).subscribe((outdatedResult: string[]) => {
        if (outdatedResult) {
          this.getJobStatus(outdatedResult.toString(), true);
        }
      });
    }
  }

  /**
   * Method to switch block status between active and inactive
   * @param status current status of the block
   */
  updateBlockStatus(status: boolean): void {
    if (status) {
      this.fetchReachabilityBlocks();
    }
  }

  showReachabilityInfo(): void {
    this.isReachabilityInfoVisible = true;
  }

  /**
   * To make the filter active based by default in network view.
   * @param event user selected view.
   */
  checkFilterValue(event: string): void {
    this.switchBlockGraph = event;
    this.makeFilterActive(this.blockViewFilter.reachabilityFilter);
  }

  /**
   * Method to set the state of switch
   * @param value current state of the switch
   */
  onShowInactiveBlocksChange(value: boolean): void {
    this.switchStateChange = true;
    this.reachabilityService.setSwitchState(value);
  }

  /**
   * method to show filter again
   */
  showFilter(): void {
    this.toggleFilter = ! this.toggleFilter;
  }

  /**
   * Update RB when all blocks are selected.
   */
  updateAllBlocksChecked(): void {
    if (this.allBlocksChecked) {
      this.getAllBlockIds();
    } else {
      this.allReachabilityIds = [];
      this.reachabilityBlocks = this.reachabilityBlocks.map(block => ({...block, isSelected: false}));
    }
  }

  /**
   * Method to enable merge based on commonParent.
   * @param selectedBlock flag to check if block is checked/unchecked.
   */
  onBlockSelectionChanged(block: ReachabilityBlocks): void {
    if (block) {
      this.allReachabilityIds = block.isSelected ? [...this.allReachabilityIds, block.uid] : this.allReachabilityIds.filter(id => id !== block.uid);
    }
    this.allBlocksChecked = this.allReachabilityIds.length === this.totalElements;
  }

  /**
   * Opens table view for multiselection of blocks.
   */
  openMultiSelectionTableView(): void {
    this.storeSelectedBlockIds(this.allReachabilityIds);
    if (this.allReachabilityIds.length === 1) {
      void this.router.navigate(['/project-' + this.projectId + '/reachability/' + this.allReachabilityIds[0] + '/table']);
    } else {
      void this.router.navigate(['/project-' + this.projectId + '/reachability/table']);
    }
  }

  /**
   * This method is used to bulk inactivate or activate functional blocks.
   * @param status - The status to be set for the blocks.
   */
  bulkUpdateFunctionalBlocksStatus(status: 'ACTIVE' | 'INACTIVE'): void {
    this.modalService.warning({
      nzTitle: this.translateService.instant('reachability.bulkInactiveDescription'),
      nzOkText: this.translateService.instant('btnLabel.inactivate'),
      nzIconType: 'warning',
      nzOnOk: () => this.reachabilityService.updateBlockStatus(
        this.projectId,
        status,
        this.allReachabilityIds,
        this.translateService.instant('reachability.bulkInactiveSuccess', { count: this.allReachabilityIds.length }) as string,
        () => {
          this.fetchReachabilityBlocks();
        }),
      nzCancelText: this.translateService.instant('btnLabel.cancel')
    });
  }

  /**
   * Method to execute reachability analysis.
   */
  createReachability(): void {
    const modal = this.modalService.create<InitiateReachabilityComponent>({
      nzTitle: this.translateService.instant('reachability.inititateModalTitle'),
      nzMaskClosable: false,
      nzBodyStyle: { height: '480px' },
      nzWidth: '90vw',
      nzClassName: 'block-view__info-title',
      nzContent: InitiateReachabilityComponent
    });
    const modalComponent = modal.getContentComponent();
    modalComponent.projectId = this.projectId;
    modal.afterClose.subscribe((data: ReachabilityAnalysisRequest) => {
      if (data) {
        this.reachabilityAnalysisInitialized = true;
        this.functionalBlockControllerService.executeReachabilityAnalysis(this.projectId, data).subscribe((result: string[]) => {
          if (result) {
            this.showBlocks = false;
            this.getJobStatus(result.toString());
          }
        }, () => {
          const errorContent: string = this.translateService.instant('reachability.errorExecutingFB');
          this.messageService.error(errorContent);
          this.allReachabilityIds = [];
          this.allBlocksChecked = false;
          this.getReachabilityBlocks();
        });
      }
    });
  }

  /**
   * Opens a Model to merge the selected blocks.
   */
  openModalToMerge(): void {
    this.reachabilityService.openModalToMergeReachabilityBlocks().afterClose.subscribe((data: MergeBlockData) => {
      this.reachabilityService.mergeReachabilityBlocks(data, this.projectId, this.commonParent, this.allReachabilityIds);
      this.reachabilityService.getMergeStatus().subscribe((status: string) => {
        this.showBlocks = false;
        this.reachabilityBlocks = [];
        if (status === this.translateService.instant('messageService.success')) {
          this.pageIndex = 0;
        }
        this.allReachabilityIds = [];
        this.allBlocksChecked = false;
        this.fetchReachabilityBlocks();
      });
    });
  }

  /**
   * Makes the confirmation modal visible.
   */
  showConfirmModal(): void {
    this.generateConfirmVisible = true;
  }

  /**
   * Submits a generate-reachability-block-descriptions job and makes the confirmation modal invisible.
   */
  handleConfirmModalOk(): void {
    const uids = this.allReachabilityIds;
    this.jobController.submitJobExtensionV2(this.projectId, 'generate-reachability-block-descriptions',
      { uids, 'overwrite': this.overwriteExistingDescriptions, 'generateModuleDescriptions': this.generateModuleDescriptions })
      .subscribe((response: string[]) => {
        this.getJobStatus(response.toString());
      }, (error: HttpErrorResponse) => {
        log.error(error.message);
      });
    this.closeConfirmModal();
  }

  /**
   * Closes the modal and resets the checkboxes.
   */
  closeConfirmModal(): void {
    this.generateConfirmVisible = false;
    this.overwriteExistingDescriptions = false;
    this.generateModuleDescriptions = false;
  }

  /**
   * Closes the reachability info modal.
   */
  closeInfoModal(): void {
    this.isReachabilityInfoVisible = false;
  }

  /**
   * Gets the user selected taxonomy details.
   * @param event taxonomy filter selected.
   */
  getTaxonomySelection(event: TaxonomyFilterSelected[]): void {
    this.selectedTaxonomyIds = [];
    event?.forEach((taxonomy: TaxonomyFilterSelected) => {
      const taxonomyIds = `${taxonomy.taxonomyId}`.split(',').map(Number);
      this.selectedTaxonomyIds.push(...taxonomyIds);
    });
  }

  /**
   * Method for the Pagination
   * @param current is the current page number.
   */
  paginate(current: number): void {
    this.pageIndex = current ? current - 1 : this.pageIndex;
    this.fetchReachabilityBlocks(true);
  }

  /**
   * Get all when drawer is closed.
   */
  getAllBlocks(): void {
    this.fetchReachabilityBlocks();
  }

  /**
   * Checks the visibility of the network graph based on the provided event.
   * @param event A boolean value indicating whether the network graph is visible or not.
   */
  networkGraphData(event: boolean): void {
    this.isNetworkGraphVisible = event;
  }

  /**
   * method for module search on type and getting all the matched records
   * @param search: typed character
   */
  onModuleNameSearch(search: string): void {
    this.addRemoveDropdownclassList('add');
    this.listSearchModule = [];
    if (search.trim().length >= 1) {
      this.addRemoveDropdownclassList('remove');
      const filterObject = {
        content_name: { eq: search }
      };
      this.moduleService.fetch({ projectId: this.projectId, size: 10, page: 0, filterObject }).subscribe((listModule) => {
        listModule.data.modules.content.forEach((module) => {
          if (this.listSearchModule.findIndex(moduleOpt => moduleOpt.label === module.name) === -1) {
            this.listSearchModule.push({ value: module.id, label: module.name, path: module.path, description: module.description });
          }
        });
      });
    }
  }

  /**
   * Method to search functional blocks on type
   * @param search: tyoed character
   */
  onFunctionalBlockSearch(search: string): void {
    this.listOfFunctionalBlocks = [];
    let contentTypeFilter = '{content_type: {eq: FUNCTIONAL_GROUP}';
    contentTypeFilter += 'content_parents:{notEq: {content_type: {eq: FUNCTIONAL_GROUP}}}}';
    const requestQuery = {
      'query': `{
        functionalBlocks(projectId: ${this.projectId},
          filterObject: ${contentTypeFilter}) {
          content {
            uid
            name
            type
            children {
              content {
                name
                uid
                type
              }
            }
          }
        }
      }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      if (response && response?.data.functionalBlocks.content?.length) {
        const functionalBlocks = sortNaturally(response.data.functionalBlocks.content as Array<{[key: string]: any}>, 'name').map((block: any) => block.name);
        this.listOfFunctionalBlocks =  functionalBlocks.filter((x: string) => x.includes(search));
      }
    });
  }

  /**
   * Method to get block based on search input.
   */
  onReachabilityNameSearch(): void {
    this.onApplyFilter(true);
  }

  /**
   * Method to filter reachability blocks based on applied filters.
   */
  onApplyFilter(nameSearch: boolean = false): void {
    const filterDetails: ReachabilityFilter = this.constructFilterDetails(false);
    this.pageIndex = 0;
    this.switchStateChange = false;
    this.getReachabilityBlocks(this.blockViewFilter, true);
    if ( ! nameSearch) {
      this.toggleFilter =  ! this.toggleFilter;
    }

    // Redirect to the same page after adding filter to the URL
    const filters = filterDetails;
    filters['inActiveSwitch'] = this.showInactiveBlocks;
    filters['dependency'] = this.dependencyFilterValue;
    filters['nameSearch'] = this.blockNameSearch;

    this.makeFilterActive(filterDetails);

    void this.router.navigate([], {
      queryParams: {
        filter: JSON.stringify(filters),
        page: this.pageIndex + 1
      },
      queryParamsHandling: 'merge',
      replaceUrl: true
    });
  }

  /**
   * To reset the filter values
   */
  onResetFilter(): void {
    this.selectedModuleIds = [];
    this.selectedTaxonomyIds = [];
    this.selectedFunctionalBlock = [];
    this.selectTypeTechnology = [];
    this.selectedReferenceType = [];
    this.isFiltersActive = false;
    this.taxonomyFilterComponent.selectedValue = [];
    this.dependencyFilterValue = defaultDependencyFilterValue;
    this.showInactiveBlocks = false;
    this.reachabilityService.setSwitchState(false);
    this.onApplyFilter();
    void this.router.navigate([], { replaceUrl: true });
  }

  /**
   * method to check if buttons shoudl be disabled or not
   * @returns state of the buttons
   */
  isButtonDisabled(): boolean {
    const isCommonFilterApplied: boolean = !!this.selectedModuleIds.length || !!this.selectedTaxonomyIds.length ||
      !!this.selectedFunctionalBlock.length || !!this.showInactiveBlocks || !!this.selectTypeTechnology.length;
    if (this.switchBlockGraph === 'graph') {
      return !(isCommonFilterApplied || this.dependencyFilterValue !== defaultDependencyFilterValue);
    } else {
      return !(isCommonFilterApplied  || !!this.selectedReferenceType.length);
    }
  }

  /**
   * method to recalculate reachability analysis on selected blocks
   * @param uid the uid of the selected block
   */
  recalculateReachabilityAnalysisOnSelectedBlocks(uid?: string): void {
    const reachabilityAnalysisRequest: ReachabilityAnalysisRequest = {
      analysisType: ReachabilityAnalysisRequest.AnalysisTypeEnum.TOP_DOWN,
      moduleTaxonomies: new Set(),
      functionalBlockUids: new Set(uid ? [uid] : this.allReachabilityIds),
      recalculateAll: true
    };
    this.functionalBlockControllerService.executeReachabilityAnalysis(this.projectId, reachabilityAnalysisRequest).subscribe((result: string[]) => {
      if (result) {
        this.getJobStatus(result.toString(), true);
        this.allReachabilityIds = [];
        this.allBlocksChecked = false;
      }
    }, () => {
      const errorContent: string = this.translateService.instant('reachability.errorExecutingFB');
      this.messageService.error(errorContent);
      this.allReachabilityIds = [];
      this.allBlocksChecked = false;
      this.getReachabilityBlocks();
    });
  }

  /**
   * triggers Rechability Analysis for the Block Card
   * @param event selected block uid
   */
  recalculateFromBlockCard(event: string): void {
    this.recalculateReachabilityAnalysisOnSelectedBlocks(event);
  }

  private getReachabiityBlockCount(pageSize: number, pageIndex: number): void {
    this.reachabilityService.getReachabilityBlocksCount(this.projectId, pageSize, pageIndex, '', { inActiveSwitch: true }).subscribe((count: number) => {
      this.numberOfReachabilityBlocksInProject = count;
      this.filterAppliedNoData = this.checkFilterData();
    });
  }

  private addRemoveDropdownclassList(action: string): void {
    const dropdownElement = document.querySelectorAll('[id^="cdk-overlay-"]');
    const dropdownClassList = dropdownElement[dropdownElement.length - 1]?.classList;
    if (action === 'add') {
      dropdownClassList?.add('module-listing__select-dropdown');
    } else {
      dropdownClassList?.remove('module-listing__select-dropdown');
    }
  }

  private getCommonParent(): void {
    this.reachabilityService.checkToEnableMerge(this.projectId).subscribe((uid: string) => {
      this.commonParent = uid;
      this.constructFilterDetails();
    });
  }

  private getAllBlockIds(): void {
    this.reachabilityService.getReachabilityBlocksId(this.projectId, this.blockViewFilter, this.blockNameSearch).subscribe((blockIds: string[]) => {
      if (blockIds?.length) {
        this.allReachabilityIds = blockIds;
        if (this.allBlocksChecked) {
          this.reachabilityBlocks = this.reachabilityBlocks.map(block => ({...block, isSelected: true}));
        }
      }
    });
  }

  private storeSelectedBlockIds(allBlockIds: string[]): void {
    this.storeReachabilityDetails();
    localStorage.setItem(`${this.projectId}-reachabilityIds`, JSON.stringify(allBlockIds));
  }

  private storeReachabilityDetails(): void {
    const selectedBlocks = this.getSelectedBlocks();
    let blockDetails = {};
    if (selectedBlocks.length === 1) {
      blockDetails = {
        pageTitle: selectedBlocks[0].name,
        blockId: selectedBlocks[0].uid,
        mergeParent: selectedBlocks[0].type?.includes('MERGE_PARENT') ? true : false,
        outdated: selectedBlocks[0].outdatedModule,
        deleted: selectedBlocks[0].deletedModule
      };
    } else {
      blockDetails = {
        outdated: selectedBlocks.some(x => x.outdatedModule),
        deleted: selectedBlocks.some(x => x.type?.includes('MERGE_PARENT'))
      };
    }
    localStorage.setItem(`${this.projectId}-reachabilityDetails`, JSON.stringify(blockDetails));
  }

  private makeFilterActive(filterDetails: ReachabilityFilter): void {
    const filterActive = this.checkFilterApplied(filterDetails);
    if ( ! filterActive) {
      this.isFiltersActive = this.blockViewFilter.inActiveSwitch;
      if (this.switchBlockGraph === 'graph' && ! this.isFiltersActive) {
        this.isFiltersActive = this.dependencyFilterValue !== defaultDependencyFilterValue;
      }
    } else {
      this.isFiltersActive = filterActive;
    }
  }

  private checkFilterApplied(filterApplied: ReachabilityFilter): boolean {
    for (const key in filterApplied) {
      if ((this.switchBlockGraph === 'block' || key !== 'referenceType')
        && (key !== 'excludeParentTypes' && Array.isArray(filterApplied[key]) && filterApplied[key].length > 0) ||
          (key === 'excludeParentTypes' && (filterApplied[key].length === 0 || filterApplied[key].length > 1
            || (filterApplied[key].length === 1 && filterApplied[key][0] !== 'MERGE_PARENT')))) {
        return true;
      }
    }
    return false;
  }

  private fetchReachabilityBlocks(paginate: boolean = false): void {
    this.showInactiveBlocks = this.reachabilityService.getSwitchState();
    this.constructFilterDetails(true);
    this.getReachabilityBlocks(this.blockViewFilter, paginate);
  }

  private constructFilterDetails(filterActive: boolean = false): ReachabilityFilter {
    const type: string[] = [];
    const technology: string[] = [];
    const technologyTypeList: TechnologyType[] = [];
    this.selectTypeTechnology.forEach((typeTechnologyItem: string) => {
      const typeTechnology = typeTechnologyItem.split(',');
      type.push(typeTechnology[1]);
      technology.push(typeTechnology[0]);
      technologyTypeList.push({ type: typeTechnology[1] as TechnologyType.TypeEnum, technology: typeTechnology[0] as TechnologyType.TechnologyEnum });
    });

    const filterDetails =
    {
      taxonomies: this.selectedTaxonomyIds,
      functionalBlocks: this.selectedFunctionalBlock,
      modulesIds: this.selectedModuleIds,
      type,
      technology,
      referenceType: this.selectedReferenceType,
      excludeParentTypes: ['MERGE_PARENT']
    };
    this.blockViewFilter = {
      reachabilityFilter: filterDetails,
      inActiveSwitch: this.showInactiveBlocks
    };
    const networkFilterObject = this.reachabilityService['buildReachabilityFilter']({
		  reachabilityFilter: filterDetails,
		  inActiveSwitch: this.showInactiveBlocks
    });
    this.networkViewFilter = { filterObject: networkFilterObject };
    if (this.dependencyFilterValue !== 'both') {
      this.networkViewFilter.functionalBlockLinkType = this.dependencyFilterValue === 'control' ? 'RA_FROM_SCHEDULER_INFO' : 'RA_SHARED_RESOURCE';
    }
    if (technologyTypeList.length) {
      this.networkViewFilter.technologyTypes = new Set(technologyTypeList);
    }
    if (filterActive) {
      this.makeFilterActive(filterDetails);
    }
    return filterDetails;
  }

  private maintainBlockSelectionOnPaginateAndFilter(): void {
    this.allReachabilityIds?.forEach((Id: string) => {
      const findSelectedBlock = this.reachabilityBlocks.find((block: ReachabilityBlocks) => block.uid === Id);
      if (findSelectedBlock) {
        findSelectedBlock.isSelected = true;
      }
    });
  }

  private getSelectedBlocks(): ReachabilityBlocks[] {
    return this.reachabilityBlocks.filter(b => b.isSelected);
  }

  private getReachabilityBlocks(reachabilityFilter?: BlockViewFilter, paginate?: boolean): void {
    this.reachabilityBlocks = [];
    if ( ! paginate ) {
    this.allReachabilityIds = [];
    }
    this.showBlocks = false;
    this.reachabilityService.getReachabilityBlocks(this.projectId, this.pageSize, this.pageIndex, this.blockNameSearch, reachabilityFilter)
    .subscribe((blocks: ReachabilityBlocks[]) => {
      this.reachabilityBlocks = blocks;
      this.totalElements = this.reachabilityService.totalElements;
      this.showBlocks = true;
      this.isOutdatedRbUpdated = false;
      this.getReachabiityBlockCount(0, 1);
      if (this.allReachabilityIds.length && (this.allReachabilityIds.length !== this.totalElements) && ! paginate) {
        this.getAllBlockIds();
      }
      if (paginate) {
        this.maintainBlockSelectionOnPaginateAndFilter();
      }
    }, () => {
      this.reachabilityBlocks = [];
    });
    this.closeConfirmModal();
  }

  private checkFilterData(): boolean {
    let showSearch = false;
    if ((this.isFiltersActive || this.blockNameSearch) && ! this.reachabilityBlocks.length) {
      showSearch = true;
      /** This is initial condition where no filters are applied */
    } else if (! this.isFiltersActive && ! this.blockNameSearch && this.numberOfReachabilityBlocksInProject > 0) {
      showSearch = true;
    } else if ((this.isFiltersActive || this.blockNameSearch) && this.numberOfReachabilityBlocksInProject > 0) {
      showSearch = true;
    } else {
      showSearch = false;
    }
    return showSearch;
  }

  private getJobStatus(jobId: string, update: boolean = false): void {
    const remoteJob = {
      jobId: jobId as unknown as string,
      autoDownloadResult: false,
      foreground: true,
      cancellable: true
    };
    this.jobManagerService.register(remoteJob).status$.pipe(last()).subscribe(() => {
      if (update) {
        this.reachabilityService.setNotifyAlertBannerForOutdatedOrRemovedRb(true);
        this.isGraphUpdated = true;
        this.fetchReachabilityBlocks();
      } else {
        this.allReachabilityIds = [];
        this.allBlocksChecked = false;
        if (this.reachabilityAnalysisInitialized) {
          this.getCommonParent();
          this.setModuleTypeOptions();
          this.showBlocks = true;
        }
        this.getReachabilityBlocks();
      }
      this.reachabilityAnalysisInitialized = false;
    });
  }

  private setModuleTypeOptions(): void {
    const requestQuery = {
      'query': `{
      functionalBlocks(projectId: ${this.projectId}, filterObject: {content_type: {eq: RA_LOWER_BOUND }}) {
        aggregations {
          groupBy {
            REFERENCED_MODULE_TYPE
            REFERENCED_MODULE_TECHNOLOGY
          }
        }
      }
    }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      if (response && response.data && response.data.functionalBlocks) {
        response.data.functionalBlocks.aggregations.forEach((aggregationItem: any) => {
          if (aggregationItem && aggregationItem.groupBy) {
            const combinedValue = `${aggregationItem.groupBy.REFERENCED_MODULE_TECHNOLOGY},${aggregationItem.groupBy.REFERENCED_MODULE_TYPE}`;
            const mappedTechnology: string = this.labelMappingService.mapLabel(LabelType.TECHNOLOGY,
              aggregationItem.groupBy.REFERENCED_MODULE_TECHNOLOGY as string);
            const mappedType: string = this.labelMappingService.mapLabel(LabelType.TYPE,
              aggregationItem.groupBy.REFERENCED_MODULE_TYPE as string);
            const combinedLabel = `${mappedTechnology} ${mappedType}`;
            if ( ! this.listOfTypeTech.some(item => item.value === combinedValue)) {
              this.listOfTypeTech.push({ label: combinedLabel, value: combinedValue });
            }
          }
        });
      }
    });
  }

  private setFiltersFromUrl(): void {
    const queryParams = this.router.parseUrl(this.router.url).queryParams;
    if (queryParams.filter) {
      const filterDetails = JSON.parse(queryParams.filter as string);
      this.selectedModuleIds = filterDetails.modulesIds || [];
      this.selectedFunctionalBlock = filterDetails.functionalBlocks || [];
      this.selectedReferenceType = filterDetails.referenceType || [];
      this.dependencyFilterValue = filterDetails.dependency || '';
      this.blockNameSearch = filterDetails.nameSearch || '';
      this.selectedTaxonomyIds = filterDetails.taxonomies || [];

      if (filterDetails?.type) {
        this.selectTypeTechnology = filterDetails.type.map((type: string, index: number) => `${filterDetails.technology[index]},${type}`);
      } else  {
        this.selectTypeTechnology = [];
      }

      if (filterDetails?.inActiveSwitch !== '') {
        this.showInactiveBlocks = filterDetails.inActiveSwitch;
        this.reachabilityService.setSwitchState(this.showInactiveBlocks);
      }
    }
  }
}
