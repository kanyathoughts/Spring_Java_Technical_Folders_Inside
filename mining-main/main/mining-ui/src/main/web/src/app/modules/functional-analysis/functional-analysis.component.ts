import { Component, OnInit, TemplateRef, ViewChild } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ListDetail } from '../module-details/call-chain-export/call-chain-export.interface';
import { ActivatedRoute, Router } from '@angular/router';
import { TaxonomyFilterComponent } from '@app/shared/components/taxonomy-filter/taxonomy-filter.component';
import { TranslateService } from '@ngx-translate/core';
import { NzModalService } from 'ng-zorro-antd/modal';
import { CreateFunctionalGroupComponent } from './create-functional-group/create-functional-group.component';
import { FunctionalAnalysisTreeComponent } from './functional-analysis-tree/functional-analysis-tree.component';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { DataDictionaryControllerService, DataDictionaryPojo, JobControllerService, ProjectRole } from '@innowake/mining-api-angular-client';
import { HttpErrorResponse } from '@angular/common/http';
import { Logger } from '@app/core';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
export const EXPORT_TOKEN = 'export-options-';
const log = new Logger('ModuleReporting');
import { NzMessageService } from 'ng-zorro-antd/message';
import { ModulesGQL } from '@app/graphql/generated/generated';
import { ModuleSearchFilter } from '../reachability-ui-product-vision/utils/reachability-interface';
import { DDSearchFilter } from './functional-analysis.interface';
import { Subject, Subscription } from 'rxjs';
import { debounceTime, distinctUntilChanged } from 'rxjs/operators';
import { sortNaturally } from '@app/core/utils/sort.util';
import { ConfirmDeleteModalComponent, DELETE_MODAL_CONFIRMED } from '@app/shared/components/confirm-delete-modal/confirm-delete-modal.component';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';


const LISTING_DROPDOWN_CLASS = 'module-listing__select-dropdown';
const DROPDOWN_ELEMENT_SELECTOR = '[id^="cdk-overlay-"]';

@Component({
    selector: 'app-functional-analysis',
    templateUrl: 'functional-analysis.component.html'
})
export class FunctionalAnalysisComponent implements OnInit {

    @ViewChild('taxonomyFilter') taxonomyFilterComponent: TaxonomyFilterComponent;
    @ViewChild('deleteModal') deleteModalContent: TemplateRef<any>;
    @ViewChild('functionalAnalysisTree') functionalAnalysisTreeCmp: FunctionalAnalysisTreeComponent;
    projectId: number;
    moduleId: number;
    listSearchModule: ModuleSearchFilter[] = [];
    listReachabilityBlock: ListDetail[] = [];
    selectedModuleIds: string[] = [];
    selectReachabilityIds: string[] = [];
    selectedTaxonomyIds: number[] = [];
    showFilterPanel = false;
    isGroupButtonDisable = true;
    toggleFilter = false;
    getFGDetails: Array<{ uid: string, type: string, parent: NzTreeNodeOptions}> = [];
    filterDetailsFromTree: { [key: string]: any };
    canEditFB = false;
    rootBlocksUIDArray: string[] = [];
    isFiltersActive = false;
    listOfFunctionalBlocks: string[] = [];
    selectedFunctionalBlock: string[] = [];
    selectedViewMode = 'details';
    disableGroupButton = true;
    groupOperationType = 'FUNCTIONAL_GROUP';
    disableGraphButton = false;
    disableGraphBtnToolTip = '';
    blockNameSearch: string[] = [];
    disableRemoveButton = true;
    disableGenerateButton = true;
    updateAnnotations = false;
    generateBlockDescription = false;
    selectedDDIds: string[] = [];
    listSearchDD: DDSearchFilter[] = [];
    ddSearchText = new Subject<string>();
    ddStringSubscription: Subscription;
    ddSearchSubscription: Subscription;
    functionalBlockIdArray: number[] = [];
    functionalBlockConfirmVisible = false;
    explainFunctionalBlocksOverwrite = false;
    gptTranslateActive = false;

    constructor(
        private graphQlControllerService: GraphQlControllerService,
        private clientProjectRelationship: ClientProjectRelationshipService,
        private modulesGQL: ModulesGQL,
        private router: Router,
        private route: ActivatedRoute,
        private jobController: JobControllerService,
        private jobManagerService: JobManagerService,
        private translateService: TranslateService,
        private modalService: NzModalService,
        private authorizationService: KeycloakAuthorizationService,
        private messageService: NzMessageService,
        private dataDictionaryControllerService: DataDictionaryControllerService,
        protected featureToggleService: FeatureToggleService
    ) { }

    ngOnInit(): void {
        this.featureToggleService.isActive('generativeAiTranslations').subscribe((isActive: boolean) => {
          this.gptTranslateActive = isActive;
        });
        this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
            this.projectId = response?.getProjectId();
            this.canEditFB = this.authorizationService.hasUserRole(response, ProjectRole.UserRoleEnum.EDITOR);
            this.getReachabilityBlock();
        });
        if (this.route.snapshot.queryParams['filterApplied']) {
            const filterApplied = JSON.parse(this.route.snapshot.queryParams['filterApplied'] as string);
            this.selectReachabilityIds = filterApplied?.reachabilityIds && (Array.isArray(filterApplied.reachabilityIds) ?
            filterApplied.reachabilityIds :  [filterApplied?.reachabilityIds]) || [];
            this.blockNameSearch = filterApplied?.blockNameSearch || [];
            this.selectedTaxonomyIds = filterApplied?.taxonomyIds || [];
            this.selectedModuleIds = filterApplied?.moduleIds || [];
            this.selectedDDIds = filterApplied?.ddIds || [];
            this.findModuleNames();
            const filterLength = Object.keys(filterApplied as object).length;
            this.isFiltersActive =  !! filterLength && ! (filterLength === 1 && filterApplied.hasOwnProperty('blockNameSearch'));
        }

        this.ddStringSubscription = this.ddSearchText.pipe(
            debounceTime(300),
            distinctUntilChanged()).subscribe((searchTxt: string) => {
                this.ddSearchSubscription?.unsubscribe();
                return this.onDDSearch(searchTxt);
            });
    }

    /**
     * method to get the taxonomy ids
     * @param event: output event
     */
    updateTaxonomySelection(event: TaxonomyFilterSelected[]): void {
        this.selectedTaxonomyIds = [];
        event?.forEach((taxonomy: TaxonomyFilterSelected) => {
            const taxonomyIds = `${taxonomy.taxonomyId}`.split(',').map(Number);
            this.selectedTaxonomyIds.push(...taxonomyIds);
        });
    }

    /**
     * method for module search on type and getting all the matched records
     * @param search: typed character
     */
    onModuleNameSearch(search: string): void {
        const dropdownElement = document.querySelectorAll(DROPDOWN_ELEMENT_SELECTOR);
        const dropdownClassList = dropdownElement[dropdownElement.length - 1]?.classList;
        dropdownClassList?.add(LISTING_DROPDOWN_CLASS);
        this.listSearchModule = [];
        if (search.trim().length >= 1) {
            dropdownClassList?.remove(LISTING_DROPDOWN_CLASS);
            const filterObject = {
                content_name: { eq: search }
            };
            this.modulesGQL.fetch({ projectId: this.projectId, size: 10, page: 0, filterObject }).subscribe((listModule) => {
                listModule.data.modules.content.forEach((module) => {
                    if (this.listSearchModule.findIndex(moduleOpt => moduleOpt.label === module.name) === -1) {
                        this.listSearchModule.push({ value: module.id, label: module.name, path: module.path, description: module.description });
                    }
                });
            });
        }
    }

    /**
     * method to set all the filterIds in the URL
     */
    onApplyFilter(): void {
        this.functionalAnalysisTreeCmp.clearSelectedBlocks();
        void this.router.navigate([], { replaceUrl: true });
        const currentParams = this.route.snapshot.paramMap;
        const filterApplied = {};
        // If any moduleName filter is applied, then replace the name with id.
        const filterIndex = this.selectedModuleIds.findIndex(ids => this.filterDetailsFromTree.filterDetails?.moduleName?.includes(ids));
        if (filterIndex !== -1) {
            filterApplied['moduleIds'] = this.filterDetailsFromTree.filterDetails.moduleIds;
        } else if (this.selectedModuleIds.length) {
            if (this.selectedModuleIds.some(id => typeof id === 'string') && this.filterDetailsFromTree.filterDetails?.moduleIds) {
                const nameIndex = this.selectedModuleIds.findIndex(id => typeof id === 'string');
                this.selectedModuleIds[nameIndex] = this.filterDetailsFromTree.filterDetails.moduleIds;
            }
            filterApplied['moduleIds'] = this.selectedModuleIds;
        }
        if (this.selectedTaxonomyIds.length) {
            filterApplied['taxonomyIds'] = this.selectedTaxonomyIds;
        }
        if (this.selectReachabilityIds.length && this.selectReachabilityIds[0] != null) {
            filterApplied['reachabilityIds'] = this.selectReachabilityIds;
        }
        if (this.blockNameSearch.length) {
            filterApplied['blockNameSearch'] = this.blockNameSearch;
        }

        if (this.selectedDDIds.length) {
            filterApplied['ddIds'] = this.selectedDDIds;
        }

        if (Object.keys(filterApplied).length) {
            currentParams['filterApplied'] = JSON.stringify(filterApplied);
            void this.router.navigate([], { queryParams: currentParams, queryParamsHandling: 'merge' });
        }
        this.isFiltersActive =  !! Object.keys(filterApplied).length && !(Object.keys(filterApplied).length === 1
            && filterApplied.hasOwnProperty('blockNameSearch'));
        this.toggleFilter = false;
    }

    /**
     * method to reset all the filter
     */
    onReset(): void {
        this.selectedModuleIds = [];
        this.selectedTaxonomyIds = [];
        this.selectReachabilityIds = [];
        this.selectedDDIds = [];
        this.selectedFunctionalBlock = [];
        this.taxonomyFilterComponent.selectedValue = [];
        const params = this.route.snapshot.queryParams;
        const updatedParams = {
            sortApplied: params.sortApplied || JSON.stringify({content_name: 'ASC'})
        };
        void this.router.navigate([], { queryParams: updatedParams, replaceUrl: true });
        this.isFiltersActive = false;
    }

    /**
     * method to show filter again
     */
    showFilter(): void {
        this.toggleFilter = !this.toggleFilter;
    }

    /**
     * method to check if buttons shoudl be disabled or not
     * @returns state of the buttons
     */
    isButtonDisabled(): boolean {
        return !(!!this.selectedModuleIds.length || !!this.selectReachabilityIds.length || !!this.selectedTaxonomyIds.length || !!this.selectedDDIds.length);
    }

    /**
     * Removes annotations and updates the annotations flag.
     */
    removeAnnotations(): void {
        this.updateAnnotations = false;
        const deleteModal = this.modalService.create<ConfirmDeleteModalComponent>({
          nzTitle: this.translateService.instant('functionalAnalysis.removeAnnotationsModalTitle'),
          nzClosable: true,
          nzMaskClosable: false,
          nzKeyboard: true,
          nzAutofocus: null,
          nzContent: ConfirmDeleteModalComponent,
        });
        const instance = deleteModal.getContentComponent();
        instance.modalContent = this.deleteModalContent;
        instance.confirmationText = 'deleteProjectModal.confirmText';
        instance.confirmationButtonText = 'btnLabel.remove';
        instance.isConfirmationReq = false;
        deleteModal.afterClose.subscribe((result) => {
          if (result === DELETE_MODAL_CONFIRMED) {
           this.updateAnnotations = true;
          } else {
            this.updateAnnotations = false;
          }
        });
      }

    /**
     * Prepares CSV data by setting the provided array of UIDs as the rootBlocksUIDArray property.
     * @function
     * @name prepareCSVData
     * @param uidArray - An array of UIDs to be set as CSV data.
     * @returns void
     */
    prepareCSVData(uidArray: string[]): void {
        this.rootBlocksUIDArray = uidArray;
    }

    /**
     * Exports functional block analysis tree view by submitting a job to the server.
     * @function
     * @name export
     * @throws Throws an HTTP error response if the job submission fails.
     * @returns void
     */
    exportFunctionalAnalysis(): void {
        this.jobController.submitJobExtensionV2(this.projectId, 'functional-block-analysis-csv', {
            'moduleIds': this.selectedModuleIds.length > 0 ? this.selectedModuleIds : [],
            'taxonomyIds': this.selectedTaxonomyIds.length > 0 ? this.selectedTaxonomyIds : [],
            'peerUids': this.selectReachabilityIds.length > 0 ? this.selectReachabilityIds.map(item => JSON.parse(item)) : [],
            'ddeNames': this.selectedDDIds.length > 0 ? this.selectedDDIds.map((ddIdItem: string) => `${ddIdItem.trim()}`) : [],
            '$query': ['annotations'],
            '$columns': []
        }, undefined)
        .subscribe((response: string[]) => {
            this.getJobStatus(response.toString());
        }, (error: HttpErrorResponse) => {
            log.error(error.message);
        });
    }

    /**
     * get the filter details and page index from child component
     * @param event: filter and current page index
     */
    onFilterDetails(event: { [key: string]: any }): void {
        this.filterDetailsFromTree = event;
    }

    /**
     * method to open the modal
     */
    OnGroup(): void {
        const modal = this.modalService.create<CreateFunctionalGroupComponent>({
            nzTitle: this.translateService.instant('functionalAnalysis.createFunctionalModalTitle'),
            nzClosable: true,
            nzMaskClosable: false,
            nzWrapClassName: 'vertical-center-modal',
            nzKeyboard: true,
            nzContent: CreateFunctionalGroupComponent,
          });
        const instance = modal.getContentComponent();
        instance.functionalGroupDetails = this.getFGDetails;
        instance.projectId = this.projectId;
        modal.afterClose.subscribe((result) => {
            if (result.OPERATION === 'SUCCESS') {
                this.functionalAnalysisTreeCmp.clearSelectedBlocks();
                this.messageService.success(this.translateService.instant('functionalAnalysis.functionalBlockCreated') as string);
                const { filterDetails, pageNo } = this.filterDetailsFromTree;
                this.functionalAnalysisTreeCmp.generateFunctionalKeys();
                this.functionalAnalysisTreeCmp.getTreeDetailsBasedOnFilter(filterDetails as { [key: string]: any }, pageNo as number, result.uid as string);
                this.functionalAnalysisTreeCmp.selectedBlock.uid = result.uid;
                this.functionalAnalysisTreeCmp.selectedBlock.title = result.title;
                this.functionalAnalysisTreeCmp.selectedBlock.isLeaf = false;
                this.functionalAnalysisTreeCmp.selectedBlock.selectedType = result.type;
                this.functionalAnalysisTreeCmp.selectedBlock.currentNode = { origin: {uid: result.uid, type: result.type, title: result.title} };
                this.functionalAnalysisTreeCmp.deletedUID = result.uid;
                this.functionalAnalysisTreeCmp.getGroupLevelDetails(result.uid as string, this.projectId);
                this.isGroupButtonDisable = true;
            }
        });
    }

    /**
     * method to decide the state of group FB button
     * @param btnStateDecider: state which will be set for the button
     */
    onGetGroupFBDetails(fbDetails: { keys: Array<{ uid: string, type: string, parent: NzTreeNodeOptions}>, btnState: boolean }): void {
        const { keys, btnState } = fbDetails;
        this.isGroupButtonDisable = btnState;
        this.getFGDetails = keys;
        this.disableGenerateButton = this.getFunctionalBlockIds().length === 0;
    }

    /**
     * Method that triggers on the search of data dictionary, it fetches the options as per user input
     * @param search searched text
     */
    onDDSearch(search: string): void {
        const dropdownElement = document.querySelectorAll(DROPDOWN_ELEMENT_SELECTOR);
        const dropdownClassList = dropdownElement[dropdownElement.length - 1]?.classList;
        dropdownClassList?.add(LISTING_DROPDOWN_CLASS);
        this.listSearchDD = [];
        if (search) {
            dropdownClassList?.remove(LISTING_DROPDOWN_CLASS);
            this.ddSearchSubscription = this.dataDictionaryControllerService.searchDataDictionary(this.projectId, '', search)
                .subscribe((searchResult: DataDictionaryPojo[]) => {
                    searchResult.forEach((dd) => {
                        if (this.listSearchDD.findIndex(ddOpt => ddOpt.label === dd.dataElementName) === -1) {
                            this.listSearchDD.push({ value: `"${dd.uid}"`, label: dd.dataElementName, description: dd.description });
                        }
                    });
                });
        }
    }

    /**
     * Method to switch the group button to details tab.
     * @param event boolean value to switch to details tab
     */
    switchToDetails(event: boolean): void {
        if (event) {
            this.selectedViewMode = 'details';
            this.disableGroupButton = false;
        }
    }

    /**
     * Method to enable/disable the graph button.
     * @param event boolean value to enable/disable the button
     */
    disableGraphBtn(event: { buttonStatus: boolean, msg: string }): void {
        this.disableGraphButton = event.buttonStatus;
        this.disableGraphBtnToolTip = event.msg;
    }

    /**
     * Checks if any filter is applied.
     * @returns True if any filter is applied, false otherwise.
     */
    isFilterApplied(): boolean {
        return (
          this.selectedModuleIds.length > 0 ||
          this.selectedTaxonomyIds.length > 0 ||
          this.selectReachabilityIds.length > 0 ||
          this.blockNameSearch.length > 0 ||
          this.selectedDDIds.length > 0
        );
      }

    /**
     * Handles the removal of labels from the annotations data.
     * @param annotationsData - The array of FunctionalBlockPojoPrototype objects representing the annotations data.
     */
    handleRemoveLabel(annotationsData: Map<string, NzTreeNodeOptions>): void {
        this.disableRemoveButton = annotationsData.size < 1 || this.getFGDetails.length !== annotationsData.size;
    }

    handleGenerateLabel(annotationsData: Map<string, NzTreeNodeOptions>): void {
        if (annotationsData.size > 0) {
            this.disableGenerateButton = false;
        } else  {
            this.disableGenerateButton = true;
        }
    }
    /**
     * Updates the status of the remove button.
     */
    updateRemoveButtonStatus(): void {
       this.disableRemoveButton = true;
    }

    /**
     * Method to search functional blocks on type
     * @param search: typed character
     */
    onFunctionalBlockSearch(search: string): void {
        this.addRemoveDropdownclassList('add');
        this.listOfFunctionalBlocks = [];
        const contentTypeFilter = '{content_type: {eq: FUNCTIONAL_GROUP}}';
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
            this.addRemoveDropdownclassList('remove');
            const functionalBlocks = sortNaturally(response.data.functionalBlocks.content as Array<{[key: string]: any}>, 'name')
                .map((block: any) => block.name);
            this.listOfFunctionalBlocks = functionalBlocks.filter((x: string) => x.toLowerCase().includes(search.toLowerCase()));
          }
        });
      }

    getFunctionalBlockIds(): string [] {
      return this.getFGDetails
        .filter((detail) =>  detail.type === 'FUNCTIONAL_BLOCK')
        .map((detail) => detail.uid);
    }

    onGenerateClick(): void {
      this.functionalBlockConfirmVisible = true;
    }

    handleFunctionalBlockModalOk(): void {
     this.functionalBlockConfirmVisible = true;
     const functionalBlockIds: string [] = this.getFunctionalBlockIds();
     this.jobController.submitJobExtensionV2(this.projectId, 'generate-functional-block-descriptions',
            {
                functionalBlockIds,
                'overwrite': this.explainFunctionalBlocksOverwrite,
                '$query': ['annotations'],
                '$columns': [
                ]
            })
        .subscribe((response: string[]) => {
            this.getJobStatus(response.toString());
        }, (error: HttpErrorResponse) => {
            log.error(error.message);
        });
        this.functionalBlockConfirmVisible = false;
    }

    handleFunctionalBlockModalCancel(): void {
      this.functionalBlockConfirmVisible = false;
    }

    private getReachabilityBlock(): void {
        const requestQuery = {
            query: `{
                functionalBlocks(projectId: ${this.projectId}, filterObject: {content_type: {eq: RA_TOP_DOWN }}) {
                    content {
                        uid
                        name
                    }
                }
            }`
        };
        this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
            if (response && response['data']['functionalBlocks']['content'].length) {
                this.listReachabilityBlock = response['data']['functionalBlocks']['content']
                    .map((functionalBlockItem: { [key: string]: any }) =>
                        ({ label: functionalBlockItem.name, value: functionalBlockItem.uid}));
                this.listReachabilityBlock = this.listReachabilityBlock.sort((a, b) => a.label.localeCompare(b.label));
            }
        });
    }

    private getJobStatus(jobId: string): void {
        const remoteJob = {
            jobId: jobId as unknown as string,
            autoDownloadResult: true,
            foreground: true,
            cancellable: true
        };
        this.jobManagerService.register(remoteJob);
    }

    private filterItems(items: any): any {
        return items.reduce((acc: any, item: any) => {
            if (item.checked) {
                acc.push(item.uid);
            }
            if (item.children && Array.isArray(item.children)) {
                acc = acc.concat(this.filterItems(item.children));
            }
            return acc;
        }, []);
    };

    private addRemoveDropdownclassList(action: string): void {
        const dropdownElement = document.querySelectorAll(DROPDOWN_ELEMENT_SELECTOR);
        const dropdownClassList = dropdownElement[dropdownElement.length - 1]?.classList;
        if (action === 'add') {
          dropdownClassList?.add(LISTING_DROPDOWN_CLASS);
        } else {
          dropdownClassList?.remove(LISTING_DROPDOWN_CLASS);
        }
      }

      private findModuleNames(): void {
        if(this.selectedModuleIds.length > 0){
            const filterObject = {
                content_id: { in: this.selectedModuleIds }
            };
            this.modulesGQL.fetch({ projectId: this.projectId, size: 10, page: 0, filterObject }).subscribe((listModule) => {
                    listModule.data.modules.content.forEach((module) => {
                        if (this.listSearchModule.findIndex(moduleOpt => moduleOpt.label === module.name) === -1) {
                            this.listSearchModule.push({ value: module.id, label: module.name, path: module.path, description: module.description });
                        }
                    });
                });
        }
    }
}
