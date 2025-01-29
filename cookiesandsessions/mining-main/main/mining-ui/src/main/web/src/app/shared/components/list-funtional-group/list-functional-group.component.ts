import { Component, Input, NgZone, OnInit, ViewChild } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { AnnotationItem, FunctionalGroupService } from '@app/core/services/functional-group.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import {
  AnnotationToFunctionalBlockControllerService,
  EntityId,
  FunctionalBlockControllerService,
  AnnotationPojo
} from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { Observable, Subscription, forkJoin } from 'rxjs';
import { TaxonomyFilterSelected } from '../taxonomy-filter/taxonomy-filter-selected.interface';
import { ModuleSearchFilter } from '@app/modules/reachability-ui-product-vision/utils/reachability-interface';
import { ModulesGQL } from '@app/graphql/generated/generated';
import { ListDetail } from '@app/modules/module-details/call-chain-export/call-chain-export.interface';
import { TaxonomyFilterComponent } from '../taxonomy-filter/taxonomy-filter.component';
import { NzTreeNodeKey } from 'ng-zorro-antd/core/tree';

interface DeleteFunctionalAnnotations {
  uid: string;
  description: string;
  name: string;
  children: string[];
  annotationId: number;
}
@Component({
  selector: 'list-functional-group-component',
  templateUrl: './list-functional-group.component.html'
})
export class ListFunctionalGroupComponent implements OnInit {
  @ViewChild('taxonomyFilter') taxonomyFilterComponent: TaxonomyFilterComponent;
  @Input() projectId!: number;
  @Input() moduleId!: EntityId;
  @Input() annotationIds!: number[];
  @Input() annotation: { [key: string]: any };
  @Input() annotationsOfModule: any[] = [];
  @Input() saveButtonState: boolean;
  annotationMappingWithIds: { [key: number]: any } = {};
  mappedData: { [key: string]: any } = {};
  isLoading = false;
  checked = false;
  indeterminate = false;
  setOfCheckedId = new Set<string>();
  searchValue = '';
  uids: string[] = [];
  deleteFgs: string[] = [];
  deleteAnnotationIds: number[] = [];
  totalTreeItemsCount: number;
  selectedPageIndex = 0;
  filterDetails = {};
  functionalAnalysisTree: NzTreeNodeOptions[] = [];
  initialFunctionalAnalysisTree: NzTreeNodeOptions[] = [];
  perPage = 30;
  functionalKeys: string[] = [];
  selectedModuleIds: string[] = [];
  selectReachabilityIds: string[] = [];
  selectedTaxonomyIds: TaxonomyFilterSelected[] | string[] = [];
  toggleFilter = false;
  isFiltersActive = true;
  listSearchModule: ModuleSearchFilter[] = [];
  listReachabilityBlock: ListDetail[] = [];
  nzExpandedKeys: NzTreeNodeKey[] = [];
  deleteFunctionalAnnotationsFromFb: DeleteFunctionalAnnotations[] = [];

  private parentUUIDs: string[] = [];
  private fgGraphqlSubscription: Subscription;

  constructor(
    private graphQlControllerService: GraphQlControllerService,
    private modalRef: NzModalRef,
    private functionalBlockControllerService: FunctionalBlockControllerService,
    private annotationFunctionalBlockController: AnnotationToFunctionalBlockControllerService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private translateService: TranslateService,
    private functionalGroupService: FunctionalGroupService,
    private modulesGQL: ModulesGQL,
    private ngZone: NgZone
  ) {}

  ngOnInit(): void {
    if (this.annotationsOfModule.length > 0) {
      this.getReachabilityBlock();
      this.selectedModuleIds = [this.annotation?.moduleName];
      this.filterDetails['moduleIds'] = [this.moduleId];
      this.getTreeDetailsBasedOnFilter(this.filterDetails, 0);
    }
    this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.projectId = response?.getProjectId();
    });
    this.getFunctionalBlocks();
    if (this.annotationIds && this.annotationIds.length > 0) {
      this.annotationFunctionalBlockController
        .getFunctionalUnitsForAnnotations(this.projectId, [this.annotationIds[0]])
        .subscribe((functionalUnits: { [key: string]: any }) => {
          this.annotation = { ...this.annotation, uid: Object.values(functionalUnits)[0] as string };
        });
    }
    this.functionalKeys.length = 0;
  }

  /**
   * method tp generate keys ( headers for the panel)
   */
  generateFunctionalKeys(): void {
    this.functionalKeys = [
      this.translateService.instant('functionalTree.description'),
      this.translateService.instant('functionalTree.modules'),
      this.translateService.instant('functionalTree.code'),
    ];
  }

  /**
   * method to call on the pagination change
   * @param current indicates which page is selected
   */
  onPaginationChange(current: number): void {
    const pageIndex = current - 1;
    this.selectedPageIndex = pageIndex;
    this.getTreeDetailsBasedOnFilter(this.filterDetails, pageIndex);
  }

  /**
   * method to get initial data functional block/group
   * @param filterDetails: initial filters
   * @param pageNo: current page no
   */
  getTreeDetailsBasedOnFilter(filterDetails: { [key: string]: any }, pageNo: number): void {
    this.functionalGroupService
      .getFunctionalGroupTree(this.projectId, filterDetails, pageNo, this.perPage)
      .subscribe((tree) => {
        this.totalTreeItemsCount = this.functionalGroupService.totalTreeElements;
        this.initialFunctionalAnalysisTree = tree;
        this.functionalAnalysisTree.length = 0;
        this.functionalAnalysisTree = tree;
        this.getFunctionalBlocks();
      });
  }

  /**
   * Updates the set of checked IDs based on the provided UID and check status.
   * @param uid - The UID to be added or removed from the set.
   * @param checked - The flag indicating whether the UID should be added (true) or removed (false) from the set.
   * @returns void
   */
  updateCheckedSet(uid: string, checked: boolean): void {
    const functionalGroup = this.findFunctionalGroupByUid(uid);
    if (functionalGroup) {
      functionalGroup['checkStatus'] = checked ? 'checked' : 'unchecked';
    }
    if (checked) {
      this.setOfCheckedId.add(uid);
    } else {
      this.setOfCheckedId.delete(uid);
    }
    this.uids = [...this.setOfCheckedId];
    if (this.uids.length) {
      this.fetchAnnotationsListAsPerFG(this.uids, this.projectId);
    }
  }

  /**
   * Refreshes the checked status of the list functional group component.
   * It updates the 'checked' and 'indeterminate' properties based on the selected items.
   * If there are any 'uids' present, it fetches the annotations list as per the functional group.
   */
  refreshCheckedStatus(): void {
    const listOfEnabledData = this.flattenTree(this.functionalAnalysisTree);
    this.checked = listOfEnabledData.every(({ uid }) => typeof uid === 'string' && this.setOfCheckedId.has(uid));
    this.indeterminate =
      listOfEnabledData.some(({ uid }) => typeof uid === 'string' && this.setOfCheckedId.has(uid)) && !this.checked;
    if (this.uids.length) {
      this.fetchAnnotationsListAsPerFG(this.uids, this.projectId);
    }
  }

  /**
   * Finds a functional group in the flattened tree by UID.
   * @param uid - The UID of the functional group to find.
   * @returns The functional group if found, otherwise null.
   */
  findFunctionalGroupByUid(uid: string): NzTreeNodeOptions | null {
    const stack = [...this.functionalAnalysisTree];
    while (stack.length > 0) {
      const node = stack.pop();
      if (node) {
        if (node.key === uid) {
          return node;
        }
        if (node.children) {
          stack.push(...node.children);
        }
      }
    }
    return null;
  }

  /**
   * Flattens a tree structure represented by an array of NzTreeNodeOptions.
   * @param tree The tree structure to flatten.
   * @returns The flattened tree structure as an array of NzTreeNodeOptions.
   */
  flattenTree(tree: NzTreeNodeOptions[]): NzTreeNodeOptions[] {
    const flattened: NzTreeNodeOptions[] = [];
    tree.forEach((node) => {
      flattened.push(node);
      if (node.children && node.children.length > 0) {
        flattened.push(...this.flattenTree(node.children));
      }
    });
    return flattened;
  }

  /**
   * Handles checking or unchecking an individual item in the functional group table.
   * @param uid - The unique identifier of the item to be checked or unchecked.
   * @param checked - A boolean indicating whether the item should be checked or unchecked.
   * @returns void
   */
  onItemChecked(uid: string, checked: boolean, updateSave: boolean): void {
    if (updateSave) {
      this.saveButtonState = false;
    }
    this.updateCheckedSet(uid, checked);
    this.refreshCheckedStatus();
    this.fetchAnnotationsListAsPerFG(this.uids, this.projectId);
  }

  /**
   * method to get the details of FGs which needs to be updated
   * @param updatedFunctionalGps: all the FGs updated
   */
  getUpdatedFunctionalDetails(updatedFunctionalGps: Event): void {
    this.saveButtonState = false;
    const flattenFunctionalTree = this.flattenTree(this.functionalAnalysisTree);
    for (const fg of flattenFunctionalTree) {
      if (updatedFunctionalGps.hasOwnProperty(fg.uid as string)) {
        fg.annotations = updatedFunctionalGps[fg.uid].annotations;
        fg['hasUpdated'] = true;
      }
    }
  }

  /**
   * Removes the functional group with the specified UID from the functionalAnalysisTree.
   * @param uid The UID of the functional group to be removed.
   */
  removeFG(uid: string): void {
    this.saveButtonState = false;
    this.deleteFgs.push(uid);
    this.removeFromTree(uid, this.functionalAnalysisTree);
  }
  /**
   * Removes the functional group with the specified UID from the functionalAnalysisTree.
   * @param uid The UID of the functional group to be removed.
   */
  removeFGFromTree(uid: string): void {
    this.saveButtonState = false;
    this.removeFromTree(uid, this.functionalAnalysisTree);
  }

  deleteAnnotationsFromFB(requestBody: DeleteFunctionalAnnotations): void {
    this.deleteFunctionalAnnotationsFromFb.push(requestBody);
  }

  /**
   * Handles the delete and update operations for functional groups.
   * Destroys the modal and passes the data to the caller upon completion.
   */
  onSave(): void {
    this.parentUUIDs = [];
    const deletedAnnotationIds: number[] = [];
    if(this?.deleteFunctionalAnnotationsFromFb?.length > 0) {
      const updateFbs: Array<Observable<any>> = [];
      this.deleteFunctionalAnnotationsFromFb.forEach(element => {
        const uid = element.uid;
        const requestBody = {
          description: element.description,
          name: element.name,
          children: element.children
        };
        deletedAnnotationIds.push(element.annotationId);
        updateFbs.push(this.functionalBlockControllerService.updateFunctionalBlock(this.projectId, uid, requestBody));
      });
      forkJoin(updateFbs).subscribe((res) => {
        if (res) {
          this.modalRef?.destroy({ save: true, createNew: false, deletedAnnotationIds });
        }
        this.annotationFunctionalBlockController.deleteEmptyAutoGeneratedFunctionalUnits(this.projectId, AnnotationPojo.TypeEnum.FUNCTIONAL).subscribe();
      }
      );

    }

    if (this.deleteFgs?.length > 0) {
      // Perform bulk delete operation
      const deleteFunctionalGroups: Array<Observable<any>> = [];
      this.deleteFgs.forEach((uid: string) => {
        deleteFunctionalGroups.push(this.functionalBlockControllerService.deleteFunctionalBlock(this.projectId, uid));
      });
      forkJoin(deleteFunctionalGroups).subscribe((res) => {
        if (res) {
          this.modalRef?.destroy({ save: true, createNew: false, FgIds: this.deleteFgs });
        }
      });
    }

    // Perform bulk update operation
    const flattenFunctionalTree = this.flattenTree(this.functionalAnalysisTree);
    const updateFunctionalGroups: Array<{ [key: string]: any }> = [];
    const updateFgs: string[] = [];
    flattenFunctionalTree.forEach((fg) => {
      if (fg?.checkStatus === 'checked' && ! fg?.hasUpdated) {
        const annotationUUIDs = this.mappedData[fg.uid].annotations.map((annotation: any) => annotation.uid);
        let childrenUUIDs = fg?.children?.map((child: any) => child.uid);
        childrenUUIDs = [...new Set([...childrenUUIDs, ...annotationUUIDs])];
        const requestBody = {
          uid: fg.uid,
          description: fg.description,
          name: fg.title,
          children: childrenUUIDs,
        };
        this.findParentUidOfUpdatedFG(fg.uid as string, flattenFunctionalTree);
        updateFgs.push(fg.uid as string);
        updateFunctionalGroups.push(
          this.functionalBlockControllerService.updateFunctionalBlock(this.projectId, fg.uid as string, requestBody)
        );
      } else if (fg?.checkStatus === 'unchecked' && ! fg?.hasUpdated) {
        const filteredAnnotations = fg.annotations.filter(
          (annotation: AnnotationItem) => annotation.uid !== this.annotation['uid']
        );
        const requestBody = {
          uid: fg.uid,
          description: fg.title,
          name: fg.name,
          children: filteredAnnotations.map((annotation: AnnotationItem) => annotation.uid)
        };
        updateFgs.push(fg.uid as string);
        this.findParentUidOfUpdatedFG(fg.uid as string, flattenFunctionalTree);
        updateFunctionalGroups.push(
          this.functionalBlockControllerService.updateFunctionalBlock(this.projectId, fg.uid as string, requestBody)
        );
      } else if(fg?.hasUpdated) {
        // const filteredAnnotations =  fg?.annotations?.map((annotation: AnnotationItem) => annotation.uid);
        const requestBody = {
          uid: fg.uid,
          description: fg.title,
          name: fg.name,
          children: fg?.annotations?.map((annotation: AnnotationItem) => annotation.uid)
        };
        updateFgs.push(fg.uid as string);
        this.findParentUidOfUpdatedFG(fg.uid as string, flattenFunctionalTree);
        updateFunctionalGroups.push(
          this.functionalBlockControllerService.updateFunctionalBlock(this.projectId, fg.uid as string, requestBody)
        );
      }
    });
    forkJoin(updateFunctionalGroups).subscribe(() => {
      this.modalRef?.destroy({ save: true, createNew: false, updatIds: updateFgs, deletedAnnotationIds });
      const combinedArray = Array.from(new Set(updateFgs.concat(this.parentUUIDs)));
      this.functionalBlockControllerService.computeFunctionalBlock(this.projectId, new Set(combinedArray)).subscribe(() => {
        this.annotationFunctionalBlockController.deleteEmptyAutoGeneratedFunctionalUnits(this.projectId, AnnotationPojo.TypeEnum.FUNCTIONAL).subscribe();
      });
    });
    forkJoin(updateFunctionalGroups).subscribe(() => {
      this.modalRef?.destroy({ save: true, createNew: false, updatIds: updateFgs, deletedAnnotationIds });
    });
  }

  /**
   * Handles the cancel action for the modal.
   * Closes the modal and destroys it.
   */
  handleCancel(): void {
    this.modalRef?.destroy();
  }

  /**
   * Destroys the modal with specific parameters indicating the creation of a new item.
   * @returns void
   */
  createNew(): void {
    this.modalRef?.destroy({ save: false, createNew: true });
  }

  /**
   * method to show filter again
   */
  showFilter(): void {
    this.toggleFilter = !this.toggleFilter;
  }

  /**
   * method to get the taxonomy ids
   * @param event: output event
   */
  updateTaxonomySelection(event: TaxonomyFilterSelected[]): void {
    this.selectedTaxonomyIds = event;
  }

  /**
   * Determines whether the save button should be enabled based on the state of the functional analysis tree.
   * The save button should be enabled if any of the following conditions are met:
   * - A functional group is checked and has not been updated
   * - A functional group is unchecked and has not been updated
   * - A functional group has been updated
   * @returns A boolean value indicating whether the save button should be enabled.
   */
  shouldEnableSaveButton(): boolean {
    const flattenFunctionalTree = this.flattenTree(this.functionalAnalysisTree);
    for (const fg of flattenFunctionalTree) {
      if (
        (fg?.checkStatus === 'checked' && !fg?.hasUpdated) ||
        (fg?.checkStatus === 'unchecked' && !fg?.hasUpdated) ||
        fg?.hasUpdated
      ) {
        return true;
      }
    }
    return false;
  }

  /**
   * method for module search on type and getting all the matched records
   * @param search: typed character
   */
  onModuleNameSearch(search: string): void {
    const dropdownElement = document.querySelectorAll('[id^="cdk-overlay-"]');
    const dropdownClassList = dropdownElement[dropdownElement.length - 1]?.classList;
    dropdownClassList?.add('module-listing__select-dropdown');
    this.listSearchModule = [];
    if (search.trim().length >= 1) {
      dropdownClassList?.remove('module-listing__select-dropdown');
      const filterObject = {
        content_name: { eq: search },
      };
      this.modulesGQL.fetch({ projectId: this.projectId, size: 10, page: 0, filterObject }).subscribe((listModule) => {
        listModule.data.modules.content.forEach((module) => {
          if (this.listSearchModule.findIndex((moduleOpt) => moduleOpt.label === module.name) === -1) {
            this.listSearchModule.push({
              value: module.id,
              label: module.name,
              path: module.path,
              description: module.description
            });
          }
        });
      });
    }
  }

  /**
   * method to check if buttons should be disabled or not
   * @returns state of the buttons
   */
  isButtonDisabled(): boolean {
    return ! (
      !!this.selectedModuleIds.length ||
      !!this.selectReachabilityIds.length ||
      !!this.selectedTaxonomyIds.length
    );
  }

  /**
   * method to reset all the filter
   */
  onReset(): void {
    this.selectedModuleIds = [];
    this.selectedTaxonomyIds = [];
    this.selectReachabilityIds = [];
    this.taxonomyFilterComponent.selectedValue = [];
    // void this.router.navigate([], { replaceUrl: true });
    this.isFiltersActive = false;
  }

  /**
   * method to set all the filterIds in the URL
   */
  onApplyFilter(): void {
    this.filterDetails = {};
    if (this.selectedModuleIds.length) {
      this.filterDetails['moduleIds'] = this.selectedModuleIds;
    }
    if (this.selectedTaxonomyIds.length) {
      this.filterDetails['taxonomyIds'] = this.selectedTaxonomyIds;
    }
    if (this.selectReachabilityIds.length) {
      this.filterDetails['reachabilityIds'] = this.selectReachabilityIds;
    }
    this.isFiltersActive = !!Object.keys(this.filterDetails).length;
    this.toggleFilter = false;
    if (this.annotationsOfModule.length > 0) {
      this.getTreeDetailsBasedOnFilter(this.filterDetails, 0);
    }
    this.functionalKeys.length = 0;
  }

  /**
   * Checks if an object is empty.
   * @param obj - The object to check.
   * @returns True if the object is empty, false otherwise.
   */
  isEmptyObject(obj: object): boolean {
    return Object.keys(obj).length === 0;
  }

  private getFunctionalBlocks(): void {
    const requestQuery = {
      query: `{
        functionalBlocks(projectId:  ${this.projectId}, filterObject: { content_generatedFrom_annotationId: { in: ${this.annotationIds} } }) {
            content {
              uid 
              parents(filterObject: { content_type: { eq: FUNCTIONAL_GROUP } }) {
                content {
                    uid  
                    name
                }
              }
              generatedFrom {
                annotationId
              }
            }
          }
        }`,
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: any) => {
      if (response && response?.data?.functionalBlocks) {
        const functionalBlocksRecords: any[] = response?.data?.functionalBlocks?.content;
        const functionalBlocksRecordData: Array<{ uid: string; name: string }> = [];
        functionalBlocksRecords.forEach((record) => {
          const functionalBlocksRecord: any[] = record?.parents?.content;
          functionalBlocksRecord?.forEach((element: any) => {
            functionalBlocksRecordData.push({
              uid: element.uid,
              name: element.name
            });
          });
        });
        functionalBlocksRecordData?.forEach((data) => {
          this.onItemChecked(data.uid, true, false);
          this.expandNodesByUid(data.uid, this.functionalAnalysisTree);
        });
        this.fetchAllTheAnnotationsOfModule();
      }
    });
  }

  private expandNodesByUid(uid: string, tree: any[]): void {
    const expandNode = (node: any): boolean => {
      if (node.uid === uid) {
        node.expanded = true;
        node.isChecked = true;
          this.ngZone.run(() => {
            this.nzExpandedKeys = [...this.nzExpandedKeys, node.uid as string];
          });
        return true;
      }
      if (node.children && node.children.length > 0) {
        for (const child of node.children) {
          if (expandNode(child)) {
            node.expanded = true;
            this.ngZone.run(() => {
              this.nzExpandedKeys = [...this.nzExpandedKeys, node.uid as string];
              this.nzExpandedKeys = [...this.nzExpandedKeys, child.uid as string];
          });
            child.isChecked = true;
            return true;
          }
        }
      }
      return false;
    };
    for (const node of tree) {
      if (expandNode(node)) {
        break;
      }
    }
  }

  private fetchAnnotationsListAsPerFG(functionalGroupIds: string[], projectId: number): void {
    const requestQuery = {
      query: `query functionalGroupDetails($projectId: Long, $functionalGroupIds: [UUID]){
        functionalBlocks(projectId: $projectId, filterObject: {content_uid: {in: $functionalGroupIds}}) {
        content {
          uid
        name
        description
        children {
          content {
            uid
            name
            generatedFrom {
              annotation {
                id
                state
                type
                categoryName
                name
              }
              annotationId
            }
          }
        }
        }
      }
    }`,
    };
    this.fgGraphqlSubscription?.unsubscribe();
    this.fgGraphqlSubscription = this.graphQlControllerService
      .graphQl({ ...requestQuery, variables: { projectId, functionalGroupIds } })
      .subscribe((response: { [key: string]: any }) => {
        if (response) {
          this.mappedData = this.createAnnotationFgMapping(
            response.data.functionalBlocks.content as { [key: string]: any }
          );
        }
      });
  }

  /**
   * Finds the parent UIDs of the updated functional group.
   * @param uid - The UID of the updated functional group.
   * @returns An array of parent UIDs.
   */
  private findParentUidOfUpdatedFG(uid: string, flattenFunctionalTree: NzTreeNodeOptions[], parentNode: NzTreeNodeOptions | null = null): void {
    flattenFunctionalTree.forEach((node: NzTreeNodeOptions) => {
      if (node.children) {
        node.children.forEach((child) => {
          if (child.uid === uid) {
            this.parentUUIDs.push(node.uid as string);
            if (parentNode) {
              this.parentUUIDs.push(parentNode.uid as string);
            }
          } else {
            this.findParentUidOfUpdatedFG(uid, node.children, node);
          }
        });
      }
    });
  }

  private createAnnotationFgMapping(data: { [key: string]: any }) {
    const annotationMapping: any = {};
    data.forEach((dataItem: { [key: string]: any }) => {
      annotationMapping[dataItem.uid] = {
        uid: dataItem.uid,
        name: dataItem.name,
        description: dataItem.description,
        togglePanel: false,
        annotations: this.getAnnotations(dataItem),
        customStyle: {
          'background-color': '#fff',
          'border-radius': '0',
          'margin-bottom': '16px',
          border: '1px solid #eee'
        },
      };
      if (Object.keys(this.annotation).length) {
        let addToExisting = false;
        dataItem?.children?.content.forEach((item: any) => {
          if (item?.generatedFrom?.annotationId === this.annotation?.id) {
            addToExisting = true;
          }
        });
        if (
          ! addToExisting &&
          ! annotationMapping[dataItem.uid].annotations.some((item: { uid: any }) => item.uid === this.annotation.uid)
        ) {
          annotationMapping[dataItem.uid].annotations.push({
            typeLabel: this.annotation.type,
            uid: this.annotation.uid,
            categoryName: this.annotation.categoryName,
            name: this.annotation.name
          });
        }
      }
    });
    return annotationMapping;
  }

  private getAnnotations(data: any): any[] {
    const annotations: any = [];
    data.children.content.forEach((item: any) => {
      if (item?.generatedFrom?.annotationId) {
        annotations.push({
          uid: item.uid,
          stateLabel: item?.generatedFrom?.annotation?.state,
          typeLabel: item?.generatedFrom?.annotation?.type,
          id: item?.generatedFrom?.annotationId,
          categoryName: item?.generatedFrom?.annotation?.categoryName,
          name: item?.generatedFrom?.annotation?.name,
        });
      }
    });
    return annotations;
  }

  private fetchAllTheAnnotationsOfModule(): void {
    this.annotationsOfModule?.forEach((annotationItem) => {
      this.annotationMappingWithIds[annotationItem.id] = {
        stateLabel: annotationItem.state,
        typeLabel: annotationItem.type,
        id: annotationItem.id,
        categoryName: annotationItem.categoryName,
        name: annotationItem.name
      };
    });
  }

  /*
   * Removes the node with the specified UID from the tree recursively.
   * @param uid The UID of the node to be removed.
   * @param tree The tree structure to search and remove the node from.
   */
  private removeFromTree(uid: string, tree: NzTreeNodeOptions[]): void {
    for (let i = 0; i < tree.length; i++) {
      const node = tree[i];
      if (node.key === uid) {
        tree.splice(i, 1);
        return;
      }
      if (node.children && node.children.length > 0) {
        this.removeFromTree(uid, node.children);
      }
    }
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
    }`,
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      if (response) {
        this.listReachabilityBlock = response.data.functionalBlocks.content.map(
          (functionalBlockItem: { [key: string]: any }) => ({
            label: functionalBlockItem.name,
            value: `"${functionalBlockItem.uid}"`
          })
        );
        this.listReachabilityBlock = this.listReachabilityBlock.sort((a, b) => a.label.localeCompare(b.label));
      }
    });
  }
}
