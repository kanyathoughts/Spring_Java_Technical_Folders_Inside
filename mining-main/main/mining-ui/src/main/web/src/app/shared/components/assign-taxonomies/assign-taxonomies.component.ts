import { FlatTreeControl } from '@angular/cdk/tree';
import { Component, EventEmitter, Input, OnInit, Optional, Output } from '@angular/core';
import { NzTreeFlatDataSource, NzTreeFlattener } from 'ng-zorro-antd/tree-view';
import { BehaviorSubject, combineLatest, Observable } from 'rxjs';
import { auditTime, map } from 'rxjs/operators';
import { FilteredTreeResult, FlatNode, TreeNode } from '@app/shared/interfaces/tree-view.interface';
import { AssignTaxonomiesModalService } from './assign-taxonomies-modal.service';
import { FormResult } from '../../interfaces/annotation-editor.interface';
import { NzDrawerRef } from 'ng-zorro-antd/drawer';
import { MiningTableAction } from '../mining-table/mining-table-action.interface';
import { HttpErrorResponse } from '@angular/common/http';
import { Logger } from '@app/core/services/logger.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { JobInformation, TaxonomyAssignmentsGetResponse, TaxonomyAssignmentsSetRequest,
  TaxonomyControllerService, TaxonomySetAssignment } from '@innowake/mining-api-angular-client';
import { TaxonomyGetAssignment } from '@innowake/mining-api-angular-client/model/taxonomyGetAssignment';
const log = new Logger('AssignTaxonomiesComponent');

@Component({
  selector: 'assign-taxonomies',
  templateUrl: './assign-taxonomies.component.html',
})
export class AssignTaxonomiesComponent implements OnInit {

  @Input() taxonomyResponse: TaxonomyAssignmentsGetResponse;
  @Input() hintText: string;
  @Input() moduleIdArray: number[];
  @Input() projectId: number;
  @Input() parentComponent = false;
  @Input() nzModalFooter = true;
  @Input() modulePathPatterns: string[];
  @Output() formResult = new EventEmitter<FormResult>();
  taxonomyNodes: TreeNode[] = [];
  openNodes: TreeNode[] = [];
  isLoading: boolean;
  taxonomyRequest: TaxonomyAssignmentsSetRequest = {};
  disableSave = true;
  // Properties used for tree view search.
  originData$: BehaviorSubject<TreeNode[]>;
  filteredData$: Observable<FilteredTreeResult>;
  emptySearch = false;
  flatNodeMap = new Map<FlatNode, TreeNode>();
  nestedNodeMap = new Map<TreeNode, FlatNode>();
  expandedNodes: TreeNode[] = [];
  searchValue = '';
  searchValue$ = new BehaviorSubject<string>('');
  treeControl: FlatTreeControl<FlatNode, TreeNode>;
  dataSource: any;
  private previousTaxonomyResponse: string;

  constructor(
    private assignTaxonomiesModalService: AssignTaxonomiesModalService,
    private taxonomyControllerService: TaxonomyControllerService,
    private jobManager: JobManagerService,
    @Optional() private drawerRef: NzDrawerRef<MiningTableAction>
  ) {
    this.treeControl = new FlatTreeControl<FlatNode, TreeNode>(
      node => node.level,
      node => node.checkedState,
      {
        trackBy: flatNode => this.flatNodeMap.get(flatNode)
      }
    );
    const treeFlattener = new NzTreeFlattener<TreeNode, FlatNode, TreeNode>(
      this.transformer,
      node => node.level,
      node => node.expandable,
      node => node.children
    );
    this.dataSource = new NzTreeFlatDataSource(this.treeControl, treeFlattener);
  }

  ngOnInit(): void {
    this.createTreeData();
    const openNodes: TreeNode[] = [];
    this.previousTaxonomyResponse = JSON.stringify(this.taxonomyResponse.taxonomies);
    this.taxonomyNodes.forEach((node) => {
      node.children?.forEach((child) => {
        child?.children.forEach((childNode) => {
          if (childNode.checkedState || childNode.state === 'SOME') {
            child.checkedState = true;
            node.checkedState = true;
            openNodes.push(child);
          }
        });
      });
    });
    this.treeControl.expansionModel.select(...this.taxonomyNodes);
    this.treeControl.expansionModel.select(...openNodes);
    this.originData$ = new BehaviorSubject(this.taxonomyNodes);
    this.filteredData$ = combineLatest([
      this.originData$,
      this.searchValue$.pipe(
        auditTime(300),
        map(value => (this.searchValue = value.toLowerCase()))
      )
    ]).pipe(map(([data, value]) => (value ? this.filterTreeData(data, value) : new FilteredTreeResult(data))));
    this.filteredData$.subscribe(result => {
      this.dataSource.setData(result.treeData);
      if (this.searchValue) {
        if (this.expandedNodes.length === 0) {
          this.expandedNodes = this.treeControl.expansionModel.selected;
          this.treeControl.expansionModel.clear();
        }
        this.treeControl.expansionModel.select(...result.needsToExpanded);
        if (result.treeData.length === 0) {
          this.emptySearch = true;
        } else {
          this.emptySearch = false;
        }
      } else {
        this.emptySearch = false;
        if (this.expandedNodes.length) {
          this.treeControl.expansionModel.clear();
          this.treeControl.expansionModel.select(...this.expandedNodes);
          this.expandedNodes = [];
        }
      }
    });
  }

  /**
   * Update the state of the taxonomy.
   *
   * @param checkedState flag if state is 'ALL' or 'NONE'.
   * @param node contains node data.
   */
  updateAllChecked(checkedState: boolean, node: TreeNode): void {
    if (checkedState) {
      node.state = 'ALL';
      this.updateTaxonomyState(node.id, 'ALL');
    } else {
      node.state = 'NONE';
      this.updateTaxonomyState(node.id, 'NONE');
    }
    this.updateParentNode();
    this.disableSave = false;
    this.assignTaxonomiesModalService.setUpdatedTaxonomyData(this.taxonomyResponse.taxonomies);
  }

  /**
   * Handles cancel operation without saving.
   */
  handleCancel(): void {
    this.assignTaxonomiesModalService.triggerLoadSubject(false);
    this.assignTaxonomiesModalService.closeModal.next(true);
    this.drawerRef?.close();
    this.formResult.emit(FormResult.Canceled);
  }

  /**
   * Update taxonomy data after saving.
   */
  updateTaxonomies(): void {
    this.isLoading = true;
    const updatedList: TaxonomySetAssignment[] = [];
    this.taxonomyResponse.taxonomies.forEach((element, index) => {
      if (element.state !== JSON.parse(this.previousTaxonomyResponse)[index].state) {
        updatedList.push({state: element.state, taxonomyId: element.taxonomy.id}) ;
      }
    });
    this.taxonomyRequest = { 'modules': { 'ids': this.moduleIdArray,'pathPatterns': this.modulePathPatterns }, 'taxonomies': updatedList };
    const submitObservable = this.taxonomyControllerService.bulkUpdateTaxonomiesToModules(this.projectId, this.taxonomyRequest);
    submitObservable.subscribe((response: string[]) => {
      this.getJobStatus(response.toString());
    }, (error: HttpErrorResponse) => {
      this.disableSave = false;
      log.error(error.message);
    });
  }

  /**
   * Filters tree data based on search.
   * @param data tree data.
   * @param value search value.
   * @returns FilteredTreeResult value
   */
  filterTreeData(data: TreeNode[], value: string): FilteredTreeResult {
    const needsToExpanded = new Set<TreeNode>();
    let isExpanded: boolean;
    const filterData = (node: TreeNode, result: TreeNode[]) => {
      if (node.name.toLowerCase().search(value.toLowerCase()) !== -1) {
        isExpanded = true;
        result.push(node);
      }
      if (Array.isArray(node.children)) {
        const nodes = node.children.reduce((a, b) => filterData(b, a), [] as TreeNode[]);
        if (nodes.length) {
          const parentNode = { ...node, children: nodes };
          if (isExpanded) {
            needsToExpanded.add(parentNode);
          }
          result.push(parentNode);
        }
      }
      return result;
    };
    const treeData = data.reduce((a, b) => filterData(b, a), [] as TreeNode[]);
    const uniqueTreeData: TreeNode[] = [];
    const uniqueId: number[] = [];
    treeData.reverse().forEach((data) => {
      if (uniqueId.indexOf(data.id) === -1) {
        uniqueId.push(data.id);
        uniqueTreeData.push(data);
      }
    });
    return new FilteredTreeResult(uniqueTreeData.reverse(), [...needsToExpanded]);
  }

  /**
   * Checks if the node has children.
   * @param index index.
   * @param node individual node.
   * @returns boolean value.
   */
  hasChild: (index: number, node: FlatNode) => boolean = (index: number, node: FlatNode) => node.expandable && (node.level === 0 || node.level === 1);

  private getJobStatus(jobId: string): void {
    const remoteJob = {
      jobId: jobId as unknown as string,
      autoDownloadResult: true,
      foreground: true
    };
    this.jobManager.register(remoteJob).status$.subscribe((status: JobInformation.StatusEnum) => {
      if (status === JobInformation.StatusEnum.SUCCESS) {
        this.assignTaxonomiesModalService.triggerLoadSubject(true);
        this.formResult.emit(FormResult.Saved);
        this.drawerRef?.close({result: FormResult.Saved});
        this.isLoading = false;
        if (this.parentComponent) {
          this.disableSave = true;
        }
      }
    });
    if ( ! this.parentComponent) {
      this.handleCancel();
    }
  }

  private transformer = (node: TreeNode, level: number) => {
    const existingNode = this.nestedNodeMap.get(node);
    const flatNode =
      existingNode && existingNode.name === node.name
        ? existingNode
        : {
          expandable: !!node.children && node.children.length > 0,
          name: node.name,
          level,
          checkedState: node.checkedState,
          id: node.id,
          state: node.state
        };
    this.flatNodeMap.set(flatNode, node);
    this.nestedNodeMap.set(node, flatNode);
    return flatNode;
  };

  private updateTaxonomyState(taxonomyId: number, state: TaxonomyGetAssignment.StateEnum) {
    this.taxonomyResponse.taxonomies.forEach((taxonomy) => {
      if (taxonomy.taxonomy.id === taxonomyId) {
        taxonomy.state = state;
      }
    });
  }

  private updateParentNode(): void {
    let parentLevelZero: FlatNode;
    let parentLevelOne: FlatNode;
    this.treeControl.dataNodes.forEach((node: FlatNode) => {
      if (node.level === 0) {
        parentLevelZero = node;
        parentLevelZero.checkedState = false;
      } else if (node.level === 1) {
        parentLevelOne = node;
        parentLevelOne.checkedState = false;
      } else if (node.checkedState || node.state === 'SOME') {
        parentLevelZero.checkedState = true;
        parentLevelOne.checkedState = true;
      }
    });
  }

  private createTreeData(): void {
    const assignedTaxonomies: TaxonomyGetAssignment = this.taxonomyResponse.taxonomies.reduce(
      (newList: TaxonomyGetAssignment, currentValue: TaxonomyGetAssignment) => {
        newList[currentValue.taxonomy.type.category.name] = newList[currentValue.taxonomy.type.category.name] || {};
        newList[currentValue.taxonomy.type.category.name][currentValue.taxonomy.type.name]
          = [...(newList[currentValue.taxonomy.type.category.name][currentValue.taxonomy.type.name] || []),
        { name: currentValue.taxonomy.name, state: currentValue.state, id: currentValue.taxonomy.id }];
        return newList;
      }, {}
    );
    Object.keys(assignedTaxonomies).forEach((category) => {
      const TREE_DATA: TreeNode = {
        name: category,
        id: null,
        children: [],
        checkedState: false,
      };
      Object.keys(assignedTaxonomies[category] as object).forEach((type) => {
        const TREE_CHILDREN: TreeNode = {
          name: type,
          id: null,
          children: [],
          checkedState: false
        };
        assignedTaxonomies[category][type].forEach((childNode: { state: string; name: string; id: number; }) => {
          TREE_CHILDREN.id = childNode.id;
          TREE_DATA.id = childNode.id;
          TREE_CHILDREN.children.push({
            name: childNode.name,
            id: childNode.id,
            state: childNode.state,
            checkedState: childNode.state === 'ALL' ? true : false
          });
        });
        TREE_CHILDREN.children.sort((a, b) => a.name.toLowerCase().localeCompare(b.name.toLowerCase()));
        TREE_DATA.children.push(TREE_CHILDREN);
      });
      TREE_DATA.children.sort((a, b) => a.name.toLowerCase().localeCompare(b.name.toLowerCase()));
      this.taxonomyNodes.push(TREE_DATA);
      this.taxonomyNodes.sort((a, b) => a.name.toLowerCase().localeCompare(b.name.toLowerCase()));
    });
  }
}
