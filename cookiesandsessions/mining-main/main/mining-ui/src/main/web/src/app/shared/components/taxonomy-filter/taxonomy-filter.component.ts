import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { NzTreeNode, NzTreeNodeOptions } from 'ng-zorro-antd/core/tree';
import { Subscription } from 'rxjs';
import { MetricsFilterService } from '@app/core/services/metrics-filter.service';
import { taxonomyReduceList } from '@app/core/utils/taxonomy.utils';
import { TaxonomyFilterSelected } from './taxonomy-filter-selected.interface';
import { TaxonomyControllerService, TaxonomyPojo } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-taxonomy-filter',
  templateUrl: './taxonomy-filter.component.html'
})
export class TaxonomyFilterComponent implements OnInit, OnDestroy {
  @Input() useMetricsFilterService = true;
  @Input() reachabilityFilter = false;
  @Input() disableFilter = false;
  @Input() categoryFilter: string = null;
  @Input() selectedIds: number[] = []; // only to be used on pages other than metrics page.
  @Output() taxonomyId = new EventEmitter<any>();
  @Output() selectedTaxonomyDetails = new EventEmitter<TaxonomyFilterSelected[]>();
  moduleTaxonomy: TaxonomyPojo[] = [];
  projectId: number;
  clientProjectSubscription: Subscription;
  selectedValue: string[] = [];
  nodes: NzTreeNodeOptions[] = [];
  modifiedArray: NzTreeNodeOptions[] = [];

  constructor(
    private metricsFilterService: MetricsFilterService,
    private taxonomyControllerService: TaxonomyControllerService,
    private clientProjectRelationship: ClientProjectRelationshipService) { }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.projectId = response.getProjectId();
      this.taxonomyControllerService.findAllTaxonomies(this.projectId).subscribe((response: TaxonomyPojo[]) => {
        this.getTaxonomyTreeData(response);
        if ( ! this.useMetricsFilterService) {
          if (this.selectedIds.length) {
            this.setSelectedNodes(this.nodes, this.selectedIds);
          } else {
            this.selectedValue = [];
          }
        } else {
          this.metricsFilterService.getMetricsTaxonomyFilter().subscribe((filterDetail: TaxonomyFilterSelected[]) => {
            if (filterDetail.length) {
              filterDetail.forEach((filterDetailsItem: TaxonomyFilterSelected) => {
                const selectedTaxonomy = filterDetailsItem.taxonomyTitle.split(': ').join('_');
                this.selectedValue.push(`${selectedTaxonomy}_${filterDetailsItem.taxonomyId}`);
              });
            } else {
              this.selectedValue = [];
            }
          });
        }
      });
    });
  };

  /**
   * Method triggers on selection of tree nodes
   * @param $event is the selected value.
   */
  onTaxonomySelection(event: string[]): void {
    this.taxonomyId.emit(event);
    const taxonomyDetails: TaxonomyFilterSelected[] = [];
    event.forEach((eventItem: any) => {
      const selectedNode = eventItem.split('_');
      taxonomyDetails.push({
        /* names can contain '_' */
        taxonomyId: eventItem.substring(eventItem.lastIndexOf('_') + 1),
        taxonomyTitle: selectedNode[0],
        selectedTaxonomy: event
      });
    });
    this.selectedTaxonomyDetails.emit(taxonomyDetails);
    if (this.useMetricsFilterService) {
      this.metricsFilterService.setMetricsTaxonomyFilter(taxonomyDetails);
    }
  }

  /**
   * Method to display the text on the input field of tree.
   * @param node is the node on the selection of the any
   * @returns the customized input that displayed on the input field of tree.
   */
  displayTrigger(node: NzTreeNode): string {
    return node.level === 0 ? node.key.split('_')[0] : node.parentNode?.title + ': ' + node.title;
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.clientProjectSubscription.unsubscribe();
  }

  /**
   * Method to Convert the data into the Tree structure.
   * @param taxonomyList contains the list of taxonomy types.
   */
  private getTaxonomyTreeData(taxonomyList: TaxonomyPojo[]): void {
    const taxonomyIdPerChild: number[] = [];
    const groupTaxonomyIds: number[] = [];
    let groupedTaxonomy = taxonomyList.reduce((newList: TaxonomyPojo, currentValue: TaxonomyPojo) => taxonomyReduceList(newList, currentValue), {});
    if (this.categoryFilter !== null) {
      groupedTaxonomy = taxonomyList.filter(key => key.type?.category?.name.includes(this.categoryFilter))
        .reduce((newList: TaxonomyPojo, currentValue: TaxonomyPojo) => taxonomyReduceList(newList, currentValue), {});
    }
    Object.keys(groupedTaxonomy).forEach((category: string) => {
      const tree: NzTreeNodeOptions = {
        title: category,
        key: category,
        expanded: true,
        children: [],
        isLeaf: false,
        selectable: false
      };
      Object.keys(groupedTaxonomy[category] as object).forEach((type: string) => {
        const tree_children: NzTreeNodeOptions = {
          title: type,
          key: `${category}:${type}`,
          expanded: false,
          children: [],
          isLeaf: false,
          selectable: false
        };
        groupedTaxonomy[category][type].forEach((childNode: NzTreeNodeOptions) => {
          tree_children.children.push({
            title: childNode.name,
            key: `${type}:${childNode.name}_${childNode.id}`,
            isLeaf: true
          });
        });
        tree_children.children.sort((a, b) => a.title.toLowerCase().localeCompare(b.title.toLowerCase()));
        tree.children.push(tree_children);
      });
      groupTaxonomyIds.length = 0;
      tree.children.forEach((treeChildrenItem: NzTreeNodeOptions) => {
        taxonomyIdPerChild.length = 0;
        treeChildrenItem.children.forEach((children: NzTreeNodeOptions) => {
          /* names can contain '_' so we search for the last occurence */
          const taxonomyIds = +(children.key.substring(children.key.lastIndexOf('_') + 1));
          taxonomyIdPerChild.push(taxonomyIds);
          groupTaxonomyIds.push(taxonomyIds);
        });
        treeChildrenItem.key = `${treeChildrenItem.key}_${taxonomyIdPerChild}`;
      });
      tree.key = `${tree.key}_${groupTaxonomyIds}`;
      tree.children.sort((a, b) => a.title.toLowerCase().localeCompare(b.title.toLowerCase()));
      this.modifiedArray.push(tree);
    });
    this.modifiedArray.sort((a, b) => a.title.localeCompare(b.title));
    this.nodes = this.modifiedArray;
  }

  private setSelectedNodes(nodes: NzTreeNodeOptions[], selectedIds: number[]) {
    nodes.forEach((node: NzTreeNodeOptions) => {
      const taxonomyId = +(node.key.substring(node.key.lastIndexOf('_') + 1));
      if (selectedIds.includes(taxonomyId)) {
        this.selectedValue.push(node.key);
      } else if (node.children) {
        this.setSelectedNodes(node.children, selectedIds);
      }
    });
  }
}
