import { Component, EventEmitter, Input, OnDestroy, OnInit, Optional, Output, TemplateRef, ViewChild } from '@angular/core';
import { Aggregation, ReachabilityBlocks, TaxonomyItem } from '../../utils/reachability-interface';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { TranslateService } from '@ngx-translate/core';
import { NzModalService } from 'ng-zorro-antd/modal';
import { EditReachabilityBlockComponent } from '@app/shared/components/edit-reachability-block/edit-reachability-block.component';
import { RESULT_EDITED } from '@app/shared/components/base-edit-functional-block/base-edit-functional-block.component';
import { NzDrawerRef, NzDrawerService } from 'ng-zorro-antd/drawer';
import { BlockViewDetailsComponent } from '../view-block-details/block-view-details.component';
import { Router } from '@angular/router';
import { ReachabilityService } from '../../utils/reachability.service';
import { EllipsisPipe } from '@app/shared/pipes/ellipsis-pipe';
import { reachabilityBlockChanged } from '@app/shell/header/job-progress-monitor/job-progress-monitor.component';
import { Subscription } from 'rxjs';
@Component({
  selector: 'block-card',
  templateUrl: './block-card.component.html'
})
export class BlockCardComponent implements OnInit, OnDestroy {

  @ViewChild('drawerTitleTemplate', { static: false }) drawerTitleTemplate?: TemplateRef<any>;
  @Output() blockSelectionEvent = new EventEmitter<void>();
  @Output() siderEvent = new EventEmitter<boolean>();
  @Output() updateBlockStatus = new EventEmitter<boolean>();
  @Output() recalculateEvent = new EventEmitter<string>();
  @Input() reachabilityBlockData: ReachabilityBlocks;
  @Input() projectId: number;
  @Input() canEditRB: boolean;
  @Input() showCheckBox: boolean;
  @Input() allReachabilityBlocksData: ReachabilityBlocks[];
  @Input() pageIndex: number;
  taxonomies: TaxonomyItem[];
  uniqueReferencedTaxonomies: string[];
  displayedTaxonomies: string[];
  tooltipTaxonomies: string;
  showDeletedTooltip = 0;
  subscription: Subscription;

  constructor(
    private labelMappingService: LabelMappingService,
    private modalService: NzModalService,
    private translateService: TranslateService,
    private nzDrawerService: NzDrawerService,
    @Optional() private drawerRef: NzDrawerRef<any>,
    private router: Router,
    private reachabilityService: ReachabilityService
  ) { }

  ngOnInit(): void {
    this.taxonomies = this.reachabilityBlockData?.upperBound[0]?.taxonomy;
    this.uniqueReferencedTaxonomies = Array.from(new Set(this.taxonomies?.flatMap(item => item.referencedTaxonomies.map(taxonomy => taxonomy.name))));
    const referencedTaxonomies = this.uniqueReferencedTaxonomies?.slice();
    this.displayedTaxonomies = referencedTaxonomies?.splice(0, 2);
    this.tooltipTaxonomies = referencedTaxonomies?.join(', ');
    if (this.reachabilityBlockData?.upperBound.length > 1 &&
      this.reachabilityBlockData.type.includes('MERGE_PARENT') && this.reachabilityBlockData.upperBound.filter(x => JSON.stringify(x) === '{}').length) {
      this.reachabilityBlockData.upperBound = this.reachabilityBlockData.upperBound.filter(x => JSON.stringify(x) !== '{}');
      this.showDeletedTooltip = this.reachabilityBlockData.upperBound.length > 1 ?
        this.reachabilityBlockData.upperBound.length - 1 : this.reachabilityBlockData.upperBound.length;
    }
    this.subscription = reachabilityBlockChanged.subscribe((uid) => {
      if (uid === this.reachabilityBlockData.uid) {
        this.updateBlockDetails();
      }
    });
  }

  ngOnDestroy(): void {
    this.drawerRef?.close();
    this.subscription.unsubscribe();
  }

  updateBlockDetails(): void {
    this.reachabilityService.fetchReachabilityBlockDetails(this.reachabilityBlockData.uid, this.projectId)
      .subscribe(({ name, description }) => {
        this.reachabilityBlockData.name = name;
        this.reachabilityBlockData.description = description;
      });
  }

  /**
   * Opens table view for a block view.
   */
  openTableView(): void {
    this.drawerRef?.close();
    localStorage.setItem(`${this.projectId}-reachabilityIds`, JSON.stringify([this.reachabilityBlockData.uid]));
    const isMergedBlock = this.reachabilityBlockData.type?.includes('MERGE_PARENT');
    this.reachabilityService.storeReachabilityDetails(this.reachabilityBlockData, this.projectId, isMergedBlock);
    void this.router.navigate(['/project-' + this.projectId + '/reachability/' + this.reachabilityBlockData.uid + '/table']);
  }

  /**
   * Opens Graph View for block.
   */
  openGraphView(): void {
    this.drawerRef?.close();
    const isMergedBlock = this.reachabilityBlockData.type?.includes('MERGE_PARENT');
    this.reachabilityService.storeReachabilityDetails(this.reachabilityBlockData, this.projectId, isMergedBlock);
    void this.router.navigate(['/project-' + this.projectId + '/reachability/' + this.reachabilityBlockData.uid + '/graph'],
      { queryParams: { type: this.reachabilityBlockData?.type, name: this.reachabilityBlockData.name } });
  }

  /**
   * Opens the modules table for a given tag.
   * @param tag - The tag to open the modules table for error/warnings.
   */
  openModulesTable(tag: string): void {
    this.reachabilityService.openModulesTable(tag, this.projectId);
  }

  /**
   * Determines whether to enable the card icon based on the provided icon type.
   * @param icon - The type of icon.
   * @returns A boolean value indicating whether the card icon should be enabled.
   */
  enableCardIcon(icon: string): boolean {
    switch (icon) {
      case 'deleted':
        return this.reachabilityBlockData?.deletedModule;
      case 'outdated':
        return this.reachabilityBlockData.outdatedModule && ! this.reachabilityBlockData.status && ! this.reachabilityBlockData?.deletedModule;
      case 'inactive':
        return this.reachabilityBlockData.status === 'INACTIVE';
      case 'error':
        return this.reachabilityBlockData.blockState.errorCount && ! this.reachabilityBlockData.status &&
          ! this.reachabilityBlockData.outdatedModule && ! this.reachabilityBlockData?.deletedModule;
      case 'warning':
        return this.reachabilityBlockData.blockState.warningsCount && !this.reachabilityBlockData.status &&
          ! this.reachabilityBlockData.outdatedModule && ! this.reachabilityBlockData?.deletedModule && ! this.reachabilityBlockData.blockState.errorCount;
      case 'checkbox':
        return this.canEditRB && ! this.reachabilityBlockData.status && ! this.reachabilityBlockData.outdatedModule &&
          ! this.reachabilityBlockData.deletedModule;
    }
  }

  /**
   * Method to stop event propagation on card click. To be used on all clickable elements inside the card.
   * @param event click event
   */
  stopBubbling(event: Event): void {
    event.stopPropagation();
  }

  /**
   * checks if upper bound in a RB has length.
   * @returns boolean value based on length.
   */
  checkUpperBoundLength(): boolean {
    return ! this.reachabilityBlockData.type.includes('MERGE_PARENT') &&
      this.reachabilityBlockData.upperBound.length === 1 && ! Object.keys(this.reachabilityBlockData.upperBound[0]).length;
  }

  /**
   * Method to character wrap long upper boundary names
   * @param upperBound upperBound name
   */
  upperBoundCharacterWrap(upperBound: string): string {
    return new EllipsisPipe().transform(upperBound, 20);
  }

  /**
   * Method to use JSON.stringify on string
   * @param data string input containing uid
   */
  stringifyData(data: string | {reachabilityIds: string}): string {
    return JSON.stringify(data);
  }

  /**
   * Method for label mapping of Lower Bound
   * @param aggregation is the lower bound aggregation data
   */
  lowerBoundLabelMapping(aggregation: Aggregation): string {
    return `${this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, aggregation.groupBy.REFERENCED_MODULE_TECHNOLOGY)}`
      + ' ' + `${this.labelMappingService.mapLabel(LabelType.TYPE, aggregation.groupBy.REFERENCED_MODULE_TYPE)}`;
  }

  /**
   * Method to trigger a method when a block is checked/unchecked.
   */
  sendToParent(): void {
    this.blockSelectionEvent.emit();
  }

  /**
   * Method to trigger recalculate reachability analysis on selected blocks.
   */
  recalculateReachabilityAnalysisOnSelectedBlocks(): void {
    this.recalculateEvent.emit(this.reachabilityBlockData.uid);
  }

  /**
   * Open the drawer when user clicks view details.
   */
  openSidePanel(): void {
    const isMergedBlock = this.reachabilityBlockData.type?.includes('MERGE_PARENT');
    this.reachabilityService.storeReachabilityDetails(this.reachabilityBlockData, this.projectId, isMergedBlock);
    const blockDetails = {
      uid: this.reachabilityBlockData.uid,
      name: this.reachabilityBlockData.name,
      description: this.reachabilityBlockData.description,
      type: this.reachabilityBlockData.type,
      outdatedModule: this.reachabilityBlockData.outdatedModule
    };
    this.drawerRef = this.nzDrawerService.create({
      nzTitle: this.drawerTitleTemplate,
      nzContent: BlockViewDetailsComponent,
      nzWrapClassName: 'block-view__side-drawer',
      nzContentParams: {
        blockDetails,
        projectId: this.projectId,
        allReachabilityBlocksData: this.allReachabilityBlocksData,
        canEditRB: this.canEditRB
      },
      nzWidth: '70vw',
      nzPlacement: 'right',
      nzClosable: true,
      nzMaskClosable: false
    });
    this.drawerRef.afterClose.subscribe(() => {
      if (this.reachabilityService.getUpdateBlocks()) {
        this.siderEvent.emit(true);
        this.reachabilityService.setUpdateBlocks(false);
      }
    });
  }

  /**
   * Method to open the edit modal for name and description
   * @param blockId is the block id of the selected block
   */
  openEditModal(): void {
    const modal = this.modalService.create<EditReachabilityBlockComponent>({
      nzTitle: this.translateService.instant('reachability.editModalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzWrapClassName: 'vertical-center-modal',
      nzClassName: 'functional-analysis-tree-component__edit-window',
      nzKeyboard: true,
      nzContent: EditReachabilityBlockComponent,
      nzWidth: 600
    });
    const instance = modal.getContentComponent();
    instance.projectId = this.projectId;
    instance.reachabilityBlockUid = this.reachabilityBlockData.uid;
    modal.afterClose.subscribe((result) => {
      if (result === RESULT_EDITED) {
        this.updateBlockDetails();
      }
    });
  }

  /**
   * Method to update the status of block
   * @param status is current status of the block
   */
  updateBlock(status: 'ACTIVE' | 'INACTIVE'): void {
    this.reachabilityService.updateBlockStatus(
      this.projectId, status, [this.reachabilityBlockData.uid],
      this.translateService.instant('reachability.inactiveSuccess') as string,
      () => {
        this.drawerRef?.close();
        this.updateBlockStatus.emit(true);
      }, () => {
        this.drawerRef?.close();
      }
    );
  }
}
