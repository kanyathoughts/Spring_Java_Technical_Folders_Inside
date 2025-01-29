import { Component, Input, OnInit, Optional } from '@angular/core';
import { Aggregation, ReachabilityBlocks } from '../../utils/reachability-interface';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { TranslateService } from '@ngx-translate/core';
import { NzModalService } from 'ng-zorro-antd/modal';
import { EditReachabilityBlockComponent } from '@app/shared/components/edit-reachability-block/edit-reachability-block.component';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { RESULT_EDITED } from '@app/shared/components/base-edit-functional-block/base-edit-functional-block.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzDrawerRef } from 'ng-zorro-antd/drawer';
import { ReachabilityService } from '../../utils/reachability.service';
import { FunctionalBlockControllerService, FunctionalBlockMergeRequest } from '@innowake/mining-api-angular-client';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { last } from 'rxjs/operators';

const defaultTabName = 'reachability.blocksInMerge';
@Component({
  selector: 'block-view-details',
  templateUrl: './block-view-details.component.html'
})
export class BlockViewDetailsComponent implements OnInit {

  @Input() blockDetails: ReachabilityBlocks;
  @Input() projectId: number;
  @Input() canEditRB: boolean;
  @Input() showCheckBox: boolean;
  @Input() commonParent: string;
  @Input() allReachabilityBlocksData: ReachabilityBlocks[];

  mergedChildrenBlocks: ReachabilityBlocks[] = [];
  nonMergedBlocks: ReachabilityBlocks[] = [];
  unmergeBlocks: boolean;
  reachabilityBlocks: ReachabilityBlocks[] = [];
  saveLoader: boolean;
  isSave: boolean;
  totalElements: number;
  pageSize = 50;
  pageIndex = 0;
  mergedPageIndex = 0;
  blockTabTitle = this.translateService.instant(defaultTabName);
  showUnmerge = true;
  blockData: ReachabilityBlocks;
  isDetailsAvailable = false;
  isOutdatedModules = false;
  isLoaded = false;

  constructor(
    private labelMappingService: LabelMappingService,
    private modalService: NzModalService,
    private graphQlControllerService: GraphQlControllerService,
    public translateService: TranslateService,
    private functionalBlockControllerService: FunctionalBlockControllerService,
    private messageService: NzMessageService,
    private reachabilityService: ReachabilityService,
    private jobManagerService: JobManagerService,
    @Optional() private drawerRef: NzDrawerRef<string>
  ) { }

  ngOnInit(): void {
    this.getReachabilityDetails();
  }

  /**
   * Method to update outdated RB or remove deleted RB based on user action.
   * @param event user action.
   */
  onActionClicked(event: string): void {
    if (event === 'delete') {
      this.functionalBlockControllerService.removeFunctionalBlocksWithoutUpperBoundModule(this.projectId).subscribe((removedResult: string[]) => {
        if (removedResult) {
          this.getJobStatus(removedResult.toString());
        }
      });
    }
    if (event === 'update') {
      this.functionalBlockControllerService.recalculateOutDatedFunctionalBlocks(this.projectId).subscribe((outdatedResult: string[]) => {
        if (outdatedResult) {
          this.getJobStatus(outdatedResult.toString());
        }
      });
    }
  }

  /**
   * Method to enable merge based on commonParent.
   * @param selectedBlock flag to check if block is checked/unchecked.
   */
  checkToEnableMerge(): void {
    const selectedCount = this.getSelectedBlocks(this.unmergeBlocks ? this.mergedChildrenBlocks : this.nonMergedBlocks).length;
    const requestQuery = {
      'query': `{ 
            functionalBlocks(projectId: ${this.projectId}, filterObject: {content_type:{eq: REACHABILITY_NETWORK}}) {
              content {
                uid
              }
            }
          }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((result: { [key: string]: any }) => {
      if (result.data?.functionalBlocks?.content && result.data?.functionalBlocks?.content.length) {
        this.commonParent = result.data?.functionalBlocks?.content[0]?.uid;
        if (selectedCount >= 1 && this.commonParent) {
          this.isSave = true;
        } else {
          this.isSave = false;
        }
      }
    });
  }

  /**
   * Method to open the edit modal for name and description
   * @param blockId is the block id of the selected block
   */
  openEditModal(blockId?: string): void {
    const modal = this.modalService.create<EditReachabilityBlockComponent>({
      nzTitle: this.translateService.instant('reachability.editModalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzWrapClassName: 'vertical-center-modal',
      nzKeyboard: true,
      nzContent: EditReachabilityBlockComponent,
      nzWidth: 600
    });
    const instance = modal.getContentComponent();
    instance.projectId = this.projectId;
    instance.reachabilityBlockUid = blockId ? blockId : this.blockDetails.uid;
    modal.afterClose.subscribe((result) => {
      if (result === RESULT_EDITED) {
        this.reachabilityService.fetchReachabilityBlockDetails(this.blockDetails.uid, this.projectId)
          .subscribe(({ name, description }) => {
            this.blockDetails.name = name;
            this.blockDetails.description = description;
            this.reachabilityService.setUpdateBlocks(true);
          });
      }
    });
  }

  /**
   * Method to get the blocks contained in merged block/group.
   */
  getMergedChildrenBlocks(): void {
    this.nonMergedBlocks = [];
    this.mergedChildrenBlocks = [];
    const blockId = JSON.stringify(this.blockDetails.uid);
    const requestQuery = {
      'query': `{
        functionalBlocks(
          projectId: ${this.projectId}
          filterObject: {content_uid: {eq: ${blockId}}}
        ) {
          content {
            reachabilityData: children(filterObject: {content_type: {eq: RA_TOP_DOWN}}, size: ${this.pageSize},
              page: ${this.mergedPageIndex}) {
              content {
                uid
                name
                description
                type
                outdatedBlock
                blocksWithDeletedUB
                resolvedModuleParts {
                  module {
                    id
                    errorCount
                    dependencies(direction: OUT, filterObject: { content_identification: { eq: MISSING } }) {
                      module {
                        id
                      }
                    }
                  }
                  referencedTaxonomies {
                    id
                    name
                  }
                }
                peers(peerType:FUNCTIONAL_GROUP) {
                  totalElements
                }
                upperBound: children(filterObject: {content_type: {eq: RA_UPPER_BOUND}}) {
                  content {
                    children {
                      content {
                        generatedFrom {
                          module {
                            name
                            id
                            linkHash
                            technology
                            type
                          }
                        }
                      }
                    }
                  }
                }
                lowerBound: children(filterObject: {content_type: {eq: RA_LOWER_BOUND}}) {
                  aggregations {
                    groupBy {
                      REFERENCED_MODULE_TYPE
                      REFERENCED_MODULE_TECHNOLOGY
                    }
                    fields {
                      UID {
                        COUNT
                      }
                    }
                  }
                }
              }
              totalElements
            }
          }
        }
      }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((blocks: { [key: string]: any }) => {
      if (blocks && blocks?.data.functionalBlocks.content?.length) {
        blocks.data.functionalBlocks.content[0]?.reachabilityData.content?.forEach((rblocks: { [key: string]: any }) => {
          const mergedChilderns = this.reachabilityService.createReachabilityBlock(rblocks);
          mergedChilderns['upperBound'].push(...this.reachabilityService.createUpperBound(rblocks).upperBound);
          mergedChilderns['lowerBound'] = [{
            aggregations: this.reachabilityService.createLowerBoundAggregation(rblocks)
          }];
          mergedChilderns['resolvedModuleParts'] = rblocks.resolvedModuleParts;
          this.mergedChildrenBlocks.push(mergedChilderns);
        });
        this.totalElements = blocks.data.functionalBlocks.content[0]?.reachabilityData.totalElements || 0;
        this.showCheckBox = false;
        this.blockTabTitle = this.translateService.instant(defaultTabName);
        this.mergedChildrenBlocks = this.mergedChildrenBlocks.filter(b => b.upperBound?.length);
      } else {
        this.closeSidePanel();
      }
    });
  }


  /**
   * Method to get blocks that are not part of any merged blocks.
   */
  getNonMergedBlocks(): void {
    this.blockTabTitle = this.translateService.instant('reachability.checkBlocksToMerge');
    this.showCheckBox = true;
    this.unmergeBlocks = false;
    this.showUnmerge = false;
    this.isLoaded = false;
    this.mergedChildrenBlocks = [];
    this.reachabilityService.getReachabilityBlocks(this.projectId, this.pageSize, this.pageIndex,
      '', {}, this.blockDetails.uid)
      .subscribe((blocks: ReachabilityBlocks[]) => {
        this.isLoaded = true;
        if (blocks && blocks.length) {
          this.nonMergedBlocks = blocks;
          this.totalElements = this.reachabilityService.totalElements;
        } else {
          this.blockTabTitle = this.translateService.instant('reachability.noBlocksToMerge');
        }
      });
  }

  /**
   * Method to check unmerge/further merge and send the requestParams.
   */
  mergeUnmergeBlocks(unmergeBlocks?: string[]): void {
    const blocks = this.unmergeBlocks ? this.mergedChildrenBlocks : this.nonMergedBlocks;
    const selectedBlocks = unmergeBlocks?.length ? unmergeBlocks : this.getSelectedBlocks(blocks).map(u => u.uid);
    this.saveLoader = true;
    this.isSave = false;
    if (this.commonParent) {
      const requestParams: FunctionalBlockMergeRequest = {
        commonParent: this.commonParent,
        mergeParent: this.blockDetails.uid,
        mergeChildren: selectedBlocks,
        removeEmptyBlocks: true
      };
      if (this.unmergeBlocks) {
        this.unmergeReachabilityBlock(requestParams);
      } else {
        this.mergeSelectedBlocks(requestParams);
      }
      this.reachabilityService.setUpdateBlocks(true);
      this.showUnmerge = true;
    }
  }

  /**
   * Merge the blocks that are selected into merged group.
   */
  mergeSelectedBlocks(requestParams: FunctionalBlockMergeRequest): void {
    this.functionalBlockControllerService.mergeFunctionalBlock(this.projectId, requestParams).subscribe(() => {
      const successContent: string = this.translateService.instant('reachability.mergedSuccessfully');
      this.messageService.success(successContent);
      this.saveLoader = false;
      this.showCheckBox = false;
      this.getMergedChildrenBlocks();
      this.getReachabilityBlocks();
    }, () => {
      const errorContent: string = this.translateService.instant('reachability.mergeBlockError');
      this.messageService.error(errorContent);
      this.saveLoader = false;
      this.showCheckBox = false;
      this.getMergedChildrenBlocks();
      this.getReachabilityBlocks();
    });
  }

  /**
   * Method to remove the merged blocks.
   */
  removeRblocks(): void {
    this.blockTabTitle = this.translateService.instant('reachability.blocksToUnmerge');
    this.showCheckBox = true;
    this.unmergeBlocks = true;
    this.showUnmerge = false;
  }

  /**
   * Method to unmerge all the merged blocks.
   */
  unmergeAll(): void {
    this.modalService.confirm({
      nzTitle: this.translateService.instant('reachability.confirmModalTitle'),
      nzContent: this.translateService.instant('reachability.confirmModalContent'),
      nzOkText: this.translateService.instant('reachability.unmerge'),
      nzOkType: 'primary',
      nzOkDanger: true,
      nzOnOk: () => this.unmergeReachabilityBlocks(true),
      nzCancelText: this.translateService.instant('btnLabel.cancel'),
    });
  }

  /**
   * Method to unmerge the selected merged blocks.
   */
  unmergeReachabilityBlock(requestParams: FunctionalBlockMergeRequest): void {
    this.functionalBlockControllerService.unmergeFunctionalBlock(this.projectId, requestParams).subscribe(() => {
      const successContent: string = this.translateService.instant('reachability.unmergedSuccessfully');
      this.messageService.success(successContent);
      this.showCheckBox = false;
      this.saveLoader = false;
      this.getMergedChildrenBlocks();
    }, () => {
      const errorContent: string = this.translateService.instant('reachability.unmergeBlockError');
      this.messageService.error(errorContent);
      this.showCheckBox = false;
      this.saveLoader = false;
      this.getMergedChildrenBlocks();
    });
  }

  /**
   * Method to close side drawer
   */
  closeSidePanel(): void {
    this.drawerRef.close();
    this.unmergeBlocks = false;
    if (this.getSelectedBlocks(this.allReachabilityBlocksData).length > 1) {
      this.allReachabilityBlocksData = this.allReachabilityBlocksData.map(b => ({ ...b, isSelected: false }));
    }
    this.isSave = false;
    this.showCheckBox = false;
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
   * Method for the Pagination
   * @param current is the current page number.
   */
  paginate(current: number): void {
    this.pageIndex = current ? current - 1 : this.pageIndex;
    this.getNonMergedBlocks();
  }

  /**
   * Method for pagination of merged children blocks.
   * @param current is the current page number.
   */
  paginateOnMergedChildren(current: number): void {
    this.mergedPageIndex = current ? current - 1 : this.mergedPageIndex;
    this.getMergedChildrenBlocks();
  }

  unmergeReachabilityBlocks(unmergeAll?: boolean): void {
    this.unmergeBlocks = true;
    this.reachabilityService.setUpdateBlocks(true);
    this.reachabilityService.checkToEnableMerge(this.projectId).subscribe((result: string) => {
      this.commonParent = result;
      if (unmergeAll) {
        const blockId = JSON.stringify(this.blockDetails.uid);
        this.reachabilityService.getMergedBlocksId(this.projectId, blockId).subscribe((blocksId: string[]) => {
          this.mergeUnmergeBlocks(blocksId);
        });
      }
    });
  }

  /**
   * Opens details tab in view details.
   */
  openDetailsTab(): void {
    this.showCheckBox = false;
    this.showUnmerge = true;
    this.blockTabTitle = this.translateService.instant(defaultTabName);
    if (this.reachabilityService.getUpdateBlocks()) {
      this.isDetailsAvailable = false;
      this.getReachabilityDetails();
    }
  }

  enableBlocksTab(): boolean {
    let blocks = false;
    if (this.blockDetails?.type?.includes('MERGE_PARENT')) {
      blocks = true;
    }
    return blocks;
  }

  private getReachabilityDetails(): void {
    const blockId = JSON.stringify(this.blockDetails.uid);
    this.isLoaded = false;
    this.reachabilityService.getReachabilityBlockDetails(this.projectId, blockId).subscribe((blockDetails: ReachabilityBlocks) => {
      if (blockDetails) {
        this.isLoaded = true;
        this.blockData = blockDetails;
        if (this.blockData.resolvedModuleParts) {
          this.reachabilityService.createLocAndComplexity(this.blockData, this.projectId);
        }
        this.isDetailsAvailable = true;
      }
    });
  }

  private getSelectedBlocks(blocks: ReachabilityBlocks[]): ReachabilityBlocks[] {
    return blocks.filter(b => b.isSelected);
  }

  private getReachabilityBlocks(): void {
    this.reachabilityService.getReachabilityBlocks(this.projectId, this.pageSize, this.pageIndex)
      .subscribe((blocks: ReachabilityBlocks[]) => {
        if (blocks && blocks.length) {
          this.totalElements = this.reachabilityService.totalElements;
          this.allReachabilityBlocksData = blocks;
        }
      }, () => {
      });
  }

  private getJobStatus(jobId: string): void {
    const remoteJob = {
      jobId: jobId as unknown as string,
      autoDownloadResult: false,
      foreground: true,
      cancellable: true
    };
    this.jobManagerService.register(remoteJob).status$.pipe(last()).subscribe(() => {
      this.reachabilityService.setNotifyAlertBannerForOutdatedOrRemovedRb(true);
      if (this.mergedChildrenBlocks.length) {
        this.getMergedChildrenBlocks();
      } else if (this.nonMergedBlocks.length) {
        this.getNonMergedBlocks();
      }
    });
  }
}
