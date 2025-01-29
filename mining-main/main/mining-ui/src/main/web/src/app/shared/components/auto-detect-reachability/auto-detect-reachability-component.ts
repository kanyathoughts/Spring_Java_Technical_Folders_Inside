import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { ReachabilityService } from '../../../modules/reachability-ui-product-vision/utils/reachability.service';
import { forkJoin } from 'rxjs';
import { Router } from '@angular/router';

@Component({
    selector: 'auto-detect-alert',
    templateUrl: './auto-detect-reachability.component.html'
})
export class AutoDetectReachabilityComponent implements OnInit {

  @Input() projectId: number;
  @Input() canUpdateOrDeleteRB: boolean;
  @Input() isDeleteActionRequired = true;
  @Input() networkView = false;
  @Output() actionClicked = new EventEmitter<string>();
  isOutdatedModules = false;
  isDeletedMoudles = false;
  isOutdatedRbUpdated = false;
  blockId: string;
  loadAction = false;

  constructor(
    private reachabilityService: ReachabilityService,
    private router: Router,
  ) { }

  ngOnInit(): void {
    this.reachabilityService.getNotifyAlertBannerForOutdatedOrRemovedRb().subscribe(() => {
      const reachabilityDetails = JSON.parse(localStorage.getItem(`${this.projectId}-reachabilityDetails`));
      this.canUpdateOrDeleteRB = JSON.parse(localStorage.getItem(`${this.projectId}-updateRB`));
      this.blockId = reachabilityDetails?.blockId;
      forkJoin([this.reachabilityService.getCountOfDeletedModules(this.projectId, this.blockId, reachabilityDetails?.mergeParent as boolean),
      this.reachabilityService.getCountOfOutdatedBlocks(this.projectId, this.blockId)])
        .subscribe(([deletedCount, outdatedCount]) => {
          this.isDeletedMoudles = deletedCount > 0;
          this.isOutdatedModules = outdatedCount > 0;
        });
    });
  }

  /**
   * Method to update the outdated blocks or removed deleted blocks.
   * @param action user clicked action.
   */
  OutdatedUpdateOrRemoveDeletedRB(action: string): void {
    this.loadAction = true;
    if (action === 'update' && ! this.isDeleteActionRequired) {
      this.reachabilityService.setUpdateOutdatedState(true);
    }
    this.actionClicked.emit(action);
    if ( ! this.networkView) {
      void this.router.navigate(['/project-' + this.projectId + '/reachability/overview']);
    }
  }
}
