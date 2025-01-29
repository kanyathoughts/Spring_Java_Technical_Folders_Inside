import { Component, OnInit, ViewChild } from '@angular/core';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { BlockViewComponent } from './block-view/block-view.component';
import { Router } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { Subscription } from 'rxjs';
import { AnalysisModalComponent } from './analysis-modal/analysis-modal.component';

@Component({
  selector: 'reachability-ui',
  templateUrl: './reachability-ui-product-vision.component.html'
})
export class ReachabilityUiProductVisionComponent implements OnInit {

  @ViewChild(BlockViewComponent) blockViewComponent: BlockViewComponent;
  @ViewChild(AnalysisModalComponent) modalComponent: AnalysisModalComponent;

  projectId: number;
  clientProjectRelationship: ClientProjectRelationship;

  blockView = false;
  selectionView = true;

  collapsed = true;

  routeEnd: string;
  clientProjectSubscription: Subscription;
  taxonomyIds: string[] = [];

  constructor(
    private clientProjectRelationshipService: ClientProjectRelationshipService,
    protected router: Router
    ) {
      this.routeEnd = router.url.split('?')[0].split('/').pop();
      switch(this.routeEnd) {
        case 'selection':
          this.blockView = false;
          this.selectionView = true;
        break;
        case 'overview':
          this.blockView = true;
          this.selectionView = false;
        break;
      }

  }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationshipService.getClientProjectObservable()
      .subscribe((response: ClientProjectRelationship) => {
        if (response) {
          this.clientProjectRelationship = response;
          this.projectId = this.clientProjectRelationship.getProjectId();
        }
      });
  }

  public openBlockView(): void {
    void this.router.navigateByUrl('/project-' + this.projectId + '/reachability/overview');
  }
}
