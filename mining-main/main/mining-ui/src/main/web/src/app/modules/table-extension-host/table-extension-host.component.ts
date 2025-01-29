import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Params } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { Subscription } from 'rxjs';
import {
  CustomTableExtensionDescription,
  MiningDataPointDefinitionWithPath,
  MiningUiExtensionsControllerService
} from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-table-extension-host',
  templateUrl: './table-extension-host.component.html'
})
export class TableExtensionHostComponent implements OnInit, OnDestroy {

  tableDescription: CustomTableExtensionDescription;
  projectId: number;
  internalDataPoints: Array<Partial<MiningDataPointDefinitionWithPath>>;
  private pageIdentifier: string;
  private clientProjectSubscription: Subscription;
  private routeSubscription: Subscription;

  constructor(
    private miningUiExtensionService: MiningUiExtensionsControllerService,
    private route: ActivatedRoute,
    private clientProjectRelationship: ClientProjectRelationshipService) { }

  ngOnInit(): void {
    this.routeSubscription = this.route.params.subscribe((paramMap: Params) => {
      this.tableDescription = null;
      this.pageIdentifier = paramMap['pageIdentifier'];
      this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
        this.projectId = response.getProjectId();
      });
      this.miningUiExtensionService.getTableExtensions().subscribe((response: CustomTableExtensionDescription[]) => {
        this.tableDescription = response.find((extension: CustomTableExtensionDescription) => extension.identifier === this.pageIdentifier);
      });
      this.internalDataPoints = [
        /* for now, the "content.id" data point is hard-coded here - the table does not function without it */
        { name: 'id', path: 'content.id' }
      ];
    });
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.clientProjectSubscription.unsubscribe();
    this.routeSubscription.unsubscribe();
  }
}
