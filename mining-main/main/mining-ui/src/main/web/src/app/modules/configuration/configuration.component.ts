import { OnDestroy } from '@angular/core';
import { Component, OnInit } from '@angular/core';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { Subscription } from 'rxjs';

@Component({
  selector: 'app-configuration',
  templateUrl: './configuration.component.html'
})

export class ConfigurationComponent implements OnInit, OnDestroy {
  projectId: number;
  currentClient: ClientProjectRelationship;
  routeAnnotations: string;
  routeTaxonomies: string;
  routeSchema: string;
  routePropagation: string;
  routeModules: string;
  routeDataDictionary: string;
  routeMetadata: string;
  routeSchedulerData: string;
  routeReachability: string;
  routeView: string;
  showTaxonomyPropagation = false;
  clientProjectData: ClientProjectRelationship;
  private clientProjectSubscription: Subscription;

  constructor(
    private clientProjectRelationship: ClientProjectRelationshipService,
    private authorizationService: KeycloakAuthorizationService) {
      this.showTaxonomyPropagation = this.authorizationService.isClientAdmin();
  }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.clientProjectData = response;
      this.projectId = response.getProjectId();
      this.currentClient = response;
      this.routeAnnotations = RouteBuilder.buildProjectRoute(this.projectId, 'configuration/annotations');
      this.routeTaxonomies = RouteBuilder.buildProjectRoute(this.projectId, 'configuration/taxonomies');
      this.routeSchema = RouteBuilder.buildProjectRoute(this.projectId, 'configuration/schema');
      this.routePropagation = RouteBuilder.buildProjectRoute(this.projectId, 'configuration/taxonomy-propagation');
      this.routeModules = RouteBuilder.buildProjectRoute(this.projectId, 'configuration/modules');
      this.routeDataDictionary = RouteBuilder.buildProjectRoute(this.projectId,'configuration/data-dictionary');
      this.routeMetadata = RouteBuilder.buildProjectRoute(this.projectId, 'configuration/metadata');
      this.routeSchedulerData = RouteBuilder.buildProjectRoute(this.projectId, 'configuration/scheduler-data');
      this.routeReachability = RouteBuilder.buildProjectRoute(this.projectId, 'configuration/reachability');
      this.routeView = RouteBuilder.buildProjectRoute(this.projectId, 'configuration/view');
    });
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription.unsubscribe();
  }
}
