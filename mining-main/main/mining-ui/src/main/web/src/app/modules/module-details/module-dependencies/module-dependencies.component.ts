import { Component, OnInit, Input } from '@angular/core';
import { Router } from '@angular/router';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { EntityId, ErrorMarker } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'mn-module-dependencies',
  templateUrl: './module-dependencies.component.html'
})
export class ModuleDependenciesComponent implements OnInit {
  @Input() projectId: number;
  @Input() moduleId: EntityId;
  @Input() moduleHash: string;
  @Input() undiscoveredDependencies: ErrorMarker[] = [];
  routeDependencies: string;

  usage = Usages.DEPENDENCYTABLE;
  graphQlType = 'moduleDependencies';
  pageType = TypeName.DEPENDENCYINFORMATION;
  additionalGraphQLParams: { [key: string]: any } = {};
  entity = 'module-dependencies';
  internalDataPoints = [
    { name: 'identification', path: 'content.target.identification' },
    { name: 'errorCount', path: 'content.errorCount' }
  ];

  constructor(protected router: Router ) {}

  ngOnInit(): void {
    this.entity = this.entity + '-' + this.moduleId;
    this.additionalGraphQLParams = {
      moduleId: this.moduleId
    };
    this.routeDependencies = RouteBuilder.buildModuleRoute(this.projectId, this.moduleHash, 'dependencies');
  }

  /**
   * Method to navigate to the error tab.
   */
  navigateToErrorTab(): void {
    void this.router.navigateByUrl(RouteBuilder.buildModuleRoute(this.projectId, this.moduleHash, 'details/errors'));
  }
}
