import { Component, Input, OnChanges, SimpleChanges } from '@angular/core';
import { Router } from '@angular/router';
import { ModuleControllerService, ModulePojo } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';

@Component({
  selector: 'app-module-breadcrumb',
  templateUrl: './module-breadcrumb.component.html'
})
export class ModuleBreadcrumbComponent implements OnChanges {

  @Input() currentModule?: ModulePojo;
  @Input() subPageTitle?: string[] = [];
  @Input() breadcrumbItems?: string[];
  @Input() projectId: number;
  private parentModule: ModulePojo;

  constructor(private router: Router, private translateService: TranslateService, private moduleService: ModuleControllerService) { }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['currentModule']) {
      this.breadcrumbItems = [this.translateService.instant('projectDashboard.modules')];
      this.breadcrumbItems.push(this.currentModule.name);
      if (this.currentModule.parent) {
        this.moduleService.findModuleById(this.currentModule.projectId, this.currentModule.parent).subscribe((response: ModulePojo) => {
          this.parentModule = response;
          this.breadcrumbItems.splice(1, 0, this.parentModule.name);
        });
      }
      if (this.subPageTitle.length) {
        this.breadcrumbItems = this.breadcrumbItems.concat(this.subPageTitle);
      }
    }
  }

  /**
   * Navigate to the selected breadcrumb page.
   * @param name selected breadcrumb item.
   */
  navigate(name: string): string {
    if (name) {
      switch (name) {
        case this.translateService.instant('projectDashboard.modules'):
          return '/project-' + this.currentModule.projectId + '/modules';
        case this.currentModule?.name:
          return '/project-' + this.currentModule.projectId + '/module-' + this.currentModule.id + '/details/overview';
        case this.parentModule?.name:
          return '/project-' + this.parentModule.projectId + '/module-' + this.parentModule.id + '/details/overview';
        case this.translateService.instant('reachability.reachabilityBlocks'):
          return '/project-' + this.projectId + '/reachability/overview';
        default:
          return '/clients';
      }
    }
  }
}
