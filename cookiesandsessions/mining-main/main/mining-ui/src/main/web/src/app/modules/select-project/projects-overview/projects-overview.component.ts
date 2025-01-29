import { Component, OnDestroy, OnInit } from '@angular/core';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ProjectFormModalComponent } from '@app/modules/admin-client-project/project-form-modal/project-form-modal.component';
import { TranslateService } from '@ngx-translate/core';
import { NzModalService } from 'ng-zorro-antd/modal';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { Subscription } from 'rxjs';
import { ProjectControllerService, ProjectPojo } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-projects-overview',
  templateUrl: './projects-overview.component.html'
})
export class ProjectsOverviewComponent implements OnInit, OnDestroy {

  clientId: number;

  clientName: string;

  projectList: ProjectPojo[];

  loading: boolean;
  private clientProjectSubscription: Subscription;
  constructor(
    public authorizationService: KeycloakAuthorizationService,
    private projectService: ProjectControllerService,
    private modal: NzModalService,
    private translateService: TranslateService,
    private clientProjectRelationship: ClientProjectRelationshipService
  ) { }

  ngOnInit(): void {
    // Resets the projectId for the select project page to fix the breadcrumb even when using browser back button.
    this.clientProjectRelationship.setProject(null);
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.clientId = response.getClientId();
      this.clientName = response.getClientName();
      this.updateProjectList();
    });
  }

  /**
   * Create a modal to add a project
   */
  openCreateModal(): void {
    const createModal = this.modal.create<ProjectFormModalComponent>({
      nzTitle: this.translateService.instant('projectForm.createModalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzContent: ProjectFormModalComponent
    });
    const instance = createModal.getContentComponent();
    instance.clientId = this.clientId;
    instance.clientName = this.clientName;
    createModal.afterClose.subscribe((result: string) => {
      this.onProjectUpdate(result);
    });
  }

  /**
   * Refresh the project list
   */
  onProjectUpdate(event: string): void {
    if ( ! event || event === 'cancel') {
      return;
    }
    this.updateProjectList();
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
   ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
  }

  private updateProjectList(): void {
    this.loading = true;
    this.projectService.findProjectsForClient1(this.clientId).subscribe((projects) => {
      this.projectList = projects.sort((p1, p2) => (p1.name > p2.name) ? 1 : (p1.name < p2.name) ? -1 : 0);
      this.loading = false;
    });
  }
}
