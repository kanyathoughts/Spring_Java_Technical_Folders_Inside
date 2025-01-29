import { Component, OnInit, Input, Output, EventEmitter, ViewChild, TemplateRef } from '@angular/core';
import { NzModalService } from 'ng-zorro-antd/modal';
import { ClientFormModalComponent } from '@app/modules/admin-client-project/client-form-modal/client-form-modal.component';
import { TranslateService } from '@ngx-translate/core';
import { Router } from '@angular/router';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ConfirmDeleteModalComponent, DELETE_MODAL_CONFIRMED } from '@app/shared/components/confirm-delete-modal/confirm-delete-modal.component';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NzMessageService } from 'ng-zorro-antd/message';
import { DomSanitizer } from '@angular/platform-browser';
import { MemberFormModalComponent } from '@app/modules/admin-client-project/member-form-modal/member-form-modal.component';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { KeycloakService } from '@app/core/authentication/keycloak.service';
import { ClientControllerV2Service, ClientPojo, ProjectControllerService, ProjectControllerV2Service, ProjectPojo }
  from '@innowake/mining-api-angular-client';

@Component({
  selector: 'mn-client-card',
  templateUrl: './client-card.component.html'
})
export class ClientCardComponent implements OnInit {

  @Input() client: ClientPojo;

  @Output() clientModified = new EventEmitter<string>();

  @ViewChild('deleteModalTemplate') deleteModalContent: TemplateRef<any>;

  clientLogo: string;

  projectList: ProjectPojo[];

  loadingProject: boolean;

  projectCount = 0;

  constructor(
    public authorizationService: KeycloakAuthorizationService,
    private projectServiceV1: ProjectControllerService,
    private projectServiceV2: ProjectControllerV2Service,
    private clientService: ClientControllerV2Service,
    private modalService: NzModalService,
    private translateService: TranslateService,
    private router: Router,
    private clientProjectRelationshipService: ClientProjectRelationshipService,
    private messageService: NzMessageService,
    private sanitizer: DomSanitizer,
    public keycloak: KeycloakService
  ) { }

  ngOnInit(): void {
    this.clientService.getLogo(this.client.id).subscribe(logo => {
      this.clientLogo = this.sanitizer.bypassSecurityTrustResourceUrl(logo.toString()) as string;
    });
    this.projectServiceV2.findProjectCount(this.client.id).subscribe(count => {
      this.projectCount = count;
    });
  }

  /**
   * Retrieve the project list if needed
   */
  getProjectList(): void {
    if (this.projectList) {
      return;
    }

    if (!this.projectCount) {
      this.projectList = [
        {
          name: this.translateService.instant('selectProject.noProjects')
        }
      ];
      return;
    }

    this.loadingProject = true;
    this.projectServiceV1.findProjectsForClient1(this.client.id).subscribe(projects => {
      this.projectList = projects;
      if (this.projectCount >= 10) {
        this.projectList.push({
          name: '...'
        });
      }
      this.loadingProject = false;
    });
  }

  /**
   * Create a modal to edit the client
   */
  onEditClient(): void {
    const modal = this.modalService.create<ClientFormModalComponent>({
      nzTitle: this.translateService.instant('clientForm.editModalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzContent: ClientFormModalComponent,
      nzAutofocus: null,
      nzAfterClose: this.clientModified
    });
    const instance = modal.getContentComponent();
    instance.client = this.client;
    instance.clientLogo = this.clientLogo;
  }

  /**
   * Create a modal to edit the client admins
   */
  onEditClientAdmins(): void {
    const modal = this.modalService.create<MemberFormModalComponent>({
      nzTitle: this.translateService.instant('updateClientAdmins.editClientAdminsHeader', {clientName: this.client.name}),
      nzClosable: true,
      nzKeyboard: true,
      nzContent: MemberFormModalComponent,
      nzWidth: 800,
      nzAutofocus: null,
      nzFooter: null,
      nzAfterClose: this.clientModified
    });
    const instance = modal.getContentComponent();
    instance.client = this.client;
  }

  /**
   * Create a modal to delete the client
   */
  onDeleteClient(): void {
    this.getProjectList();

    const deleteModal = this.modalService.create<ConfirmDeleteModalComponent>({
      nzTitle: this.translateService.instant('deleteClientModal.modalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzContent: ConfirmDeleteModalComponent
    });
    const instance = deleteModal.getContentComponent();
    instance.modalContent = this.deleteModalContent;
    instance.confirmationText = 'deleteClientModal.confirmText';
    instance.confirmationButtonText = 'btnLabel.delete';
    instance.isConfirmationReq = true;
    deleteModal.afterClose.subscribe((result) => {
      if (result === DELETE_MODAL_CONFIRMED) {
        this.clientService.deleteClient(this.client.id).subscribe(() => {
          this.clientModified.emit('delete');
          this.messageService.create('success', `${this.translateService.instant('deleteClientModal.successDelete', { clientName: this.client.name })}`);
        }, () => {
          this.messageService.create('error', `${this.translateService.instant('deleteClientModal.errorDelete')}`);
        });
      }
    });
  }

  /**
   * Handles on card click to navigate to the Select Project page
   */
  onCardClick(): void  {
    const clientProjectRelation = new ClientProjectRelationship(
      this.client.id,
      this.client.name
    );
    this.clientProjectRelationshipService.setClientProjectRelationship(clientProjectRelation);
    void this.router.navigate([RouteBuilder.buildClientRoute(this.client.id, 'projects')]);
  }
}
