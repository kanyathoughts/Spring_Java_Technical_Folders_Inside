import { Component, EventEmitter, Input, OnInit, Output, TemplateRef, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { KeycloakService } from '@app/core/authentication/keycloak.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { MemberFormModalComponent } from '@app/modules/admin-client-project/member-form-modal/member-form-modal.component';
import { ProjectFormModalComponent } from '@app/modules/admin-client-project/project-form-modal/project-form-modal.component';
import { ConfirmDeleteModalComponent, DELETE_MODAL_CONFIRMED } from '@app/shared/components/confirm-delete-modal/confirm-delete-modal.component';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { Member, MemberControllerService, PageMember, ProjectControllerV2Service, ProjectPojo, ProjectRole } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalService } from 'ng-zorro-antd/modal';

@Component({
  selector: 'mn-project-card',
  templateUrl: './project-card.component.html'
})
export class ProjectCardComponent implements OnInit {

  @Input() project: ProjectPojo;

  @Input() clientId: number;

  @Input() clientName: string;

  @Output() projectModified = new EventEmitter<string>();

  @ViewChild('deleteModal') deleteModalContent: TemplateRef<any>;

  memberCount = 0;

  loadingMembers: boolean;

  memberList: Member[];

  displayMemberList: string[];

  constructor(
    public authorizationService: KeycloakAuthorizationService,
    private memberService: MemberControllerService,
    private router: Router,
    private translateService: TranslateService,
    private modalService: NzModalService,
    private clientProjectRelationshipService: ClientProjectRelationshipService,
    private projectService: ProjectControllerV2Service,
    private messageService: NzMessageService,
    public keycloak: KeycloakService
  ) { }

  ngOnInit(): void {
    this.refreshMemberList();
  }

  /**
   * Handles click on project card to navigate to project home page
   */
  async onCardClick(): Promise<void> {
    const clientProjectRelation = new ClientProjectRelationship(
      this.clientId,
      this.clientName,
      this.project.id,
      this.project.name,
      this.project.metricsDate
    );
    this.clientProjectRelationshipService.setClientProjectRelationship(clientProjectRelation);
    await this.router.navigate([RouteBuilder.buildProjectRoute(this.project.id, 'dashboard')]);
  }

  /**
   * Retrieve the project member list
   */
  loadMembers(): void {
    if (this.memberList || this.memberCount === 0) {
      return;
    }

    this.memberService.findMembersForProject(this.project.id, 0, 9).subscribe((members: PageMember) => {
      this.memberList = members.content;
      this.displayMemberList = members.content.map((member: Member) => {
        let displayValue = this.translateService.instant('messageService.unknown');
        if (member.firstName || member.lastName) {
          displayValue = member.firstName + ' ' + member.lastName;
        } else if (member.email) {
          displayValue = member.email;
        }
        return displayValue + ' (' + this.getRoleForCurrentProject(member.projectRoles) + ')';
      });

      if (this.memberCount >= 10) {
        this.displayMemberList.push('...');
      }
    });
  }

  /**
   * Get the member's role for the current project
   * @param roles member's roles
   */
  getRoleForCurrentProject(roles: ProjectRole[]): string {
    const roleName = roles.find(role => role.projectId === this.project.id).userRole.toLowerCase();
    return roleName.charAt(0).toLocaleUpperCase() + roleName.slice(1);
  }

  /**
   * Create a modal to edit a project
   */
  onEditProject(): void {
    const modal = this.modalService.create<ProjectFormModalComponent>({
      nzTitle: this.translateService.instant('projectForm.editModalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzContent: ProjectFormModalComponent,
      nzAfterClose: this.projectModified
    });
    const instance = modal.getContentComponent();
    instance.clientId = this.clientId;
    instance.clientName = this.clientName;
    instance.project = this.project;
  }

  /**
   * Create a modal to edit project members
   */
  onEditProjectMembers(): void {
    const modal = this.modalService.create<MemberFormModalComponent>({
      nzTitle: this.translateService.instant('updateProjectMember.editProjectMembersHeader', {projectName: this.project.name}),
      nzClosable: true,
      nzKeyboard: true,
      nzMaskClosable: false,
      nzAutofocus: null,
      nzContent: MemberFormModalComponent,
      nzWidth: 800,
      nzFooter: null,
      nzOnCancel: (component) => {
        if (component.hasBeenUpdated) {
          this.refreshMemberList();
        }
      }
    });
    const instance = modal.getContentComponent();
    instance.clientName = this.clientName;
    instance.project = this.project;
  }

  /**
   * Create a modal to delete a project
   */
  onDeleteProject(): void {
    const deleteModal = this.modalService.create<ConfirmDeleteModalComponent>({
      nzTitle: this.translateService.instant('deleteProjectModal.modalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzContent: ConfirmDeleteModalComponent,
    });
    const instance = deleteModal.getContentComponent();
    instance.modalContent = this.deleteModalContent;
    instance.confirmationText = 'deleteProjectModal.confirmText';
    instance.confirmationButtonText = 'btnLabel.delete';
    instance.isConfirmationReq = true;
    deleteModal.afterClose.subscribe((result) => {
      if (result === DELETE_MODAL_CONFIRMED) {
        this.projectService.deleteProject(this.project.id).subscribe(() => {
          this.projectModified.emit('delete');
          this.messageService.create('success',
            `${this.translateService.instant('deleteProjectModal.successDelete', { projectName: this.project.name })}`);
        }, () => {
          this.messageService.create('error', `${this.translateService.instant('deleteProjectModal.errorDelete')}`);
        });
      }
    });
  }

  private refreshMemberList(): void {
    this.memberService.findMemberCountForProject(this.project.id).subscribe((result: number) => {
      this.memberCount = result;
      this.memberList = null;
    });
  }
}
