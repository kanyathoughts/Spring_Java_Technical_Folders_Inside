import { Component, Input, OnInit } from '@angular/core';
import { UntypedFormArray, UntypedFormBuilder, UntypedFormControl, UntypedFormGroup, Validators } from '@angular/forms';
import { BaseFormModalComponent } from '@app/shared/components/base-form-modal/base-form-modal.component';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { ClientProjectModal } from './client-project-modal.interface';
import { ClientPojo, Member, MemberControllerService, PageMember, ProjectPojo, ProjectRole } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'mn-member-form-modal',
  templateUrl: './member-form-modal.component.html'
})
export class MemberFormModalComponent extends BaseFormModalComponent implements OnInit {
  @Input() project: ProjectPojo;
  @Input() clientName: string;
  @Input() client: ClientPojo;

  helpUrl = `http://appmod-documentation.deloitte.com/innowake-documentation/trunk/mining/mining-manual/
  configuration/#25-Identity-and-Access-Management-IAM-with-Keycloak`;
  clientProjectModal: ClientProjectModal;
  userRolesOptions: Array<{ value: string, label: string }> = [];
  projNatureOptions: Array<{ value: string, label: string }> = [];
  members: Member[];
  userInfo: UntypedFormGroup;
  modelUpdate: boolean;
  listOfProjNatureSelected: string[] = ['defNature'];
  defaultProjNatureOption: Array<{ value: string, label: string }> = [{ 'value': 'defNature', 'label': 'Default Project Nature' }];
  // Flag to de-activate the Project Nature feature until the Licensing feature is done
  hideNature: boolean;
  isClientAdmin: boolean;
  hasBeenUpdated = false;

  constructor(
    public modal: NzModalRef,
    public messageService: NzMessageService,
    private memberService: MemberControllerService,
    private translateService: TranslateService,
    private fb: UntypedFormBuilder,
  ) {
    super(modal, messageService);
  }

  ngOnInit(): void {
    this.userInfo = this.fb.group({
      userEmail: ['', [Validators.email, Validators.required, this.memberAlreadyAddedValidation]],
      userRole: ['', [Validators.required]],
      projNature: [[]]
    });
    if (this.client != null) {
      this.isClientAdmin = true;
      this.userInfo.controls.userRole.setValue(ProjectRole.UserRoleEnum.ADMIN);
      this.memberService.findClientAdmins(this.client.id).subscribe((pageMember: PageMember) => {
        this.members = pageMember.content;
      });
    } else {
      this.memberService.findMembersForProject(this.project.id).subscribe((pageMember: PageMember) => {
        this.members = pageMember.content;
        this.members.forEach(mem => {
          this.updateCurrentProjRoleOnTop(mem);
        });
      });
      this.prepareProjectRolesOptions();
      this.prepareProjectNatureOptions();
    }
    this.setModalDetails();
  }

  /**
   * Updates current  project role as the first index to display in the UI List.
   *
   * @param mem Member whose roles to be swapped
   */
  updateCurrentProjRoleOnTop(mem: Member): void {
    const currentProjectRoleIndex = (mem.projectRoles )
      .findIndex(projectRole => projectRole.projectId === this.project.id);
    if (currentProjectRoleIndex >= 1) {
      const tempRole = mem.projectRoles[0];
      mem.projectRoles[0] = mem.projectRoles[currentProjectRoleIndex];
      mem.projectRoles[currentProjectRoleIndex] = tempRole;
    }
  }

  /**
   * Prepares the Project role dropdown options for Members
   */
  prepareProjectRolesOptions(): void {
    Object.keys(ProjectRole.UserRoleEnum).forEach(key => {
      const puserRoles = ProjectRole.UserRoleEnum[key];
      if (puserRoles !== ProjectRole.UserRoleEnum.ADMIN) {
        this.userRolesOptions.push({
          'value': key, 'label': puserRoles.charAt(0) + puserRoles.substring(1).toLowerCase()
        });
      }
    });
  }

  /**
   * Prepares the Project nature dropdown options for Members
   */
  prepareProjectNatureOptions(): void {
    Object.keys(ProjectRole.ProjectNaturesEnum).forEach(key => {
      const projNature = ProjectRole.ProjectNaturesEnum[key];
      this.projNatureOptions.push({
        'value': projNature,
        'label': projNature.charAt(0) + projNature.substring(1).toLowerCase().replace('_', '-')
      });
    });
  }

  /**
   * Adds the members - Makes back-end call and updates the Member list and resets the Add input section
   */
  addMember(): void {
    const projNature = this.userInfo.controls.projNature as UntypedFormArray;
    const projRole: ProjectRole = {
      projectId: this.project ? this.project.id : null,
      projectNatures: projNature.value,
      userRole: this.userInfo.controls.userRole.value
    };
    if (projNature.value.includes('defNature')) {
      projRole.projectNatures = [];
    }
    const memberToAdd: Member = {
      email: this.userInfo.controls.userEmail.value,
      projectRoles: [projRole]
    };
    let membersRequest;
    if (this.isClientAdmin) {
      membersRequest = this.memberService.addMemberAsClientAdmin(this.client.id, memberToAdd);
    } else {
      membersRequest = this.memberService.addMemberToProject(this.project.id, memberToAdd);
    }
    membersRequest.subscribe((member: Member) => {
      this.hasBeenUpdated = true;
      if (!this.isClientAdmin) {
        this.updateCurrentProjRoleOnTop(member);
        this.userInfo.controls.userRole.reset();
        this.listOfProjNatureSelected = ['defNature'];
      }
      this.userInfo.controls.userEmail.reset();
      const memberIndex = this.members.findIndex((mem: Member) => mem.id === member.id);
      memberIndex === -1 ? this.members.push(member) : this.members[memberIndex] = member;
      this.messageService.create('success', `${this.translateService.instant('updateProjectMember.successAdd')}`);
    }, () => {
      this.messageService.create('error', `${this.translateService.instant('updateProjectMember.errorAdd')}`);
    });
  }

  /**
   * Updates project roles and natures for the members.
   *
   * @param member Member whose project role/nature to be updated
   * @param openDropdown flag to specify the dropdown open/close status
   */
  updateProjectRoleNature(member: Member, openDropdown: boolean): void {
    if (!openDropdown && this.modelUpdate) {
      this.memberService.assignProjectRoleToMember(this.project.id, member.id, member).subscribe(() => {
        this.hasBeenUpdated = true;
        this.messageService.create('success', `${this.translateService.instant('updateProjectMember.successRoleProjUpdate')}`);
      }, () => {
        this.messageService.create('error', `${this.translateService.instant('updateProjectMember.errorUpdate')}`);
      });
      this.modelUpdate = false;
    }
  }

  /**
   * Removes the members and updates the list.
   *
   * @param memberIndex Index of the member to delete
   * @param member Member to be removed from the project
   */
  removeMember(memberIndex: number, member: Member): void {
    let membersRequest;
    if (this.isClientAdmin) {
      membersRequest = this.memberService.deleteMemberAsClientAdmin(this.client.id, member.id);
    } else {
      membersRequest = this.memberService.deleteMemberFromProject(this.project.id, member.id);
    }
    membersRequest.subscribe(() => {
      this.hasBeenUpdated = true;
      this.members.splice(memberIndex, 1);
      this.messageService.create('success', `${this.translateService.instant('updateProjectMember.successDelete')}`);
    }, () => {
      this.messageService.create('error', `${this.translateService.instant('updateProjectMember.errorDelete')}`);
    });
  }

  /**
   * Resets the nature dropdown on default selection
   */
  OnProjNatureChange(): void {
    const projNature = this.userInfo.controls.projNature as UntypedFormArray;
    if (projNature.value.includes('defNature')) {
      this.listOfProjNatureSelected = ['defNature'];
    } else {
      this.listOfProjNatureSelected = projNature.value;
    }
  }

  /**
   * TrackBy function which enable
   * the Angular feature that tracks elements when they are added or removed from the array for performance reasons.
   *
   * @param index Index of the member
   * @param member Member object
   * @returns id of the member
   */
  trackByFn(index: number, member: Member): string {
    return member.id;
  }

  /**
   * Custom validator to check if the member is already present or not using the given e-mail id
   */
  memberAlreadyAddedValidation = (control: UntypedFormControl): { [s: string]: boolean } => {
    if (this.members && this.members.findIndex((mem: Member) => mem.email === control.value) !== -1) {
      return { isUsrAlreadyAdded: true };
    }
  };

  /**
   * setModalDetails function sets the details of clients or project modal on the basis of clientAdmin validation.
   */
  private setModalDetails(): void {
    if (this.isClientAdmin) {
      this.clientProjectModal = {
        addMemberLabel: this.translateService.instant('updateClientAdmins.addMember'),
        addMemberPlaceholder: this.translateService.instant('updateClientAdmins.emailPlaceholder'),
        dividerText: this.translateService.instant('updateClientAdmins.clientAdministrator')
      };
    } else {
      this.clientProjectModal = {
        addMemberLabel: this.translateService.instant('updateProjectMember.addMember'),
        addMemberPlaceholder: this.translateService.instant('updateProjectMember.emailPlaceholder'),
        dividerText: this.translateService.instant('updateProjectMember.projectMembers')
      };
    }
  }
}
