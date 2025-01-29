import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { UntypedFormBuilder } from '@angular/forms';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { NzModalModule, NzModalRef } from 'ng-zorro-antd/modal';
import { throwError } from 'rxjs';
import { of } from 'rxjs/internal/observable/of';

import { MemberFormModalComponent } from './member-form-modal.component';
import { Member, MemberControllerService, PageMember } from '@innowake/mining-api-angular-client';

describe('MemberFormModalComponent', () => {
    let component: MemberFormModalComponent;
    let fixture: ComponentFixture<MemberFormModalComponent>;
    const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);
    const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['create']);
    const memberControllerServiceSpy = jasmine.createSpyObj<MemberControllerService>('MemberControllerService', ['addMemberAsClientAdmin',
        'addMemberToProject', 'assignProjectRoleToMember', 'deleteMemberAsClientAdmin', 'deleteMemberFromProject', 'findMembersForProject']);
    const member: Member = {
        'email': 'abc@abc.com',
        'firstName': 'Jose',
        'id': '123',
        'lastName': 'Kohilan',
        'projectRoles': [{
            'projectId': 1,
            'projectNatures': [],
            'userRole': 'VIEWER'
        }]
    };
    const pageMember: PageMember = {
        content: []
    };
    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            declarations: [MemberFormModalComponent],
            imports: [
                TranslateModule.forRoot({}),
                NzModalModule,
                NzMessageModule,
                BrowserAnimationsModule
            ],
            providers: [
                { provide: NzMessageService, useValue: messageServiceSpy },
                { provide: NzModalRef, useValue: nzModalRefSpy },
                { provide: MemberControllerService, useValue: memberControllerServiceSpy },
                TranslateService,
                UntypedFormBuilder
            ]
        })
            .compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(MemberFormModalComponent);
        component = fixture.componentInstance;
        component.project = { id: 1, name: 'Test project' };
        component.clientName = 'Test  Client';
        pageMember.content.push(member);
        component.members = pageMember.content;
        memberControllerServiceSpy.findMembersForProject.and.returnValue(of(pageMember as any));
        memberControllerServiceSpy.addMemberToProject.and.returnValue(of(member as any));
        memberControllerServiceSpy.assignProjectRoleToMember.and.returnValue(of('result' as any, waitForAsync));
        memberControllerServiceSpy.deleteMemberFromProject.and.returnValue(of('result' as any, waitForAsync));
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('add Member to the project', () => {
        component.addMember();
        expect(memberControllerServiceSpy.addMemberToProject).toHaveBeenCalled();
    });

    it('add Member to the project with default nature', () => {
        component.userInfo.controls.projNature.setValue('defNature');
        component.addMember();
        expect(memberControllerServiceSpy.addMemberToProject).toHaveBeenCalled();
    });

    it('update project role and natures for the member', () => {
        component.modelUpdate = true;
        component.updateProjectRoleNature(member, false);
        expect(memberControllerServiceSpy.assignProjectRoleToMember).toHaveBeenCalled();
    });
    it('delete the member', () => {
        component.removeMember(1, member);
        expect(memberControllerServiceSpy.deleteMemberFromProject).toHaveBeenCalled();
    });
    it('reset nature dropdown on default selection', () => {
        component.OnProjNatureChange();
        expect(component.listOfProjNatureSelected).toEqual([]);
    });
    it('reset nature dropdown to actual  value', () => {
        component.userInfo.controls.projNature.setValue('defNature');
        component.OnProjNatureChange();
        expect(component.listOfProjNatureSelected).toEqual(['defNature']);
    });
    it('delete member error call back', () => {
        memberControllerServiceSpy.deleteMemberFromProject.and.returnValue(throwError('Delete error'));
        component.removeMember(1, member);
        expect(memberControllerServiceSpy.deleteMemberFromProject).toHaveBeenCalled();
    });
    it('add member error call back', () => {
        memberControllerServiceSpy.addMemberToProject.and.returnValue(throwError('Add error'));
        component.addMember();
        expect(memberControllerServiceSpy.addMemberToProject).toHaveBeenCalled();
    });
    it('update project role/nature error call back', () => {
        memberControllerServiceSpy.assignProjectRoleToMember.and.returnValue(throwError('Update error'));
        component.modelUpdate = true;
        component.updateProjectRoleNature(member, false);
        expect(memberControllerServiceSpy.assignProjectRoleToMember).toHaveBeenCalled();
    });
});
