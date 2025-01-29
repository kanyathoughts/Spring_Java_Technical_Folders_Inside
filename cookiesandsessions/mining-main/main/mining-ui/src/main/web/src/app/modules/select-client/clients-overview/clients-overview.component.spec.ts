import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';

import { ClientsOverviewComponent } from './clients-overview.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule } from '@ngx-translate/core';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { NzMessageModule } from 'ng-zorro-antd/message';
import { of } from 'rxjs/internal/observable/of';
import { AdminClientProjectModule } from '@app/modules/admin-client-project/admin-client-project.module';
import { BehaviorSubject, Subject } from 'rxjs';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { ClientControllerV2Service, PagedClientPojo } from '@innowake/mining-api-angular-client';

describe('ClientsOverviewComponent', () => {
  let component: ClientsOverviewComponent;
  let fixture: ComponentFixture<ClientsOverviewComponent>;
  const clientServiceSpy = jasmine.createSpyObj<ClientControllerV2Service>('ClientControllerV2Service', ['getAllClients']);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose']);
  const mockClientPage: PagedClientPojo = {
    content: [
      {name: 'Test Client 1'},
      {name: 'Test Client 2'}
    ]
  };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ ClientsOverviewComponent ],
      providers: [
        { provide: ClientControllerV2Service, useValue: clientServiceSpy },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() }
      ],
      imports: [
        AdminClientProjectModule,
        HttpClientTestingModule,
        NzMessageModule,
        TranslateModule.forRoot({})
      ]
    })
    .compileComponents();
    clientServiceSpy.getAllClients.and.returnValue(of(mockClientPage as any));
    nzModalRefSpy.afterClose = new Subject;
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ClientsOverviewComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create and get all client on init', waitForAsync(() => {
    expect(component).toBeTruthy();
    expect(clientServiceSpy.getAllClients).toHaveBeenCalled();
    fixture.whenStable().then(() => {
      expect(component.clientList.length).toBe(2);
    });
  }));

  it('should open the client form modal', () => {
    component.openCreateModal();
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should not update client list when the update is canceled', () => {
    clientServiceSpy.getAllClients.calls.reset();
    component.onClientUpdate('cancel');
    expect(clientServiceSpy.getAllClients).not.toHaveBeenCalled();
  });

  it('should update client list when the update is successful', () => {
    nzModalRefSpy.afterClose = new BehaviorSubject('success');
    (component as any).setSubscriptionForClientUpdate(nzModalRefSpy);
    expect(clientServiceSpy.getAllClients).toHaveBeenCalled();
  });
});
