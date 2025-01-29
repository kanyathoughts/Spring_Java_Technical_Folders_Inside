import { ComponentFixture, TestBed } from '@angular/core/testing';
import { UntypedFormBuilder } from '@angular/forms';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { of } from 'rxjs';

import { CustomPropertyFormComponent } from './custom-property-form.component';
import { ModuleControllerService, ModulePojo, ProjectPojoCustomProperties } from '@innowake/mining-api-angular-client';

describe('CustomPropertyFormComponent', () => {
  let component: CustomPropertyFormComponent;
  let fixture: ComponentFixture<CustomPropertyFormComponent>;

  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService',
    ['updateModule']);

  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['create']);

  const notificationSpy: jasmine.SpyObj<NzNotificationService> = jasmine.createSpyObj<NzNotificationService>('NzNotificationService', ['create']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);
  const currentClient: ClientProjectRelationship = new ClientProjectRelationship(1, 'test', 1, 'test');

  const customProperties: { [key: string]: ProjectPojoCustomProperties[] } = {
    'ModuleCustomProperties': [{
      'name': 'newCustomProperty',
      'value': 'test[dfds',
      'dataType': 'STRING',
    }]
  }
  const moduleValue: ModulePojo = {
    uid: '#136:600',
    customProperties: {
      'ModuleCustomProperties': [{
        'name': 'newCustomProperty',
        'value': 'test',
        'dataType': 'STRING'
      }, {
        name: 'myCustomProperty',
        value: 'option1',
        dataType: 'STRING'
      }, {
        name: 'newCustomProp2',
        value: 'option2',
        dataType: 'STRING'
      }]
    },
    id: 2007,
    name: 'CC1',
    projectId: 1,
    path: 'src/cobol/programs/CC1.cpy',
    technology: 'COBOL',
    type: 'COPYBOOK',
    storage: 'FILE',
    identification: 'IDENTIFIED',
    origin: 'CUSTOM',
    info: null,
    description: 'A test copy',
    sourceMetrics: {
      codeLines: null,
      commentLines: null,
      complexityMcCabe: null,
    },
    content: null,
    sourceCodeAvailable: false
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [CustomPropertyFormComponent],
      imports: [
        TranslateModule.forRoot({}),
      ],
      providers: [
        TranslateService,
        UntypedFormBuilder,
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        { provide: NzNotificationService, useValue: notificationSpy },
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: NzModalRef, useValue: nzModalRefSpy }
      ]
    })
    .compileComponents();
    moduleControllerServiceSpy.updateModule.and.returnValue(of(moduleValue as any));
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CustomPropertyFormComponent);
    component = fixture.componentInstance;
    component.currentClient = currentClient;
    component.customProperties = customProperties;
    component.selectedModule = moduleValue;
    fixture.detectChanges();
  });

  it('should create', () => {
    component.ngOnInit();
    expect(component).toBeTruthy();
  });

  it('should test saveCustomProperties', () => {
    component.saveCustomProperties();
    expect(component.isLoading).toBeFalsy();
  });

  it('should test cancel', () => {
    spyOn(component, 'handleCancel').and.callThrough();
    component.handleCancel();
    expect(component.isLoading).toBeFalsy();
  });
});
