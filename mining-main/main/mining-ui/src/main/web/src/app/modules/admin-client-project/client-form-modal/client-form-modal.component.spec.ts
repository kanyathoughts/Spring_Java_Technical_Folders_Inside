import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { ClientFormModalComponent } from './client-form-modal.component';
import { NzModalModule, NzModalRef } from 'ng-zorro-antd/modal';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageService, NzMessageModule } from 'ng-zorro-antd/message';
import { of, throwError } from 'rxjs';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ClientControllerService, ClientControllerV2Service, ClientPojo } from '@innowake/mining-api-angular-client';

const clientTest: ClientPojo = {
  id: 1,
  name: 'Test Client'
};

const mockClientLogo = 'fake logo src';

describe('ClientEditModalComponent', () => {
  let component: ClientFormModalComponent;
  let fixture: ComponentFixture<ClientFormModalComponent>;
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['create']);
  const clientControllerSpy = jasmine.createSpyObj<ClientControllerService>('ClientControllerService', [
    'updateClient'
  ]);

  const clientControllerV2Spy = jasmine.createSpyObj<ClientControllerV2Service>('ClientControllerV2Service', [
    'createLogo',
    'updateLogo',
    'deleteLogo',
    'createClientV2'
  ]);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
        declarations: [
            ClientFormModalComponent
        ],
        providers: [
            { provide: ClientControllerV2Service, useValue: clientControllerV2Spy},
            { provide: ClientControllerService, useValue: clientControllerSpy },
            { provide: NzMessageService, useValue: messageServiceSpy},
            TranslateService,
            { provide: NzModalRef, useValue: nzModalRefSpy }
        ],
        imports: [
            NzModalModule,
            NzMessageModule,
            HttpClientTestingModule,
            TranslateModule.forRoot({}),
            BrowserAnimationsModule
        ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ClientFormModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    clientControllerV2Spy.createClientV2.and.returnValue(of(clientTest as any, waitForAsync));
    clientControllerSpy.updateClient.and.returnValue(of(clientTest as any, waitForAsync));
    clientControllerV2Spy.createLogo.and.returnValue(of('response' as any));
    clientControllerV2Spy.updateLogo.and.returnValue(of('response' as any));
    clientControllerV2Spy.deleteLogo.and.returnValue(of('response' as any));
    messageServiceSpy.create.and.returnValue(null);
  });

  it('should open in create mode', () => {
    expect(component).toBeTruthy();
    expect(component.clientName).toBe('');
  });

  it('should open in edit mode', () => {
    component.client = clientTest;
    component.ngOnInit();
    fixture.detectChanges();
    expect(component).toBeTruthy();
    expect(component.clientName).toBe(clientTest.name);
  });

  it('should cancel and close the modal', () => {
    component.onCancel();
    expect(nzModalRefSpy.close).toHaveBeenCalled();
  });

  it('should delete logo in form', () => {
    const parts = [new Blob(['aaaaa'], { type: 'plain/txt' })];
    const newFile = new File(parts, 'sample.text');
    const updateComponent = component.inputFile.concat(newFile);
    expect(updateComponent).toBeDefined();
    component.logoSrc = 'Mock logo sources';
    component.deleteLogo();
    expect(component.inputFile).toEqual([]);
    expect(component.logoSrc).toBe(undefined);
  });

  it('should block NgZorro component to automatically upload', () => {
    const newFile =  new Blob(['aaaaa'], { type: 'plain/txt' });
    expect(component.beforeImageUpload(newFile as any)).toBe(false);
    expect(component.inputFile.length).toBe(1);
  });

  it('should create Client', () => {
    clientControllerV2Spy.createClientV2.and.returnValue(of());
    component.clientForm.get('name').setValue('Test');
    component.onSubmit();
    expect(clientControllerV2Spy.createClientV2).toHaveBeenCalled();
  });

  it('should create Client without logo', () => {
    component.clientForm.get('name').setValue('Test');
    component.onSubmit();
    expect(clientControllerV2Spy.createClientV2).toHaveBeenCalled();
  });

  it('should create Client with logo', waitForAsync(() => {
    component.clientForm.get('name').setValue('Test');
    let blob = new Blob([''], { type: 'plain/image' });
    blob['lastModifiedDate'] = '';
    blob['name'] = 'test.png';
    component.inputFile = component.inputFile.concat(<File>blob);
    fixture.detectChanges();
    component.onSubmit();
    expect(clientControllerV2Spy.createClientV2).toHaveBeenCalled();
    fixture.whenStable().then(() => {
      fixture.detectChanges();
      expect(clientControllerV2Spy.createLogo).toHaveBeenCalled();
    });
  }));

  it('should update client without logo', () => {
    component.client = clientTest;
    component.ngOnInit();
    fixture.detectChanges();
    component.clientForm.get('name').setValue('Test update');
    component.onSubmit();
    expect(clientControllerSpy.updateClient).toHaveBeenCalled();
  });

  it('should update client and logo', waitForAsync(() => {
    component.client = clientTest;
    component.clientLogo = mockClientLogo;
    component.ngOnInit();
    let blob = new Blob([''], { type: 'plain/image' });
    blob['lastModifiedDate'] = '';
    blob['name'] = 'test.png';
    component.inputFile = component.inputFile.concat(<File>blob);
    fixture.detectChanges();
    component.onSubmit();
    fixture.whenStable().then(() => {
      fixture.detectChanges();
      expect(clientControllerV2Spy.updateLogo).toHaveBeenCalled();
    });
  }));

  it('should update client and create logo', waitForAsync(() => {
    component.client = clientTest;
    component.ngOnInit();
    let blob = new Blob([''], { type: 'plain/image' });
    blob['lastModifiedDate'] = '';
    blob['name'] = 'test.png';
    component.inputFile = component.inputFile.concat(<File>blob);
    fixture.detectChanges();
    component.onSubmit();
    fixture.whenStable().then(() => {
      fixture.detectChanges();
      expect(clientControllerV2Spy.createLogo).toHaveBeenCalled();
    });
  }));

  it('should update client and delete logo', waitForAsync(() => {
    component.client = clientTest;
    component.clientLogo = mockClientLogo;
    component.ngOnInit();
    component.deleteLogo();
    fixture.detectChanges();
    component.onSubmit();
    fixture.whenStable().then(() => {
      fixture.detectChanges();
      expect(clientControllerV2Spy.deleteLogo).toHaveBeenCalled();
    });
  }));

  it('should close modal after error on create Client', () => {
    clientControllerV2Spy.createClientV2.and.returnValue(throwError(new Error('')));
    component.clientForm.get('name').setValue('Test');
    component.onSubmit();
    expect(nzModalRefSpy.close).toHaveBeenCalled();
  });

  it('should close modal after error on create Client and logo', () => {
    clientControllerV2Spy.createLogo.and.returnValue(throwError(new Error('')));
    component.clientForm.get('name').setValue('Test');
    let blob = new Blob([''], { type: 'plain/image' });
    blob['lastModifiedDate'] = '';
    blob['name'] = 'test.png';
    component.inputFile = component.inputFile.concat(<File>blob);
    component.onSubmit();
    expect(nzModalRefSpy.close).toHaveBeenCalled();
  });

  it('should close modal after error on update Client', () => {
    clientControllerSpy.updateClient.and.returnValue(throwError(new Error('')));
    component.client = { name: 'Test client without logo'  };
    component.ngOnInit();
    component.clientForm.get('name').setValue('Test');
    component.onSubmit();
    expect(nzModalRefSpy.close).toHaveBeenCalled();
  });

  it('should close modal after error on update Client and create Logo', () => {
    clientControllerV2Spy.createLogo.and.returnValue(throwError(new Error('')));
    component.client = { name: 'Test client without logo'  };
    component.ngOnInit();
    component.clientForm.get('name').setValue('Test');
    let blob = new Blob([''], { type: 'plain/image' });
    blob['lastModifiedDate'] = '';
    blob['name'] = 'test.png';
    component.inputFile = component.inputFile.concat(<File>blob);
    component.onSubmit();
    expect(nzModalRefSpy.close).toHaveBeenCalled();
  });

  it('should close modal after error on update Client and create Logo', () => {
    clientControllerV2Spy.updateLogo.and.returnValue(throwError(new Error('')));
    component.client = clientTest;
    component.ngOnInit();
    component.clientForm.get('name').setValue('Test');
    let blob = new Blob([''], { type: 'plain/image' });
    blob['lastModifiedDate'] = '';
    blob['name'] = 'test.png';
    component.inputFile = component.inputFile.concat(<File>blob);
    component.onSubmit();
    expect(nzModalRefSpy.close).toHaveBeenCalled();
  });

  it('should close modal after error on update Client and delete Logo', () => {
    clientControllerV2Spy.updateLogo.and.returnValue(throwError(new Error('')));
    component.client = clientTest;
    component.ngOnInit();
    component.clientForm.get('name').setValue('Test');
    component.deleteLogo();
    component.onSubmit();
    expect(nzModalRefSpy.close).toHaveBeenCalled();
  });
});
