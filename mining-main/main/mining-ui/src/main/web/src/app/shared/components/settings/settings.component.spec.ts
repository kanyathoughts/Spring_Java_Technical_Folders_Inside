import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';

import { SettingsComponent } from './settings.component';
import { ReactiveFormsModule } from '@angular/forms';
import { DeepLinkService } from '../../../core/services/deep-link.service';
import { StateMaintainenceService } from '../../../core/services/state-maintenance/state-maintainence.service';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { HttpClient } from '@angular/common/http';
import { HttpService } from '@app/core/http/http.service';
import { ApiPrefixInterceptor } from '@app/core/http/api-prefix.interceptor';
import { ErrorHandlerInterceptor } from '@app/core/http/error-handler.interceptor';
import { RouterTestingModule } from '@angular/router/testing';
import { NzAlertModule } from 'ng-zorro-antd/alert';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { from } from 'rxjs';
import { Type } from '@angular/core';
import { NzModalModule, NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { CancelRequestOnNavigationInterceptor } from '@app/core/http/cancel-request-on-navigation.interceptor';
import { FeatureControllerService } from '@innowake/mining-api-angular-client';
import { SetSerializationInterceptor } from '@app/core/http/set-serialization.interceptor';

describe('SettingsComponentComponent', () => {
  let component: SettingsComponent;
  let fixture: ComponentFixture<SettingsComponent>;
  let deepLinkService: DeepLinkService;
  let stateMaintainenceService: StateMaintainenceService;
  let httpMock: HttpTestingController;

  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['create']);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [SettingsComponent],
      imports: [
        ReactiveFormsModule,
        HttpClientTestingModule,
        RouterTestingModule,
        NzAlertModule,
        TranslateModule.forRoot({}),
        NzModalModule,
        NzMessageModule,
        BrowserAnimationsModule
      ],
      providers: [
        FeatureControllerService,
        SetSerializationInterceptor,
        TranslateService,
        ApiPrefixInterceptor,
        ErrorHandlerInterceptor,
        CancelRequestOnNavigationInterceptor,
        {
          provide: HttpClient,
          useClass: HttpService
        },
        NzModalService,
        { provide: NzMessageService, useValue: messageServiceSpy},
        { provide: NzModalRef, useValue: nzModalRefSpy }
      ]
    }).compileComponents();
    httpMock = TestBed.inject(HttpTestingController as Type<HttpTestingController>);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SettingsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    stateMaintainenceService = TestBed.inject(StateMaintainenceService);
    deepLinkService = TestBed.inject(DeepLinkService);
  });

  afterEach(() => {
    component.setDeepLinksPort(8083);
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should set port', () => {
    component.setDeepLinksPort(9090);
    expect(deepLinkService.getPort()).toBe(9090);
  });

  it('should get url from deeplink service', () => {
    component.setDeepLinksPort(9090);
    expect(deepLinkService.getURL()).toBe('http://localhost:9090');
  });

  it('should check if server is up or not', () => {
    spyOn(deepLinkService.http, 'head').and.returnValue(from(Promise.resolve(true)));
    deepLinkService.heartbeat().then(resp => {
      expect(resp).toBeTrue();
    });
  });

  it('should show module in eclipse', () => {
    component.setDeepLinksPort(9090);
    deepLinkService.showModuleInEclipse({ projectId: 1, path: 'test' });
    httpMock.expectOne({ method: 'POST', url: 'http://localhost:9090' });
  });

  it('should show Annotation in eclipse', () => {
    component.setDeepLinksPort(9090);
    deepLinkService.showAnnotationIneclipse({ projectId: 1, uid: '1' });
    httpMock.expectOne({ method: 'POST', url: 'http://localhost:9090' });
  });

  it('should handle submit', () => {
    component.newPort = '8956';
    component.onSubmit();
    expect(deepLinkService.getPort()).toBe(8956);
  });

  it('should set port change flag on port update.', () => {
    component.currentPort = 8083;
    component.newPort = '8084';
    component.portChange();
    expect(component.isPortChanged).toBeTruthy();
  });
  
  it('should not set port change flag.', () => {
    component.currentPort = 8083;
    component.newPort = '8083';
    component.portChange();
    expect(component.isPortChanged).toBeFalsy();
  });
  
  it('should set invalidPort flag when invalid port is entered', () => {
    component.currentPort = 8083;
    component.newPort = '0';
    component.portChange();
    expect(component.isPortInvalid).toBeTruthy();

    component.newPort = '999999';
    component.portChange();
    expect(component.isPortInvalid).toBeTruthy();
  });
});
