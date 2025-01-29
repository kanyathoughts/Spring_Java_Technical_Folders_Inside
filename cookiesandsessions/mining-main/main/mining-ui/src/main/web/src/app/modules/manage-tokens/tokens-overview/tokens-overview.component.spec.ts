import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, inject, TestBed } from '@angular/core/testing';
import { UntypedFormBuilder, FormsModule, ReactiveFormsModule } from '@angular/forms';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ActivatedRoute, convertToParamMap } from '@angular/router';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { DELETE_MODAL_CONFIRMED } from '@app/shared/components/confirm-delete-modal/confirm-delete-modal.component';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { of } from 'rxjs/internal/observable/of';
import { BehaviorSubject, Subject, Observable} from 'rxjs';
import { TokensOverviewComponent } from './tokens-overview.component';
import { NzMessageModule } from 'ng-zorro-antd/message';
import { AuthControllerService } from '@app/core/services/auth-controller/auth-controller.service';
import { TokenInfo } from '@app/core/services/auth-controller/tokenInfo';

export class ActivatedRouteMock {
  public paramMap = of(convertToParamMap({
    testId: 'abc123',
    anotherId: 'd31e8b48-7309-4c83-9884-4142efdf7271',
  }));
}

describe('TokensOverviewComponent', () => {
  let component: TokensOverviewComponent;
  let fixture: ComponentFixture<TokensOverviewComponent>;
  let formBuilder: UntypedFormBuilder;
  let route: ActivatedRoute;
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', [
    'success',
    'warning',
    'remove',
    'error',
    'loading',
  ]);
  const authControllerServiceSpy: jasmine.SpyObj<AuthControllerService> = jasmine.createSpyObj<AuthControllerService>('AuthControllerService', ['getToken', 'getTokens', 'deleteToken', 'updateToken', 'initOffline', 'probeToken']);
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose', 'getContentComponent']);
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
  const tokensList: TokenInfo[] = [{ bearerToken: 'string', description: 'string', id: 'string1', refreshToken: 'qwerty', subject: 'string', username: 'rnoothi' }, { bearerToken: 'string', description: 'string', id: 'string2', refreshToken: 'qwerty', subject: 'string', username: 'rnoothi' }];
  const token: TokenInfo = { bearerToken: 'string', description: 'string', id: 'string1', refreshToken: 'qwerty', subject: 'string', username: 'rnoothi' }
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [
        BrowserAnimationsModule,
        HttpClientTestingModule,
        SharedModule,
        TranslateModule.forRoot({}),
        FormsModule,
        ReactiveFormsModule,
        AntDesignImportsModule,
        NzMessageModule
      ],
      declarations: [TokensOverviewComponent],
      providers: [
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: AuthControllerService, useValue: authControllerServiceSpy },
        { provide: NzModalService, useValue: modalServiceSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            queryParamMap: of({
              has: (key: string) => key === 'token',
              get: (key: string) => 'some-token-value'
            })
          }
        },
        UntypedFormBuilder,
        TranslateService
      ]
    }).compileComponents();
    authControllerServiceSpy.getTokens.and.returnValue(of(tokensList as any));
    authControllerServiceSpy.getToken.and.returnValue(of(token as any));
    authControllerServiceSpy.deleteToken.and.returnValue(of({} as any));
    authControllerServiceSpy.updateToken.and.returnValue(of({} as any));
    authControllerServiceSpy.initOffline.and.returnValue(of('https://www.example.com' as any));
    authControllerServiceSpy.probeToken.and.returnValue(of({} as any));
    nzModalRefSpy.afterClose = new Subject();
    nzModalRefSpy.afterClose.next({});
    nzModalRefSpy.getContentComponent.and.returnValue({});
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(TokensOverviewComponent);
    component = fixture.componentInstance;
    formBuilder = TestBed.inject(UntypedFormBuilder);
    component.saveTokenModalForm = formBuilder.group({
      tokenDescription: ['']
    });
    fixture.detectChanges();
  });

  beforeEach(inject([ActivatedRoute], (activatedRoute: ActivatedRoute) => {
    route = activatedRoute;
  }));


  it('should create', () => {
    component.ngOnInit();
    expect(component).toBeTruthy();
  });

  it('should subscribe to query param map', () => {
    component.ngOnInit();
    let token: string;
    route.queryParamMap.subscribe(params => {
      if (params.has('token')) {
        token = params.get('token');
      }
    });
    expect(token).toEqual('some-token-value');
  });

  it('should loadTokens', () => {
    component.loadTokens();
    expect(component.storedTokens).toEqual(tokensList);
  });

  it('should test handleTokenOperations method of operation type probeToken', () => {
    component.handleTokenOperations(token, 'probeToken');
    expect(component.modalTokenValidated).toEqual(null);
  });

  it('should test handleTokenOperations method of operation type editToken', () => {
    component.handleTokenOperations(token, 'editToken');
    expect(component.modalTokenDescription).toBe(true);
  });

  it('should test handleTokenOperations method of operation type showToken', () => {
    component.handleTokenOperations(token, 'showToken');
    expect(component.modalToken).toEqual(token.bearerToken);
  });

  it('should openCreateModal method, on opening create modal default value should be displayed in form control', () => {
    component.openCreateModal();
    let modaldescription = component.saveTokenModalForm.controls['tokenDescription'];
    modaldescription.setValue('Unnamed token '+ new Date().toISOString());
    expect(modaldescription.valid).toBeTruthy();
  });

  it('should closeModal method, on closing modal form should be marked as Untouched', () => {
    component.closeModal();
    let modaldescription = component.saveTokenModalForm.controls['tokenDescription'];
    modaldescription.markAsUntouched();
    expect(modaldescription.touched).toBeFalsy();
    modaldescription.markAsPristine();
    expect(modaldescription.pristine).toBeTruthy();
    expect(component.modalTokenDescription).toEqual(null);
  });

  it('should submitRequest method, on submitingRequest modal should update the token and close the modal', () => {
    component.modalTokenId = 'modalTokenId';
    component.submitTokenRequest();
    let modaldescription = component.saveTokenModalForm.controls['tokenDescription'];
    modaldescription.setValue('Unnamed token '+ new Date().toISOString());
    expect(component.modalTokenDescription).toBeFalsy();
  });

  it('should open the token delete modal', () => {
    nzModalRefSpy.afterClose = new Subject<string>();
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    component.handleTokenOperations(token, 'deleteToken');
    expect(modalServiceSpy.create).toHaveBeenCalled();
  });

  it('should display success message on successful deletion on confirming on delete token modal', () => {
    nzModalRefSpy.afterClose = new BehaviorSubject<string>(DELETE_MODAL_CONFIRMED);
    modalServiceSpy.create.and.returnValue(nzModalRefSpy);
    component.handleTokenOperations(token, 'deleteToken');
    authControllerServiceSpy.deleteToken.and.returnValue(of(true as any))
    expect(component.storedTokens).toEqual(tokensList);
  });
});