import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';

import { ModalWindowComponent } from './modal-window.component';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { ModalWindowParams, FormTypes } from '../../models/modal-window-params.model';
import { of, Subject, throwError } from 'rxjs';
import { CustomDynamicDialogConfig } from '../../models/custom-dynamic-dialog-config.model';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { AnnotationPojo, FeatureControllerService, ModuleControllerService } from '@innowake/mining-api-angular-client';
describe('ModalWindowComponent', () => {
  let component: ModalWindowComponent;
  let fixture: ComponentFixture<ModalWindowComponent>;

  const forReviewState = {};
  forReviewState[`previousState`] = AnnotationPojo.StateEnum.IN_ANALYSIS;
  forReviewState[`currentState`] = AnnotationPojo.StateEnum.FOR_REVIEW;
  forReviewState[`nextState`] = [AnnotationPojo.StateEnum.APPROVED, AnnotationPojo.StateEnum.INVALID];

  const editCodeAnnotationParam: ModalWindowParams[] = [
    {
      key: 'Description',
      id: 1,
      value: 'name',
      type: FormTypes.TEXT
    },
    {
      key: 'State',
      id: 2,
      type: FormTypes.COMBO_BOX,
      value: forReviewState
    }
  ];
  let saveFlag = false;
  let deleteFlag = false;
  let openInEclipseFlag = false;
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create', 'closeAll']);
  const featureToggleServiceSpy: jasmine.SpyObj<FeatureToggleService> = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);

  const dynamicDialogConfigSpy: CustomDynamicDialogConfig = {
    data: editCodeAnnotationParam,
    saveCallBack: () => {
      saveFlag = true;
    },
    deleteCallBack: () => {
      deleteFlag = true;
    },
    openInEclipseCallback: () => {
      openInEclipseFlag = true;
    }
  };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ModalWindowComponent],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        NzMessageModule,
        HttpClientTestingModule,
        TranslateModule.forRoot({}),
        RouterTestingModule
      ],
      providers: [
        { provide: NzModalRef, useValue: { destroy: () => true, close: () => true } },
        { provide: KeycloakAuthorizationService , useValue: new NoAuthorizationService() },
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: FeatureToggleService, usevalue: featureToggleServiceSpy },
        ModuleControllerService,
        TranslateService,
        FeatureControllerService,
        NzMessageService
      ]
    }).compileComponents();
    featureToggleServiceSpy.isActive.and.returnValue(of(false));

  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ModalWindowComponent);
    component = fixture.componentInstance;
    component.customDynamicDialogConfig = dynamicDialogConfigSpy;
    component.customDynamicDialogConfig.data = [];
    component.editDetails = [ {id: 1 ,
      key: 'key1',
      value: 'value1',
      options: [] ,
      label: 'label1',
      class: 'class1',
      deleteFeature: true,
      openInEclipseFeature: true
  } ,  {id: 1 ,
    key: 'key2',
    value: 'value2',
    options: [] ,
    label: 'label2',
    class: 'class2',
    deleteFeature: true,
    openInEclipseFeature: true
  }];
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should cancel', () => {
    component.modalForm.markAsDirty();
    component.display = false;
    component.cancel();
    expect(component.display).toBeTruthy();

    component.modalForm.markAsPristine();
    component.display = false;
    component.cancel();
    expect(component.display).toBeFalsy();
  });

  it('should close edit modal', () => {
    component.display = true;
    component.closeEditModal(true);
    expect(component.display).toBeFalsy();

    component.display = true;
    component.closeEditModal(false);
    expect(component.display).toBeFalsy();
  });

  it('should save', () => {
    component.save();
    expect(saveFlag).toBeTruthy();
    saveFlag = false;
    component.selectedValue = null;
    component.save();
    expect(saveFlag).toBeTruthy();
  });

  it('should open in Eclipse', () => {
    component.openInEclipse();
    expect(openInEclipseFlag).toBeTruthy();
    openInEclipseFlag = false;
    component.selectedValue = null;
    component.openInEclipse();
    expect(openInEclipseFlag).toBeTruthy();
  });

  it('should delete', () => {
    component.delete(false);
    component.showDeleteModal= true;
    expect(component.showDeleteModal).toBeTruthy();
    deleteFlag = false;
    component.selectedValue = null;
    component.delete(true);
    expect(deleteFlag).toBeTruthy();
  });
});
