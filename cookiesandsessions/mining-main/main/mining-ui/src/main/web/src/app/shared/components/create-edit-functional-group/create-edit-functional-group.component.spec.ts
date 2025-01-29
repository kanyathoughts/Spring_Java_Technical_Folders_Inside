import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { CreateEditFunctionalGroupComponent } from './create-edit-functional-group.component';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { TranslateModule } from '@ngx-translate/core';
import { of } from 'rxjs';
import { NzMessageService } from 'ng-zorro-antd/message';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { AnnotationControllerService, AnnotationToFunctionalBlockControllerService, FunctionalBlockControllerService } from '@innowake/mining-api-angular-client';

describe('CreateEditFunctionalGroupComponent', () => {
  let component: CreateEditFunctionalGroupComponent;
  let fixture: ComponentFixture<CreateEditFunctionalGroupComponent>;

  const annotationFunctionalController: jasmine.SpyObj<AnnotationToFunctionalBlockControllerService> = jasmine.createSpyObj('AnnotationToFunctionalBlockControllerService', [
    'getFunctionalUnitsForAnnotations'
  ]);
  const functionalBlockController: jasmine.SpyObj<FunctionalBlockControllerService> = jasmine.createSpyObj('FunctionalBlockControllerService', [
    'createFunctionalBlock', 'deleteFunctionalBlock', 'deleteAutomatedFunctionalBlock'
  ]);
  const nzMessageServiceStub: jasmine.SpyObj<NzMessageService> = jasmine.createSpyObj<NzMessageService>
    ('NzMessageService', ['create','success', 'error']); 
  const graphQlControllerService: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj('GraphQlControllerService', [
    'graphQl'
  ]);
  const featureToggleServiceSpy: jasmine.SpyObj<FeatureToggleService> = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);
  const annotationControllerService: jasmine.SpyObj<AnnotationControllerService> = jasmine.createSpyObj<AnnotationControllerService>('AnnotationControllerService', ['generateGroupDescription']);
  
  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [CreateEditFunctionalGroupComponent],
      imports: [ReactiveFormsModule, TranslateModule.forRoot({}),],
      providers: [
        { provide: NzModalRef, useValue: jasmine.createSpyObj('NzModalRef', ['destroy'])},
        { provide: FunctionalBlockControllerService, useValue: functionalBlockController },
        { provide: NzMessageService, useValue: nzMessageServiceStub },
        { provide: AnnotationToFunctionalBlockControllerService, useValue: annotationFunctionalController },
        { provide: GraphQlControllerService, useValue: graphQlControllerService },
        { provide: FeatureToggleService, useValue: featureToggleServiceSpy },
        { provide: AnnotationControllerService, useValue: annotationControllerService }
      ]
    });

    fixture = TestBed.createComponent(CreateEditFunctionalGroupComponent);
    component = fixture.componentInstance;
    featureToggleServiceSpy.isActive.and.returnValue(of(true) as any);
    functionalBlockController.createFunctionalBlock.and.returnValues(of({} as any));
    functionalBlockController.deleteAutomatedFunctionalBlock.and.returnValues(of({} as any));
    annotationFunctionalController.getFunctionalUnitsForAnnotations.and.returnValue(of({} as any));
    functionalBlockController.deleteAutomatedFunctionalBlock.and.returnValue(of({} as any));
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('handleCancel should set isVisible to false and call modalRef.destroy', () => {
    component.isVisible = true;
    component.handleCancel();
    expect(component.isVisible).toBe(false);
    const modalRef = TestBed.inject(NzModalRef);
    expect(modalRef.destroy).toHaveBeenCalled();
  });

  it('canSave should return false on untouched form', () => {
    component.createFunctionGroupForm.get('name').setValue('block name');
    component.createFunctionGroupForm.markAsUntouched();
    expect(component.canSave()).toBeFalsy();
  })

  it('canSave should return true on dirty form', () => {
    component.createFunctionGroupForm.get('name').setValue('block name');
    expect(component.canSave()).toBeFalsy();
    component.createFunctionGroupForm.markAsDirty();
    expect(component.canSave()).toBeTruthy();
  });

  it('canSave should return false on empty or whitespace name', () => {
    component.createFunctionGroupForm.markAsDirty();
    component.createFunctionGroupForm.get('name').setValue('');
    expect(component.canSave()).toBeFalsy();
    component.createFunctionGroupForm.get('name').setValue(' ');
    expect(component.canSave()).toBeFalsy();
  });

  it('canSave should return false on dirty form when name matches other existing functional group name', () => {
    prepareExistingFunctionalGroups();
    component.createFunctionGroupForm.get('name').setValue('B');
    component.createFunctionGroupForm.markAsDirty();
    expect(component.canSave()).toBeFalsy();
  });

  it('canSave should return true on dirty form when name matches the name of the functional group we want to edit', () => {
    prepareExistingFunctionalGroups();
    component.createFunctionGroupForm.get('name').setValue('A');
    component.createFunctionGroupForm.markAsDirty();
    expect(component.canSave()).toBeTruthy();
  });

  it('should set isDeleteLoading to true and handle block deletion successfully', () => {
    component.projectId = 1;
    const someUid = 'yourFunctionalBlockUid';
    component.functionalBlock = {
      uid: someUid,
      name: 'A',
      description: 'existing description',
      childrenDeep: null
    };
    component.onDeleteFg();
    expect(functionalBlockController.deleteAutomatedFunctionalBlock).toHaveBeenCalledWith(component.projectId, someUid);
    expect(nzMessageServiceStub.create).toHaveBeenCalledWith('success', 'functionalBlock.functionalBlockDeleteSuccess');
    expect(component.isDeleteLoading).toBeFalse();
});

  const prepareExistingFunctionalGroups = (): void => {
    component.functionalBlock = {
      uid: 'some_uid',
      name: 'A',
      description: 'existing description',
      childrenDeep: null
    };

    component.listOfFgsByModule = [{
      name: 'A'
    }, {
      name: 'B'
    }];
  }
});
