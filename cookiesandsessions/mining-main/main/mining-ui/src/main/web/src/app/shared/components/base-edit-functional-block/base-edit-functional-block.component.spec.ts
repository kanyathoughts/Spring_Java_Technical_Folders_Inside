import { ComponentFixture, TestBed } from "@angular/core/testing";
import { BaseEditFunctionalBlockComponent } from "./base-edit-functional-block.component";
import { NzModalRef } from "ng-zorro-antd/modal";
import { NzMessageService } from "ng-zorro-antd/message";
import { FunctionalBlockControllerService } from "@innowake/mining-api-angular-client";
import { ReactiveFormsModule } from "@angular/forms";
import { TranslateModule } from "@ngx-translate/core";
import { of } from "rxjs";
import { FeatureToggleService } from "@app/core/services/feature-toggle/feature-toggle.service";

describe('BaseEditFunctionalBlockComponent', () => {

  let component: BaseEditFunctionalBlockComponent;
  let fixture: ComponentFixture<BaseEditFunctionalBlockComponent>;

  const functionalBlockController: jasmine.SpyObj<FunctionalBlockControllerService> = jasmine.createSpyObj('FunctionalBlockControllerService', [
    'updateFunctionalBlock'
  ]);
  const nzMessageServiceStub: jasmine.SpyObj<NzMessageService> = jasmine.createSpyObj<NzMessageService>
    ('NzMessageService', ['create']); 
  const nzModalSpy: jasmine.SpyObj<NzModalRef> = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['destroy']);
  const featureToggleServiceSpy: jasmine.SpyObj<FeatureToggleService> = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [BaseEditFunctionalBlockComponent],
      imports: [ReactiveFormsModule, TranslateModule.forRoot({}),],
      providers: [
        { provide: NzModalRef, useValue: nzModalSpy },
        { provide: FunctionalBlockControllerService, useValue: functionalBlockController },
        { provide: NzMessageService, useValue: nzMessageServiceStub },
        { provide: FeatureToggleService, useValue: featureToggleServiceSpy }
      ]
    });

    fixture = TestBed.createComponent(BaseEditFunctionalBlockComponent);
    component = fixture.componentInstance;
    functionalBlockController.updateFunctionalBlock.and.returnValue(of({} as any));
    featureToggleServiceSpy.isActive.and.returnValue(of(false));
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should disable the save button for untouched form', () => {
    component.formGroup.reset();
    expect(component.canSave()).toBeFalse();
  });

  it('should disable the save button for empty name', () => {
    component.formGroup.get('name').setValue('');
    component.formGroup.markAsDirty();
    expect(component.canSave()).toBeFalse();
  });

  it('should disable the save button when name input contains only spaces', () => {
    component.formGroup.get('name').setValue('      ');
    component.formGroup.markAsDirty();
    expect(component.canSave()).toBeFalse();
  });

  it('should close the modal on cancel', () => {
    component.handleCancel();
    expect(component.isVisible).toBeFalse();
    expect(nzModalSpy.destroy).toHaveBeenCalled();
  });

  it('should not show generate-module-descriptions-checkbox', () => {
    featureToggleServiceSpy.isActive.and.returnValue(of(true));
    component.ngOnInit();
    fixture.detectChanges();
    expect(fixture.nativeElement.querySelector('#module-descriptions-checkbox-wrapper')).toBeFalsy();
  });
});
