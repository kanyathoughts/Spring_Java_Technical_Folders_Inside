import { ComponentFixture, TestBed } from "@angular/core/testing";
import { of, throwError } from "rxjs";
import { ReactiveFormsModule } from "@angular/forms";
import { TranslateModule } from "@ngx-translate/core";
import { NzMessageService } from "ng-zorro-antd/message";
import { NzModalRef } from "ng-zorro-antd/modal";
import { GraphQlControllerService } from "@app/core/services/graphql.service";
import { FeatureToggleService } from "@app/core/services/feature-toggle/feature-toggle.service";
import { CreateFunctionalGroupComponent } from "./create-functional-group.component";
import { FunctionalBlockControllerService } from "@innowake/mining-api-angular-client";

describe('CreateFunctionalGroupComponent', () => {
  let component: CreateFunctionalGroupComponent;
  let fixture: ComponentFixture<CreateFunctionalGroupComponent>;

  const functionalBlockControllerService: jasmine.SpyObj<FunctionalBlockControllerService> = jasmine.createSpyObj('FunctionalBlockControllerService', [
    'updateFunctionalBlock', 'createFunctionalBlock', 'computeFunctionalBlock']);
  
  const nzMessageServiceStub: jasmine.SpyObj<NzMessageService> = jasmine.createSpyObj<NzMessageService>
    ('NzMessageService', ['create']);

  const graphQlControllerService: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj('GraphQlControllerService', [
      'graphQl'
    ]);
  const nzModalSpy: jasmine.SpyObj<NzModalRef> = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['destroy']);
  const featureToggleServiceSpy: jasmine.SpyObj<FeatureToggleService> = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [CreateFunctionalGroupComponent],
      imports: [ReactiveFormsModule, TranslateModule.forRoot({})],
      providers: [
        { provide: NzModalRef, useValue: nzModalSpy},
        { provide: FunctionalBlockControllerService, useValue: functionalBlockControllerService },
        { provide: NzMessageService, useValue: nzMessageServiceStub },
        { provide: GraphQlControllerService, useValue: graphQlControllerService },
        { provide: FeatureToggleService, useValue: featureToggleServiceSpy }
      ]
    });
    functionalBlockControllerService.updateFunctionalBlock.and.returnValue(of({} as any));
    functionalBlockControllerService.createFunctionalBlock.and.returnValue(of({} as any));
    functionalBlockControllerService.computeFunctionalBlock.and.returnValue(of({} as any));
    graphQlControllerService.graphQl.and.returnValue(of({} as any));
    featureToggleServiceSpy.isActive.and.returnValue(of(false));
    fixture = TestBed.createComponent(CreateFunctionalGroupComponent);
    component = fixture.componentInstance;
    component.projectId = 1;
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should handle the save action when a modal form is submitted', () => {
    // Arrange
    const functionalGroupDetails = [
      { type: 'FUNCTIONAL_GROUP', parent: { title: 'parent1', key: 'key1' }, uid: 'uid1' },
      { type: 'FUNCTIONAL_GROUP', parent: { title: 'parent2', key: 'key2' }, uid: 'uid2' }
    ];
    component.functionalGroupDetails = functionalGroupDetails;
    component.formGroup.get('name').setValue('Test Name');
    component.formGroup.get('description').setValue('Test Description');
    spyOn(component as any, 'computeFunctionalBlock').and.returnValues(of({} as any));
    component.onSave();

    expect(functionalBlockControllerService.updateFunctionalBlock).toHaveBeenCalledTimes(1);
    expect(functionalBlockControllerService.createFunctionalBlock).toHaveBeenCalledTimes(1);
    });
});
