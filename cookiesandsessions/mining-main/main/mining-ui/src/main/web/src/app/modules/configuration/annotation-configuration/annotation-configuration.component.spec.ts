import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { SharedModule } from '@app/shared';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { of } from 'rxjs';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { I18nService } from '@app/core';
import { AnnotationConfigurationComponent } from './annotation-configuration.component';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule } from '@angular/forms';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { AllowedTableActions } from '@app/shared/components/mining-table/mining-table-action.interface';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { AnnotationCategory, AnnotationCategoryControllerService, AnnotationControllerService, DataDictionaryControllerService, FeatureControllerService, ModuleControllerService, ProjectControllerService, TaxonomyControllerService } from '@innowake/mining-api-angular-client';

describe('AnnotationConfigurationComponent', () => {
  let component: AnnotationConfigurationComponent;
  let fixture: ComponentFixture<AnnotationConfigurationComponent>;
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);
  const annotationCategory: AnnotationCategory[] = [
    {
      recordId: '#86:0',
      customProperties:
      {
        'Property1': [
          {
            dataType: 'STRING'
          }
        ]
      },
      id: 1,
      name: 'Annotation Category A',
      projectId: 1,
      types: ['RULE', 'DATABASE']
    },
    {
      recordId: '#86:1',
      customProperties:
      {
        'Property1': [
          {
            dataType: 'STRING'
          }
        ]
      },
      id: 6,
      name: 'AnnotationTest',
      projectId: 1,
      types: ['RULE', 'DEAD_CODE']
    }
  ];

  const annotationCategoryControllerServiceStub: jasmine.SpyObj<AnnotationCategoryControllerService> = jasmine.createSpyObj<AnnotationCategoryControllerService>
    ('AnnotationCategoryControllerService', ['findAllAnnotationCategories', 'updateAnnotationCategory', 'createAnnotationCategory', 'deleteAnnotationCategory']);
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [AnnotationConfigurationComponent],
      imports: [FormsModule,
        RouterModule,
        SharedModule,
        BrowserAnimationsModule,
        HttpClientTestingModule,
        TranslateModule.forRoot({}),
        RouterModule.forRoot([]),
        RouterTestingModule.withRoutes([])],
      providers: [
        TranslateService,
        FeatureControllerService,
        NumberFormatter,
        I18nService,
        ModuleControllerService,
        AnnotationControllerService,
        ProjectControllerService,
        TaxonomyControllerService,
        DataDictionaryControllerService,
        { provide: AnnotationCategoryControllerService, useValue: annotationCategoryControllerServiceStub },
        { provide: NzModalRef, useValue: nzModalRefSpy },
        {
          provide: KeycloakAuthorizationService,
          useValue: new NoAuthorizationService()
        },
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
              project: { id: 1 }
            }),
            snapshot: {
              queryParams: of({
                filter: '[{"key":"objectTypeLink.typeLink","value":["PROGRAM"]}]',
                page: 1,
                sort: 'name;ASC'
              })
            }
          }
        },
      ]
    }).compileComponents();
    annotationCategoryControllerServiceStub.findAllAnnotationCategories.and.returnValue(of(annotationCategory as any));
    annotationCategoryControllerServiceStub.updateAnnotationCategory.and.returnValue(of('' as any));
    annotationCategoryControllerServiceStub.createAnnotationCategory.and.returnValue(of('' as any));
    annotationCategoryControllerServiceStub.deleteAnnotationCategory.and.returnValue(of('' as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(AnnotationConfigurationComponent);
    component = fixture.componentInstance;
    component.projectId = 1;
    fixture.detectChanges();
  });

  const defaultValue={
    "type": "DATABASE",
    "id": "DATABASE",
    "isEditable": false,
    "name": "DATABASE",
    "expand": true,
    "level": 0,
    "key": "parent-DATABASE",
    "children": [{
        "type": "Close",
      },
      {
        "type": "open",
      },
      {
        "type": "edit",
      }]
  }

  it('should load instance', () => {
    expect(component).toBeTruthy();
  });

  it('should emit event when editing starts', () => {
    spyOn(component.dataChangeEvent, 'next');
    const data = {
      "type": "Close",
      "id": "DATABASE__Close",
      "rid": 12,
      "level": 1,
      "expand": false,
      "parent": defaultValue,
      "key": "child-DATABASE__Close"
    }
    component.editingStarted(data);
    expect(component.dataChangeEvent.next).toHaveBeenCalled();
  });

  it('should test update record', () => {
    const testData = {
      "actualValue": "Close",
      "newValue": "Closes",
      "type": "Close",
      "parent": defaultValue
    }
    component.updateAnnotationCategory(testData);
    expect((component as any).annotationCategoryController.updateAnnotationCategory).toHaveBeenCalled();
  });

  it('should test update record', () => { 
    const customdata = {
      "actualValue": "New Annotation Category",
      "newValue": "you",
      "isNewRecord": true,
      "type": "New Annotation Category",
      "parent": defaultValue
    }
    component.updateAnnotationCategory(customdata);
    expect((component as any).annotationCategoryController.createAnnotationCategory).toHaveBeenCalled();
  });

  it('should test optionSelected', () => {
    spyOn(component.dataChangeEvent, 'next');
    const newTaxonomyType = { optionValue: 'addCategory', data: { children: [{ name: 'testing Type' }] } };
    component.optionSelected(newTaxonomyType);
    expect(component.dataChangeEvent.next).toHaveBeenCalled();
  });

  it('should test cancelTaxonomyForm record', () => {
    spyOn(component, 'optionSelected');
    spyOn(component.dataChangeEvent, 'next');
    component.cancelAnnotationForm({ isNewRecord: true, rid: 1 });
    expect(component.dataChangeEvent.next).toHaveBeenCalledWith({ action: AllowedTableActions.TOGGLE_ACTIONS, data: false });
    expect(component.optionSelected).toHaveBeenCalled();
  });

  it('should test delete Taxonomy functionality for taxonomyTerm', () => {
    spyOn(component as any, 'optionSelected').and.callThrough();
    spyOn(component as any, 'deleteAnnotationCategory').and.callThrough();
    const newTaxonomyTerm = { optionValue: 'deleteCategory', data: {} };
    component.optionSelected(newTaxonomyTerm);
    component.deleteAnnotationCategory(newTaxonomyTerm);
    expect(component.optionSelected).toHaveBeenCalled();
  });

  it('should test showErrorMsg Taxonomy', () => {
    spyOn((component as any).messageService, 'error');
    (component as any).showErrorMsg('error');
    expect((component as any).messageService.error).toHaveBeenCalled();
  });

 it('should create child objects and add them to categoryList', () => {
    component.categoryList = [
      {
          "type": "DATABASE",
          "children": [
              {
                  "type": "Close",
                  "id": "DATABASE__Close",
                  "rid": 8,
                  "isEditable": true,
                  "removeActions": [
                      "deleteCategory"
                  ]
              },
              {
                  "type": "Declare",
                  "id": "DATABASE__Declare",
                  "rid": 7,
                  "isEditable": true,
                  "removeActions": [
                      "deleteCategory"
                  ]
              },
              {
                  "type": "New Category",
                  "id": "DATABASE__New Category",
                  "rid": 13,
                  "isEditable": true,
                  "removeActions": []
              },
              {
                  "type": "Read",
                  "id": "DATABASE__Read",
                  "rid": 5,
                  "isEditable": true,
                  "removeActions": [
                      "deleteCategory"
                  ]
              },
              {
                  "type": "Write",
                  "id": "DATABASE__Write",
                  "rid": 6,
                  "isEditable": true,
                  "removeActions": [
                      "deleteCategory"
                  ]
              }
          ],
          "id": "DATABASE",
          "isEditable": false,
          "name": "DATABASE"
      },
      {
          "type": "DEAD_CODE",
          "id": "DEAD_CODE",
          "nonExpandableRowIndent": 1,
          "isEditable": false,
          "name": "DEAD_CODE"
      },
      {
          "type": "EXCLUDE",
          "id": "EXCLUDE",
          "nonExpandableRowIndent": 1,
          "isEditable": false,
          "name": "EXCLUDE"
      },
      {
          "type": "FUNCTIONAL",
          "children": [
              {
                  "type": "None",
                  "id": "FUNCTIONAL__None",
                  "rid": 12,
                  "isEditable": true,
                  "removeActions": []
              }
          ],
          "id": "FUNCTIONAL",
          "isEditable": false,
          "name": "FUNCTIONAL"
      },
      {
          "type": "RULE",
          "children": [
              {
                  "type": "Business Rule",
                  "id": "RULE__Business Rule",
                  "rid": 2,
                  "isEditable": true,
                  "removeActions": [
                      "deleteCategory"
                  ]
              },
              {
                  "type": "Error Processing Rule",
                  "id": "RULE__Error Processing Rule",
                  "rid": 11,
                  "isEditable": true,
                  "removeActions": [
                      "deleteCategory"
                  ]
              },
              {
                  "type": "Field Computation Rule",
                  "id": "RULE__Field Computation Rule",
                  "rid": 10,
                  "isEditable": true,
                  "removeActions": [
                      "deleteCategory"
                  ]
              },
              {
                  "type": "Process Rule",
                  "id": "RULE__Process Rule",
                  "rid": 3,
                  "isEditable": true,
                  "removeActions": [
                      "deleteCategory"
                  ]
              },
              {
                  "type": "Technical Rule",
                  "id": "RULE__Technical Rule",
                  "rid": 9,
                  "isEditable": true,
                  "removeActions": [
                      "deleteCategory"
                  ]
              },
              {
                  "type": "Validation Rule",
                  "id": "RULE__Validation Rule",
                  "rid": 4,
                  "isEditable": true,
                  "removeActions": [
                      "deleteCategory"
                  ]
              }
          ],
          "id": "RULE",
          "isEditable": false,
          "name": "RULE"
      }
  ]
    component.configureAnnotationsNotToBeRemoved = ['type1'];
    const typePerCategory = {
      category1: [
        { name: 'type1', projectId: 0, rid: 1 },
        { name: 'type2', projectId: 1, rid: 2 }
      ],
      category2: [
        { name: 'type3', projectId: 0, rid: 3 },
        { name: 'type4', projectId: 1, rid: 4 }
      ]
    };
    expect(component.categoryList.length).toBe(5);
    expect(component.categoryList[0].children.length).toBe(5);
    expect(component.categoryList[0].children[0].removeActions).toEqual(['deleteCategory']);
  });
});
