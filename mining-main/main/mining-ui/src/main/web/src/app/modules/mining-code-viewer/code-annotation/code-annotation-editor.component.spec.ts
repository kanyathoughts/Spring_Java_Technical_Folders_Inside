import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { CodeAnnotationEditorComponent } from './code-annotation-editor.component';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { NzButtonModule } from 'ng-zorro-antd/button';
import { ElementRef } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { AnnotationElementData } from '../mining-code-viewer';
import { of } from 'rxjs';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalService } from 'ng-zorro-antd/modal';
import { AnnotationControllerService, AnnotationPojo, AnnotationToFunctionalBlockControllerService, FunctionalBlockControllerService } from '@innowake/mining-api-angular-client';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { SharedAnnotationEditorService } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { BrowserAnimationsModule, NoopAnimationsModule } from '@angular/platform-browser/animations';
import { SharedAnnotationEditorComponent } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.component';

class MockElementRef implements ElementRef {
  nativeElement = { style: { display: 'none' } };
}

const annotationElement: AnnotationElementData = {
  annotation: {
    id: 1,
    state: AnnotationPojo.StateEnum.CANDIDATE
  },
  borderColor: '',
  typeLabel: '',
  stateLabel: '',
  viewZoneId: '',
  moduleId: 1,
  moduleName: '',
  projectId: 1,
  modulePath: 'modulePath'
};

describe('CodeAnnotationEditorComponent', () => {
  let component: CodeAnnotationEditorComponent;
  let fixture: ComponentFixture<CodeAnnotationEditorComponent>;
  let modalService: NzModalService;
  let translateService: TranslateService;
  let messageService: NzMessageService;

  const annotationControllerServiceSpy: jasmine.SpyObj<AnnotationControllerService> = jasmine.createSpyObj('AnnotationControllerService', [
    'updateAnnotation',
    'deleteAnnotation'
  ]);
  const sharedAnnotationServiceSpy: jasmine.SpyObj<SharedAnnotationEditorService> = jasmine.createSpyObj('SharedAnnotationEditorService', [
    'refreshCodeAnnotations$'
  ]);

  const GraphQlControllerServiceStub: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj<GraphQlControllerService>
    ('GraphQlControllerService', ['graphQl']);


  const childComponent: jasmine.SpyObj<SharedAnnotationEditorComponent> = jasmine.createSpyObj('SharedAnnotationEditorComponent', [
    'isSave',
    'isDelete',
    'resetForm',
    'isClose'
  ])

  const notificationSpy: jasmine.SpyObj<NzNotificationService> = jasmine.createSpyObj<NzNotificationService>
    ('NzNotificationService', ['create']);
  
  const nzMessageServiceStub: jasmine.SpyObj<NzMessageService> = jasmine.createSpyObj<NzMessageService>
    ('NzMessageService', ['success', 'error']);  
    const functionalBlockController: jasmine.SpyObj<FunctionalBlockControllerService> = jasmine.createSpyObj('FunctionalBlockControllerService', [
      'createFunctionalBlock'
    ]);
  const annotationFunctionalController: jasmine.SpyObj<AnnotationToFunctionalBlockControllerService> = jasmine.createSpyObj('AnnotationToFunctionalBlockControllerService', [
      'getFunctionalUnitsForAnnotations'
    ]);

  const relationshipServiceSpy = jasmine.createSpyObj<ClientProjectRelationshipService>('ClientProjectRelationshipService', ['getClientProjectObservable']);
    
  const graphQlResponse = {
    "data": {
      "functionalBlocks": {
        "content": [
          {
              "uid": "1a998630-463f-4a82-836a-5445daa2c7ce",
              "name": "STRB1",
              "description": "",
              "generatedFrom": {},
              "children": {
                  "content": [
                      {
                          "uid": "9046d5a1-7850-4ba4-8aab-d1498ca51ccb",
                          "name": "Data Validation Rule Candidate [System identified]",
                          "description": "Data Validation Rule Candidate [System identified]",
                          "generatedFrom": {
                              "annotationId": 31
                          }
                      },
                      {
                          "uid": "12494e54-4c22-478b-b0f3-063acc68be32",
                          "name": "Data Validation Rusljlacccccccccccbssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssle Candidate [System identified]",
                          "description": "Data Validation Rusljlacccccccccccbssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssle Candidate [System identified]",
                          "generatedFrom": {
                              "annotationId": 29
                          }
                      }
                  ]
              }
          },
          {
              "uid": "95e4e0b6-a809-408f-9c7a-b0ec09eff773",
              "name": "STRB4444",
              "description": "",
              "generatedFrom": {},
              "children": {
                  "content": [
                      {
                          "uid": "12494e54-4c22-478b-b0f3-063acc68be32",
                          "name": "Data Validation Rusljlacccccccccccbssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssle Candidate [System identified]",
                          "description": "Data Validation Rusljlacccccccccccbssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssle Candidate [System identified]",
                          "generatedFrom": {
                              "annotationId": 29
                          }
                      }
                  ]
              }
          }
      ]
      },
      "annotations": {
        "totalElements": 5
      }
    }
  };
  
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        NoopAnimationsModule,
        AntDesignImportsModule,
        HttpClientTestingModule,
        FormsModule,
        TranslateModule.forRoot({}),
        ReactiveFormsModule,
        BrowserAnimationsModule
      ],
      declarations: [CodeAnnotationEditorComponent, SharedAnnotationEditorComponent],
      providers: [
        TranslateService,
        { provide: AnnotationControllerService, useValue: annotationControllerServiceSpy },
        { provide: ElementRef, useClass: MockElementRef },
        { provide: NzNotificationService, useValue: notificationSpy },
        { provide: NzMessageService, useValue: nzMessageServiceStub },
        { provide: NzModalService, useValue: jasmine.createSpyObj('NzModalService', ['create', 'afterClose']) },
        { provide: FunctionalBlockControllerService, useValue: functionalBlockController },
        { provide: AnnotationToFunctionalBlockControllerService, useValue: annotationFunctionalController },
        { provide: NzMessageService, useValue: jasmine.createSpyObj('MessageService', ['create']) },
        {
          provide: GraphQlControllerService,
          useValue: GraphQlControllerServiceStub
        },
        { provide: SharedAnnotationEditorService, useValue: sharedAnnotationServiceSpy },
        { provide: ClientProjectRelationshipService, useValue: relationshipServiceSpy }
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CodeAnnotationEditorComponent);
    component = fixture.componentInstance;
    component.data = annotationElement;
    component.listOfFunctionalGroups = [
      {
          "uid": "1a998630-463f-4a82-836a-5445daa2c7ce",
          "name": "STRB1",
          "description": "",
          "generatedFrom": null,
          "children": {
              "content": [
                  {
                      "uid": "9046d5a1-7850-4ba4-8aab-d1498ca51ccb",
                      "name": "Data Validation Rule Candidate [System identified]",
                      "description": "Data Validation Rule Candidate [System identified]",
                      "generatedFrom": {
                          "annotationId": 31
                      }
                  },
                  {
                      "uid": "12494e54-4c22-478b-b0f3-063acc68be32",
                      "name": "Data Validation Rusljlacccccccccccbssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssle Candidate [System identified]",
                      "description": "Data Validation Rusljlacccccccccccbssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssle Candidate [System identified]",
                      "generatedFrom": {
                          "annotationId": 29
                      }
                  }
              ]
          }
      },
      {
          "uid": "95e4e0b6-a809-408f-9c7a-b0ec09eff773",
          "name": "STRB4444",
          "description": "",
          "generatedFrom": null,
          "children": {
              "content": [
                  {
                      "uid": "12494e54-4c22-478b-b0f3-063acc68be32",
                      "name": "Data Validation Rusljlacccccccccccbssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssle Candidate [System identified]",
                      "description": "Data Validation Rusljlacccccccccccbssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssle Candidate [System identified]",
                      "generatedFrom": {
                          "annotationId": 29
                      }
                  }
              ]
          }
      }
  ];
    modalService = TestBed.inject(NzModalService);
    translateService = TestBed.inject(TranslateService);
    messageService = TestBed.inject(NzMessageService);
    sharedAnnotationServiceSpy.refreshCodeAnnotations$ = of(true);
    annotationControllerServiceSpy.updateAnnotation.and.returnValues(of(annotationElement.annotation as any));
    annotationControllerServiceSpy.deleteAnnotation.and.returnValues(of('' as any));
    functionalBlockController.createFunctionalBlock.and.returnValues(of({} as any));
    annotationFunctionalController.getFunctionalUnitsForAnnotations.and.returnValue({} as any);
    GraphQlControllerServiceStub.graphQl.and.returnValue(of(graphQlResponse as any));
    fixture.detectChanges();
  });

  it('should toggle content view', () => {
    component.showAllContent = true;
    component.toggleContentView();
    expect(component.showAllContent).toBeFalsy();

    component.showAllContent = false;
    component.toggleContentView();
    expect(component.showAllContent).toBeTruthy();
  });

  // Add expect statement
  xit('should delete new annotation on click of cancel button else close the editor', () => {
    component.deletedCallback = () => { };
    spyOn(component, 'deletedCallback').and.callThrough();
  });

});
