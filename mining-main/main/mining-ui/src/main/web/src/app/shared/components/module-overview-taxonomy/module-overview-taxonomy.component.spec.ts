import { HttpClientTestingModule } from '@angular/common/http/testing';
import { SimpleChange, SimpleChanges } from '@angular/core';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { SharedModule } from '@app/shared';
import { TranslateModule } from '@ngx-translate/core';
import { BehaviorSubject, of, throwError } from 'rxjs';
import { AssignTaxonomiesModalService } from '../assign-taxonomies/assign-taxonomies-modal.service';
import { ModuleOverViewTaxonomyComponent } from './module-overview-taxonomy.component';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { ModuleControllerService, TaxonomyAssignmentsGetResponse, TaxonomyControllerService, TaxonomyGetAssignment, TaxonomyPojo } from '@innowake/mining-api-angular-client';

describe('ModuleOverViewTaxonomyComponent', () => {
    const taxonomyValue: TaxonomyGetAssignment[] = [
      {taxonomy: {
        uid: null,
        customProperties: {},
        id: 3,
        name: 'ARB100',
        projectId: 1,
        type: {
          category: {
            name: 'Business Taxonomies',
            projectId: 1
          },
          name: 'BusinessSubsystem',
          projectId: 1,
        },
        taxonomyReferenceCount: null
    },
    state: 'ALL'}];
    const taxonomy: TaxonomyAssignmentsGetResponse = {
      "moduleCount": 3,
      "taxonomies": [
          { "state": "ALL",
            "taxonomy": {
            "uid": "f1862801-4799-5217-b9a5-998728532de1",
            "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
            "projectId": 1,
            "type": {
              "id": "b04d8da6-aa4b-584e-8b77-a18d5d4d1b8a",
              "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
              "projectId": 1,
              "category": {
                "id": 3,
                "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
                "projectId": 1,
                "name": "Technical Taxonomies"
              },
              "name": "File Access"
            },
            "name": "Read",
            "taxonomyReferenceCount": 1,
            "customProperties": {},
            "id": 11
          }}, 
          { "state": "NONE",
            "taxonomy": {
              "uid": "e4eb4323-32cf-54c3-afa3-71f3c2ed1d94",
              "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
              "projectId": 1,
              "type": {
                  "id": "b19d94a1-cab6-54cb-9874-7d454d8ec3d3",
                  "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
                  "projectId": 1,
                  "category": {
                      "id": 3,
                      "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
                      "projectId": 1,
                      "name": "Technical Taxonomies"
                  },
                  "name": "DB Access"
              },
              "name": "Read",
              "taxonomyReferenceCount": 0,
              "customProperties": {},
              "id": 12
          }},
          { "state": "SOME",
          "taxonomy": {
            "uid": "50b4df09-4c1f-5bca-9244-42154a118b61",
            "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
            "projectId": 1,
            "type": {
                "id": "0fdbcc58-cb4f-57a8-a96c-d13815a7ae3e",
                "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
                "projectId": 1,
                "category": {
                    "id": 3,
                    "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
                    "projectId": 1,
                    "name": "Technical Taxonomies"
                },
                "name": "Program Type"
            },
            "name": "MQ",
            "taxonomyReferenceCount": 0,
            "customProperties": {},
            "id": 20
        }},
          { "state": "SOME",
            "taxonomy": {
              "uid": "f4853255-3099-5b39-b407-0d7f1a41c056",
              "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
              "projectId": 1,
              "type": {
                  "id": "0fdbcc58-cb4f-57a8-a96c-d13815a7ae3e",
                  "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
                  "projectId": 1,
                  "category": {
                      "id": 3,
                      "project": "4b58428c-a038-59cb-80ee-952c9e69433a",
                      "projectId": 1,
                      "name": "Technical Taxonomies"
                  },
                  "name": "Program Type"
              },
              "name": "UI",
              "taxonomyReferenceCount": 0,
              "customProperties": {},
              "id": 19
          }}
      ]
  };

    let component: ModuleOverViewTaxonomyComponent;
    let fixture: ComponentFixture<ModuleOverViewTaxonomyComponent>;
    const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['destroy', 'updateConfig', 'close', 'getContentComponent']);
    const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService', ['findTaxonomiesForModule']);
    const assignTaxonomiesModalServiceSpy = jasmine.createSpyObj('AssignTaxonomiesModalService',
    ['triggerLoadSubject', 'getloadTaxonomySubject', 'getupdatedTaxonomyData', 'setUpdatedTaxonomyData']);
    assignTaxonomiesModalServiceSpy.closeModal = new BehaviorSubject<boolean>(false);
    const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create', 'closeAll']);

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
          declarations: [ModuleOverViewTaxonomyComponent],
          imports: [SharedModule,
            RouterTestingModule,
            BrowserAnimationsModule,
            TranslateModule.forRoot({}),
            HttpClientTestingModule],
          providers: [{provide: ModuleControllerService, useValue: moduleControllerServiceSpy}, { provide: AssignTaxonomiesModalService, useValue: assignTaxonomiesModalServiceSpy },
            { provide: NzModalRef, useValue: { destroy: () => true, close: () => true } },{ provide: NzModalService, useValue: modalServiceSpy }, TaxonomyControllerService]
        }).compileComponents();
        moduleControllerServiceSpy.findTaxonomiesForModule.and.returnValues(of(taxonomyValue as any), of([] as any), throwError(new Error('Test Error')));
        assignTaxonomiesModalServiceSpy.getloadTaxonomySubject.and.returnValue(of(true));
        assignTaxonomiesModalServiceSpy.getupdatedTaxonomyData.and.returnValue(of(taxonomy as any));
        nzModalRefSpy.close.and.returnValue();
        nzModalRefSpy.getContentComponent.and.returnValue({});
        modalServiceSpy.create.and.returnValue(nzModalRefSpy);
      }));

    beforeEach(() => {
        fixture = TestBed.createComponent(ModuleOverViewTaxonomyComponent);
        component = fixture.componentInstance;
        component.moduleIdArray = [1];
        component.moduleName = 'PRG1';
        component.projectId = 1;
        component.taxonomyResponse = {
          "moduleCount": 3,
          "taxonomies": [
              { taxonomy: { "projectId": 1, "id": 1, "uid": "#123:456", "type": { "projectId": 1, "name": "Program Type", "category": {"projectId": 1, "name": "Technical Taxonomies","id": 8} }, "name": "Library", "taxonomyReferenceCount": 3, "customProperties": {} }, "state": "ALL"},
              { taxonomy: { "projectId": 1, "id": 2, "uid": "#123:457", "type": { "projectId": 1, "name": "Program Type", "category": {"projectId": 1, "name": "Technical Taxonomies","id": 8} }, "name": "Batch", "taxonomyReferenceCount": 0, "customProperties": {}}, "state": "NONE" },
              { taxonomy: { "projectId": 1, "id": 6, "uid": "#123:458", "type": { "projectId": 1, "name": "File Access", "category": {"projectId": 1, "name": "Technical Taxonomies","id": 8} }, "name": "Read", "taxonomyReferenceCount": 1, "customProperties": {}}, "state": "SOME" },
              { taxonomy: { "projectId": 1, "id": 7, "uid": "#123:459", "type": { "projectId": 1, "name": "File Access", "category": {"projectId": 1, "name": "Technical Taxonomies","id": 8} }, "name": "Write", "taxonomyReferenceCount": 1, "customProperties": {}}, "state": "SOME" }
          ]
      };
        fixture.detectChanges();
    });

    it('should load instance', () => {
      expect(component).toBeTruthy();
    });

    it('should get taxonomy tree data', () => {
      spyOn(component, 'getTaxonomyData').and.callThrough();
      const taxonomyData: TaxonomyPojo[] = [];
      taxonomy.taxonomies.forEach(response => {
        if (response.state === 'ALL' || response.state === 'SOME') {
          taxonomyData.push(response.taxonomy);
        }
      });
      component.getTaxonomyData(taxonomyData);
      expect(component.getTaxonomyData).toHaveBeenCalled();
    });

    it('should check for changes', () => {
      let showModalfromModuleDetails: SimpleChange;
      const changes: SimpleChanges = { showModalfromModuleDetails: { previousValue: false, currentValue: true, firstChange: false, isFirstChange: undefined} };
      spyOn(component, 'assignTaxonomies').and.callThrough();
      component.ngOnChanges(changes);
      expect(component.assignTaxonomies).toHaveBeenCalled();
    });

    it('check handle cancel', () => {
      spyOn(component, 'handleCancel').and.callThrough();
      component.handleCancel();
    });
})