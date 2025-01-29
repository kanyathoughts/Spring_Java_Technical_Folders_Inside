import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { SharedModule } from '@app/shared';
import { TranslateModule } from '@ngx-translate/core';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { BehaviorSubject, of, Subject, throwError } from 'rxjs';
import { AssignTaxonomiesModalService } from './assign-taxonomies-modal.service';
import { AssignTaxonomiesComponent } from './assign-taxonomies.component';
import { TaxonomyAssignmentsGetResponse, TaxonomyControllerService } from '@innowake/mining-api-angular-client';

describe('AssignTaxonomyComponent', () => {
    const taxonomy: TaxonomyAssignmentsGetResponse = {
      "moduleCount": 3,
      "taxonomies": [
          { "state": "ALL" },
          { "state": "NONE" },
          { "state": "SOME" },
          { "state": "SOME" }
      ]
  };

    let component: AssignTaxonomiesComponent;
    let fixture: ComponentFixture<AssignTaxonomiesComponent>;

    const taxonomyControllerServiceSpy: jasmine.SpyObj<TaxonomyControllerService> = jasmine.createSpyObj('TaxonomyControllerService', ['updateAssignedTaxonomyByModule', 'bulkUpdateTaxonomiesToModules']);
    const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['destroy', 'updateConfig']);
    const assignTaxonomiesModalServiceSpy = jasmine.createSpyObj('AssignTaxonomiesModalService',
    ['triggerLoadSubject', 'getloadTaxonomySubject', 'getupdatedTaxonomyData', 'setUpdatedTaxonomyData']);
    assignTaxonomiesModalServiceSpy.loadUpdatedTaxonomyData = new Subject();
    assignTaxonomiesModalServiceSpy.closeModal = new BehaviorSubject<boolean>(false);
    const jobManagerServiceSpy: jasmine.SpyObj<JobManagerService> = jasmine.createSpyObj('JobManagerService', ['register']);

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
          declarations: [AssignTaxonomiesComponent],
          imports: [SharedModule,
            RouterTestingModule,
            BrowserAnimationsModule,
            TranslateModule.forRoot({}),
            HttpClientTestingModule],
          providers: [
            { provide: TaxonomyControllerService, useValue: taxonomyControllerServiceSpy},
            { provide: NzModalRef, useValue: nzModalRefSpy },
            { provide: AssignTaxonomiesModalService, useValue: assignTaxonomiesModalServiceSpy },
            { provide: JobManagerService, useValue: jobManagerServiceSpy },
        ]
        }).compileComponents();
        let status = {
            "status$": of({
              "_isScalar": false,
              "closed": false,
              "isStopped": true,
              "hasError": false,
              "_value": "SUCCESS"
            })
          };
        taxonomyControllerServiceSpy.updateAssignedTaxonomyByModule.and.returnValue(of(taxonomy as any));
        jobManagerServiceSpy.register.and.returnValue(status as any);

        taxonomyControllerServiceSpy.bulkUpdateTaxonomiesToModules.and.returnValue(of('fb9f033a-8c92-41cf-b619-7f0c8d56deb7' as any))
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(AssignTaxonomiesComponent);
        component = fixture.componentInstance;
        component.taxonomyResponse = {
            "moduleCount": 3,
            "taxonomies": [
                { taxonomy: {"projectId": 1, "id": 1, "uid": "#123:456", "type": { "projectId": 1, "name": "Program Type", "category": {"projectId": 1, "name": "Technical Taxonomies","id": 8} }, "name": "Library", "taxonomyReferenceCount": 3, "customProperties": {}}, "state": "ALL" },
                { taxonomy: {"projectId": 1, "id": 2, "uid": "#123:457", "type": { "projectId": 1, "name": "Program Type", "category": {"projectId": 1, "name": "Technical Taxonomies","id": 8} }, "name": "Batch", "taxonomyReferenceCount": 0, "customProperties": {}}, "state": "NONE" },
                { taxonomy: {"projectId": 1, "id": 6, "uid": "#123:458", "type": { "projectId": 1, "name": "File Access", "category": {"projectId": 1, "name": "Technical Taxonomies","id": 8} }, "name": "Read", "taxonomyReferenceCount": 1, "customProperties": {}}, "state": "SOME" },
                { taxonomy: {"projectId": 1, "id": 7, "uid": "#123:459", "type": { "projectId": 1, "name": "File Access", "category": {"projectId": 1, "name": "Technical Taxonomies","id": 8} }, "name": "Write", "taxonomyReferenceCount": 1, "customProperties": {}}, "state": "SOME" }
            ]
        };
        component.taxonomyNodes = [{name: 'Business Taxonomies', id:1, children: [{name:'DataDomain', id: 22, checkedState: true, children: [{name: "EmployeeDomain"}]}], checkedState: true}];
        fixture.detectChanges();
    });

    it('should load instance', () => {
        expect(component).toBeTruthy();
    });

    describe('check ngOnInit', () => {
        it('if case', () =>{
            component.taxonomyNodes = [{name: 'Business Taxonomies', id:1, children: [{name:'DataDomain', id: 22, checkedState: true, children: [{name: "EmployeeDomain"}]}], checkedState: true}];
            const data = [{name: 'Business Taxonomies', id:1, children: [{name:'DataDomain', id: 22, checkedState: true, children: [{name: "EmployeeDomain"}]}], checkedState: true}];
            component.searchValue$ = new BehaviorSubject<string>('test');
            component.ngOnInit();
            component.expandedNodes.length = 0;
            expect(component.originData$).toBeDefined();
            expect(component.filteredData$).toBeDefined();
            spyOn(component, 'filterTreeData').and.callThrough();
            component.filterTreeData(data,'a');
            expect(component.expandedNodes).toBeDefined();
        });

        it('empty search case', () =>{
            component.taxonomyNodes = [{name: 'Business Taxonomies', id:1, children: [{name:'DataDomain', id: 22, checkedState: true, children: [{name: "EmployeeDomain"}]}], checkedState: true}];
            const data = [{name: 'Business Taxonomies', id:1, children: [{name:'DataDomain', id: 22, checkedState: true, children: [{name: "EmployeeDomain"}]}], checkedState: true}];
            component.searchValue$ = new BehaviorSubject<string>('test');
            component.ngOnInit();
            component.expandedNodes.length = 1;
            expect(component.originData$).toBeDefined();
            expect(component.filteredData$).toBeDefined();
            spyOn(component, 'filterTreeData').and.callThrough();
            component.filterTreeData(data,'a');
            expect(component.expandedNodes).toBeDefined();
        });

        it('else case', () => {
            component.taxonomyNodes = [{name: 'Business Taxonomies', id:1, children: [{name:'DataDomain', id: 22, checkedState: true, children: [{name: "EmployeeDomain"}]}], checkedState: true}];
            component.searchValue = undefined;
            component.expandedNodes.length = 3;
            component.ngOnInit();
            expect(component.originData$).toBeDefined();
            expect(component.filteredData$).toBeDefined();
            spyOn(component, 'filterTreeData').and.callThrough();
            expect(component.expandedNodes).toBeDefined();
        });
    });

    describe('update checked values', () => {
        it('update checked values as expected in else case', () => {
            const id = 7;
            const checkedState = false;
            const indeterminateState = true;
            const node = {name: 'DataDomain', children: Array(1), checkedState: false, indeterminateState: false, expandable:true, level:1, state: 'NONE'};
            spyOn(component, 'updateAllChecked').and.callThrough();
            component.updateAllChecked(checkedState, node);
            expect(component.updateAllChecked).toHaveBeenCalledWith(checkedState,node);
        });

        it('update checked values as expected in if case', () => {
            const id = 7;
            const checkedState = true;
            const node = {name: 'DataDomain', children: Array(1), checkedState: true, indeterminateState: false, expandable:true, level:1, state: 'ALL'};
            spyOn(component, 'updateAllChecked').and.callThrough();
            component.updateAllChecked(checkedState, node);
            expect(component.updateAllChecked).toHaveBeenCalledWith(checkedState,node);
        });

        it('update checked values not as expected', () => {
            const id = 200;
            const checkedState = false;
            const node = {name: 'DataDomain', checkedState: false, indeterminateState: true, expandable:true, level:1, State: 'SOME'};
            spyOn(component, 'updateAllChecked').and.callThrough();
            component.updateAllChecked(checkedState, node);
            expect(component.updateAllChecked).toHaveBeenCalledWith(checkedState,node);
        });
    });

    it('check filterTreeData', () => {
        const data = [{name: 'Business Taxonomies', children: [{name:'DataDomain', checkedState: true, children: [{name: "EmployeeDomain"}], indeterminateState: false}], checkedState: true, indeterminateState: false}];
        spyOn(component, 'filterTreeData').and.callThrough();
        component.filterTreeData(data,'a');
        const data2 = [{name: 'Business Taxonomies', children: [{name:"BusinessProcess", checkedState: true, children: [{name: "Create Invoices"}], indeterminateState: false}], checkedState: true, indeterminateState: false}];
        component.filterTreeData(data2,'a');
        const data3 = [{name: 'Business Taxonomies', children: [{name:"BusinessProcess", checkedState: true, children: [{name: "UI"}], indeterminateState: false}], checkedState: true, indeterminateState: false}];
        component.filterTreeData(data2,'a');
        expect(component.filterTreeData).toHaveBeenCalled();
    });

    it('check hasChild', () => {
        const data = {name: 'Business Taxonomies', children: Array(1), checkedState: true, indeterminateState: false, expandable:true, level:1};
        spyOn(component, 'hasChild').and.callThrough();
        component.hasChild(1,data);
        expect(component.hasChild).toHaveBeenCalled();
    });
   
    it('update taxonomies to backend error scenario', () => {
        taxonomyControllerServiceSpy.updateAssignedTaxonomyByModule.and.returnValue(throwError('error'));
        spyOn(component, 'updateTaxonomies').and.callThrough();
        component.updateTaxonomies();
        expect(component.updateTaxonomies).toHaveBeenCalled();
    });

    it('check handle cancel', () => {
        spyOn(component, 'handleCancel').and.callThrough();
        component.handleCancel();
        expect(component.handleCancel).toHaveBeenCalled();
    });
})