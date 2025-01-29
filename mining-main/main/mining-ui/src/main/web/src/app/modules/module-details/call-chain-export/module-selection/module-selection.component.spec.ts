import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { UntypedFormBuilder, UntypedFormControl } from '@angular/forms';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { I18nService } from '@app/core';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { of } from 'rxjs';
import { ModuleSelectionComponent } from './module-selection.component';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { JobControllerService, ModuleControllerService } from '@innowake/mining-api-angular-client';
import { model } from 'model-test-data';


describe('Module Selection Component', () => {
    let component: ModuleSelectionComponent;
    let fixture: ComponentFixture<ModuleSelectionComponent>;
    let translateService: TranslateService;
    const i18nServiceSpy = { language: 'en-US' };

    const aggregationResult = model.aggregationResult;

    const graphQlResp = {
       data: {
        "modules": {
            "content": [
                {
                    "name": "#BDAT04A",
                    "id": 3742
                },
                {
                    "name": "#BDAT04N",
                    "id": 3795
                }
            ],
            "totalElements": 2,
            "size": 1073741823
        }
       }
    }

    const featureToggleServiceSpy: jasmine.SpyObj<FeatureToggleService> = jasmine.createSpyObj<FeatureToggleService>('FeatureToggleService', ['isActive']);
    const jobControllerSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj<JobControllerService>('JobControllerService', ['submitJobExtension', 'cancelJob']);
    const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj<ModuleControllerService>('ModuleControllerService', ['getAggregatedValues2']);
    const labelMappingServiceSpy: jasmine.SpyObj<LabelMappingService> = jasmine.createSpyObj('LabelMappingService', ['mapLabel'])
    const modalServiceSpy: jasmine.SpyObj<NzModalService> = jasmine.createSpyObj('NzModalService', ['create']);
    const messageServiceSpy: jasmine.SpyObj<NzMessageService> = jasmine.createSpyObj('NzMessageService', ['error']);
    const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose', 'close', 'destroy', 'getContentComponent']);
    const graphQlSpy = jasmine.createSpyObj<GraphQlControllerService>('GraphQlControllerService', ['graphQl']);
    const jobManagerServiceSpy: jasmine.SpyObj<JobManagerService> = jasmine.createSpyObj('JobManagerService', ['register']);


    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            declarations: [ModuleSelectionComponent],
            schemas: [NO_ERRORS_SCHEMA],
            providers: [
                UntypedFormBuilder,
                NzMessageService,
                { provide: JobManagerService, useValue: jobManagerServiceSpy },
                NumberFormatter,
                TranslateService,
                {provide: JobControllerService, useValue: jobControllerSpy},
                {provide: FeatureToggleService, useValue: featureToggleServiceSpy},
                {provide: ModuleControllerService, useValue: moduleControllerServiceSpy},
                { provide: LabelMappingService, useValue: labelMappingServiceSpy },
                { provide: NzModalService, useValue: modalServiceSpy },
                { provide: NzMessageService, useValue: messageServiceSpy },
                { provide: I18nService, useValue: i18nServiceSpy },
                { provide: GraphQlControllerService, useValue: graphQlSpy }
            ],
            imports: [
                NzDropDownModule,
                RouterTestingModule,
                HttpClientTestingModule,
                BrowserAnimationsModule,
                TranslateModule.forRoot({}),
            ],
        }).compileComponents();
        TestBed.compileComponents();
        featureToggleServiceSpy.isActive.and.returnValue(of(false));
        jobControllerSpy.submitJobExtension.and.returnValue(of('test' as any));
        moduleControllerServiceSpy.getAggregatedValues2.and.returnValue(of(aggregationResult as any));
        nzModalRefSpy.getContentComponent.and.returnValue({});
        modalServiceSpy.create.and.returnValue(nzModalRefSpy);
        graphQlSpy.graphQl.and.returnValue(of(graphQlResp as any));
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(ModuleSelectionComponent);
        translateService = TestBed.inject(TranslateService);
        component = fixture.componentInstance;
        component.createModalForModuleListing = nzModalRefSpy;
        component.formModule = 'startModule';
        component.callChainModuleLabel = 'start';
    });

    it('should create component', () => {
        expect(component).toBeTruthy();
    });

    it('should test onInit', () => {
        component.ngOnInit();
        expect(component.startModule.value).toEqual(['-1']);
    });

    it('should test getModuleIdsFromReportingPage', () => {
        spyOn(Object.getPrototypeOf(localStorage), 'getItem').and.returnValue(JSON.stringify([2001,2002]));
        spyOn(component, 'getModuleDetailsFromReporting').and.callThrough();
        component.currentModuleDetails = [{label: 'ModuleA', value: '2003'}];
        component.ngOnInit();
        component.getModuleDetailsFromReporting();
        component['getModuleIdsFromReportingPage'] = JSON.stringify([2001,2002]);
        expect(component.selectedRadio).toBe('name');
    });

    it('should test changeStartLabel for In', () => {
        component.changeStartLabel(['IN']);
        expect(component.allModuleOption.startLabel).toBe('callChain.endModules');
    });

    it('should test changeStartLabel for in & out', () => {
        component.changeStartLabel(['OUT','IN']);
        expect(component.allModuleOption.startLabel).toBe('callChain.entry&endModules');
    });

    it('should test changeStartLabel for Out', () => {
        component.changeStartLabel(['OUT']);
        expect(component.allModuleOption.startLabel).toBe('callChain.entryModules');
    });

    it('should test onOpenSelect else case', () => {
        spyOn(component, 'checkModuleValue').and.callThrough();
        component.onOpenSelect(false);
        expect(component.checkModuleValue).toHaveBeenCalled();
    });

    it('should test onOpenSelect name case', () => {
        component.selectedRadio = 'name';
        component.onOpenSelect(true);
        expect(component.selectByValue).toEqual(component[component.formModule].value);
    });

    it('should test onOpenSelect type case', () => {
        const mockModuleData = ['Module Type A', 'Module Type B'];
        component.selectedRadio = 'type';
        component.listModuleTypeOptions = [{ label: 'Module Type A', value: 'ModuleTypeA' }, { label: 'Module Type B', value: 'ModuleTypeB' }, { label: 'Module Type C', value: 'ModuleTypeC' }];
        spyOn(component as any, 'getSelectByTypeValue').and.returnValue(mockModuleData);
        component.onOpenSelect(true);
        expect(component['getSelectByTypeValue']).toHaveBeenCalledWith(component[component.formModule].value, component.listModuleTypeOptions);
        expect(component.selectByTypeValue).toEqual(mockModuleData);
    });

    it('should test handleModuleSelection allModuleOption.value case', () => {
        component.selectedRadio = component.allModuleOption.value;
        component.handleModuleSelection();
        expect(component[component.formModule].value).toEqual([component.allModuleOption.value]);
    });

    it('should test handleModuleSelection name case', () => {
        const selectedByIdValue = ['Module 1', 'Module 2'];
        component.selectByValue = selectedByIdValue;
        component.selectedRadio = 'name';
        spyOn<any>(component, 'getOptionValue').and.returnValue(selectedByIdValue);
        component.handleModuleSelection();
        expect(component[component.formModule].value).toEqual(selectedByIdValue);
    });

    it('should test handleModuleSelection type case', () => {
        component.selectedRadio = 'type';
        component.selectByTypeValue = ['ModuleTypeA', 'ModuleTypeB'];
        component.listModuleTypeOptions = [{ label: 'Module Type A', value: 'ModuleTypeA' }, { label: 'Module Type B', value: 'ModuleTypeB' }, { label: 'Module Type C', value: 'ModuleTypeC' }];
        component.handleModuleSelection();
        expect(component.startModule.value).toEqual(['Module Type A', 'Module Type B']);
    });

    it('should test updateParentSelect end type case', () => {
        const updateValue = ['Module1'];
        component.selectedRadio = 'type';
        spyOn<any>(component, 'getOptionValue').and.returnValue(updateValue);
        spyOn<any>(component, 'getLabelFromValue').and.returnValue(updateValue);
        component.updateTypeSelect(updateValue);
        expect(component.Options).toEqual(updateValue);
        expect(component[component.formModule].value).toEqual(updateValue);
    });

    it('should test updateParentSelect end name case', () => {
        const updateValue = ['Module1'];
        component.selectedRadio = 'name';
        component.updateTypeSelect(updateValue);
        expect(component.Options).toEqual(updateValue);
        expect(component[component.formModule].value).toEqual(updateValue);
    });

    it('should test onOpenStartSelect type case', () => {
        component.selectedRadio = 'type';
        component.onOpenSelect(true);
        expect(component.selectByTypeValue).toEqual(component[component.formModule].value);
    });

    it('should test checkModuleValue startModule', () => {
        component['isSelectOpen'] = false;
        component['startModule.value.length'] = false;
        component.allModuleOption.value = 'Module1';
        component.selectedRadio = 'type';
        component.checkModuleValue('startModule');
        expect(component.selectedRadio !== '-1' && ! component['isSelectOpen'] && ! component['startModule.value.length']).toBeTruthy();
        expect(component.selectedRadio).toEqual(component.allModuleOption.value);
    });

    it('should test openModalForModules for start', () => {
        const startModules = ['module1', 'module2'];
        component.startModule.setValue(startModules);
        const mockModalForModuleListing = {
            afterClose: {
                subscribe: () => {}
            }
        };
        spyOn(component as any, 'modalCreator').and.returnValue(mockModalForModuleListing);
        spyOn(component, 'openModalForModules').and.callThrough();
        component.openModalForModules('start');
        expect(component['modalCreator']).toHaveBeenCalledWith(startModules, 'start');
    });

    it('should test modalCreator', () => {
        const modalref = component['modalCreator'](['module1'], 'start');
        expect(modalref).toEqual(nzModalRefSpy);
    });

    it('should test updateParentSelect start type case', () => {
        const updateValue = ['Module1'];
        component.selectedRadio = 'type';
        spyOn<any>(component, 'getOptionValue').and.returnValue(updateValue);
        spyOn<any>(component, 'getLabelFromValue').and.returnValue(updateValue);
        component.updateTypeSelect(updateValue);
        expect(component.Options).toEqual(updateValue);
        expect(component[component.formModule].value).toEqual(updateValue);
    });

    it('should test checkRemovedModule', () => {
        const startModule = new UntypedFormControl();
        startModule.setValue(['Module1', 'Module2']);
        component.moduleDetails = [{label: 'Module1', value: [2001]}, {label: 'Module2', value: [2002]}];
        component.modulesFromModuleListingCmp.start = ['Module1', 'Module2', 'Module3'];
        component['checkRemovedModule']('start', startModule);
        expect(component.modulesFromModuleListingCmp.start.length).toEqual(0);
    });
});