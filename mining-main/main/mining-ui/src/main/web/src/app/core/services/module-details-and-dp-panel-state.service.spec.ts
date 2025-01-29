import { TestBed, ComponentFixture, waitForAsync } from '@angular/core/testing';

import { ModuleDetailsDPSidePanelService } from './module-details-and-dp-panel-state.service';
import { Component } from '@angular/core';
import { TranslateService } from '@ngx-translate/core'; 
import { NzModalService } from 'ng-zorro-antd/modal';

const panelState = {
    characteristics: false,
    metrics: false,
    description: false,
    taxonomy: false,
    annotations: false,
    customProperty: false
};
@Component({
    template: ''
})
class SharedModuleDetailsComponent {
    constructor(private moduleDetailsDPSidePanelService: ModuleDetailsDPSidePanelService) {
        this.moduleDetailsDPSidePanelService.setPanelState(panelState, 'nodePanelState');
    }

    getmetricsTaxonomyFilter() {
        return this.moduleDetailsDPSidePanelService.getPanelState('nodePanelState');
    }
}
const translateServiceSpy = jasmine.createSpyObj<TranslateService>('TranslateService', ['instant']);
const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
describe('SharedModuleDetailService', () => {
    let fixture: ComponentFixture<SharedModuleDetailsComponent>;
    let component: SharedModuleDetailsComponent;

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            declarations: [SharedModuleDetailsComponent],
            providers: [SharedModuleDetailsComponent, 
                { provide: TranslateService, useValue: translateServiceSpy },
                { provide: NzModalService, useValue: modalServiceSpy }]
        }).compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(SharedModuleDetailsComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should be created', () => {
        const service: ModuleDetailsDPSidePanelService = TestBed.inject(ModuleDetailsDPSidePanelService);
        expect(service).toBeTruthy();
        service.setPanelState(panelState, 'nodePanelState');
        service.getPanelState('nodePanelState');
        expect(component).toBeTruthy();
    });
})
