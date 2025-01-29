import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { SharedModule } from '@app/shared';
import { TranslateModule } from '@ngx-translate/core';
import { VirtualModuleDetailsComponent } from './virtual-module-details.component';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ModulePojo } from '@innowake/mining-api-angular-client';

describe('VirtualModuleDetailsComponent', () => {
  let component: VirtualModuleDetailsComponent;
  let fixture: ComponentFixture<VirtualModuleDetailsComponent>;

  const moduleValue = [{ "group": { "CONTAINING_MODULE_ID": 4609 as any, "CONTAINING_MODULE_NAME": "ADD012A" as any }, "fields": { "NAME": ["ADD012A"] as any, "ID": [4606] as any } }]
  const module: ModulePojo = {
    uid: '#136:600',
    customProperties: {'Property1':[{
      name: 'customMetaInfo2',
      value: null,
      dataType: 'STRING'
    }, {
      name: 'customMetaInfo1',
      value: null,
      dataType: 'STRING'
    }]},
    id: 2007,
    name: 'CC1',
    projectId: 1,
    path: 'src/cobol/programs/CC1.cpy',
    technology: 'COBOL',
    type: 'COPYBOOK',
    storage: 'FILE',
    identification: 'IDENTIFIED',
    origin: 'CUSTOM',
    info: null,
    description: 'A test copy',
    sourceMetrics: {
      codeLines: null,
      commentLines: null,
      complexityMcCabe: null
    },
    content: null,
    sourceCodeAvailable: false,
    parent: '4609'
  };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [VirtualModuleDetailsComponent],
      imports: [SharedModule,
        RouterTestingModule,
        BrowserAnimationsModule,
        TranslateModule.forRoot({}),
        HttpClientTestingModule],
      providers: []
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(VirtualModuleDetailsComponent);
    component = fixture.componentInstance;
    component.selectedModule = module;
    component.virtualModuleDetails = moduleValue;
    fixture.detectChanges();
  });

  it('should load instance', () => {
    expect(component).toBeTruthy();
  });

  it('should call ngOnChange', () => {
    component.selectedModule.parent = '4609';
    component.selectedModule.id = null;
    const data = {
      virtualModuleDetails: module
    };
    component.ngOnChanges(data as any);
    expect(component.selectedModule.id).toEqual(null);
  });

  describe('should call else ngOnChanges', () => {
    it('should call else when data is empty', () => {
      component.selectedModule.parent = '4609';
      component.selectedModule.id = null;
      component.ngOnChanges({} as any);
      expect(component.selectedModule.id).toEqual(null);
    });

    it('should call else when containingModuleId is null', () => {
      component.selectedModule.parent = null;
      component.selectedModule.id = null;
      const data = {
        virtualModuleDetails: module
      };
      component.ngOnChanges(data as any);
      expect(component.selectedModule.id).toEqual(null);
    });
  });
})