import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import {
  HotSpotCardComponent,
  HotSpotConfig
} from './hotspot-card.component';
import { SharedModule } from '@app/shared';
import { RouterTestingModule } from '@angular/router/testing';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import {
  of,
  throwError
} from 'rxjs';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { HotSpotVO } from '@app/shared/models/hotspot-vo.model';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { NzCollapseModule } from 'ng-zorro-antd/collapse';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { FilterType, MiningTableConfig } from '@app/shared/components/mining-table/mining-table-config.interface';
import { I18nService } from '@app/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { WindowToken } from '@app/core/utils/window';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { AnnotationControllerService, DataDictionaryControllerService, FeatureControllerService, HotSpot, HotSpotControllerService, ModuleControllerService, ProjectControllerService, TaxonomyControllerService } from '@innowake/mining-api-angular-client';
import { CfgSupportedTypeService } from '@app/core/services/cfg-supported-type.service';

describe('HotSpotCardComponent', () => {
  let component: HotSpotCardComponent;
  let fixture: ComponentFixture<HotSpotCardComponent>;
  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(1, 'TestClient', 1, 'TestProject');
  const hotspotControllerServiceSpy: jasmine.SpyObj<HotSpotControllerService> = jasmine.createSpyObj('HotspotControllerService', ['getHotspots']);
const cfgSupportedTypeServiceSpy = jasmine.createSpyObj<CfgSupportedTypeService>('CfgSupportedTypeService', ['checkIfSupported']);  
  let openedUrl = '';
  const hotspots: HotSpot[] = [{
    count: 1,
    module: {}
  }];
  let mockWindow: any;
  const hotspotsNoContent: HotSpot[] = [];
  const miningTableConfig: MiningTableConfig = {
    columnMap: {
      moduleName: { field: 'moduleName', header: 'moduleName', filterProperties: { filterType: FilterType.freeText }},
      language: { field: 'language', header: 'language', filterProperties: { filterType: FilterType.multiSelect }, options: [] },
      value: { field: 'count', header: 'value', filterProperties: { filterType: FilterType.numberValue }}
    },
    actions: [[{ label: 'btnLabel.view', value: 'view', disableItem: () => false }, { label: 'btnLabel.view', value: 'view', disableItem: () => false }], [{ label: 'btnLabel.view', value: 'view', disableItem: () => false }]],
    actionsWidth: '80px',
    rows: 10
  };
  const i18nServiceSpy = { language: 'en-US' };

  beforeEach(waitForAsync(() => {
    mockWindow = {
      get location() {
          return {
              href: 'http://localhost:8081/#/browse-modules/1/1/1/explore',
              hash: '#/browse-modules/1/1/1/explore'
          };
      },
      open: (sUrl: any) => {
          openedUrl = sUrl;
      }
  };
  mockWindow.open.bind(mockWindow);
    TestBed.configureTestingModule({
      imports: [FormsModule, SharedModule, BrowserAnimationsModule, NzCollapseModule, TranslateModule.forRoot({}), HttpClientTestingModule,RouterTestingModule],
      declarations: [HotSpotCardComponent],
      providers: [
        { provide: HotSpotControllerService, useValue: hotspotControllerServiceSpy },
        { provide: ClientProjectRelationship, useValue: clientProjectRelationship },
        TranslateService,
        FeatureControllerService,
        NumberFormatter,
        ModuleControllerService,
        ProjectControllerService,
        AnnotationControllerService,
        TaxonomyControllerService,
        DataDictionaryControllerService,
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: WindowToken, useValue: mockWindow },
        { provide: CfgSupportedTypeService, useValue: cfgSupportedTypeServiceSpy }
      ]
    }).compileComponents();
    hotspotControllerServiceSpy.getHotspots.and.returnValue(of(hotspots) as any);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(HotSpotCardComponent);
    component = fixture.componentInstance;
    component.clientProjectRelationship = clientProjectRelationship;
    component.config = new HotSpotConfig('test_references_header', 'header', 10, miningTableConfig);
    fixture.detectChanges();
  });

  describe('nocontent', () => {
    beforeEach(waitForAsync(() => {
      hotspotControllerServiceSpy.getHotspots.and.returnValue(of(hotspotsNoContent) as any);
    }));

    beforeEach(() => {
      fixture = TestBed.createComponent(HotSpotCardComponent);
      component = fixture.componentInstance;
      component.clientProjectRelationship = clientProjectRelationship;
      component.config = new HotSpotConfig('test_references_header', 'header', 10, miningTableConfig);
      fixture.detectChanges();
    });

    it('should have LoaderState as nocontent', () => {
      expect(component.config.loadState).toBe(LoaderState.nocontent);
    });
  });

  describe('error', () => {
    beforeEach(waitForAsync(() => {
      hotspotControllerServiceSpy.getHotspots.and.returnValue(throwError('TEST_ERROR'));
    }));

    beforeEach(() => {
      fixture = TestBed.createComponent(HotSpotCardComponent);
      component = fixture.componentInstance;
      component.clientProjectRelationship = clientProjectRelationship;
      component.config = new HotSpotConfig('test_references_header', 'header', 10, {} as any);
      fixture.detectChanges();
    });

    it('should have LoaderState as error', () => {
      expect(component.config.loadState).toBe(LoaderState.error);
    });
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should have LoaderState as success', () => {
    expect(component).toBeDefined();
    expect(component.config.loadState).toBe(LoaderState.success);
  });

  it('should have hotspot VO data', () => {
    const hotSpotConfig = new HotSpotConfig('hotspots.mostReferencedPrograms', 'type', 10, {
      columnMap: {
        moduleName: { field: 'moduleName', header: 'moduleName', filterProperties: { filterType: FilterType.freeText }},
        language: { field: 'language', header: 'language', filterProperties: { filterType: FilterType.multiSelect }, options: [] },
        value: { field: 'count', header: 'value', filterProperties: { filterType: FilterType.numberValue }}
      },
      rows: 10,
      actions: [[{ label: 'btnLabel.view', value: 'view', disableItem: () => false }, { label: 'btnLabel.view', value: 'view', disableItem: () => false }], [{ label: 'btnLabel.view', value: 'view', disableItem: () => false }]],
      actionsWidth: '80px'
    });
    component.config = hotSpotConfig;
    component.config.modules = [new HotSpotVO(100, 2, 'VO Module', 'lang', 'VO Type', '1', 'IDENTIFIED', 'FILE_SECTION', true, 'linkHash')];
    expect(component.config.modules[0].getId()).toBe(100);
    expect(component.config.modules[0].getCount()).toBe('1');
    expect(component.config.modules[0].getLanguage()).toBe('lang');
    expect(component.config.modules[0].getModuleName()).toBe('VO Module');
    expect(component.config.modules[0].getType()).toBe('VO Type');
  });

  it('should have most referenced database tables hotspot', () => {
    expect(fixture.debugElement.nativeElement.querySelector('mn-table')).toBeTruthy();
    expect(component.config.rows).toBe(10);
    expect(component.config.header).toBe('test_references_header');
  });
  
  it('should test handleselected option of codeviwer', () => {
    let value: MiningTableOptionSelected = { optionValue: "code-viewer", data: {id: 100} };
    component.config.tableConfig.projectId = 1;
     component.handleSelectedOption(value);
     expect(openedUrl).toContain('code-viewer')
   });
 
   it('should test handleselected option of controlFlow', () => {
     let value: MiningTableOptionSelected = { optionValue: "control-flow", data: {id: 100} };
     component.config.tableConfig.projectId = 1;
      component.handleSelectedOption(value);
      expect(openedUrl).toContain('control-flow')
    });

    it('should test handleselected option of depGraph', () => {
     let value: MiningTableOptionSelected = { optionValue: "dependencies", data: {id: 100} };
      component.config.tableConfig.projectId = 1;
      component.handleSelectedOption(value);
      expect(openedUrl).toContain('dependencies')
    });
});
