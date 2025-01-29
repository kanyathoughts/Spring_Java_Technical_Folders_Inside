import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { BulkAction, Column, FilterType, MiningTableConfig } from '../mining-table-config.interface';
import { MiningTableTitleComponent } from './mining-table-title.component';
import { of } from 'rxjs/internal/observable/of';
import { DatePipe, formatDate } from '@angular/common';
import { RouterTestingModule } from '@angular/router/testing';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import FileSaveSupport from '@app/core/utils/file-save-support.utils';
import { FeatureControllerService, JobControllerService } from '@innowake/mining-api-angular-client';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SharedModule } from '@app/shared/shared.module';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';

const tableConfig: MiningTableConfig = {
  columnMap: {},
  paginator: true,
  rows: 50,
  exportParameters: {},
  externalExportCallback: () => null || undefined
};

const columns: Column[] = [
  {
    field: 'moduleName',
    header: 'Module Name',
    filterProperties: { filterType: FilterType.freeText },
    sortFn: true
  },
  {
    field: 'annotationType',
    header: 'Type',
    filterProperties: { filterType: FilterType.multiSelect },
    sortFn: true
  },
  {
    field: 'annotationState',
    header: 'State',
    filterProperties: { filterType: FilterType.multiSelect },
    sortFn: true
  }
];

const selectedColumns: NzTreeNodeOptions[] = [
  { 'title': 'Module Name', 'name': 'name', 'key': 'name', 'checked': true, 'disableCheckbox': true, 'path': 'content.name' },
  { 'title': 'Metrics Date', 'name': 'metricsDate', 'key': 'metricsDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.metricsDate' },
  { 'title': 'Technology', 'name': 'technology', 'key': 'technology', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.technologyLink' },
  { 'title': 'Type', 'name': 'type', 'key': 'type', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.typeLink' },
  { 'title': 'Modified Date', 'name': 'modifiedDate', 'key': 'modifiedDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.modifiedDate' }
]

describe('MiningTableTitleComponent', () => {
  let component: MiningTableTitleComponent;
  let fixture: ComponentFixture<MiningTableTitleComponent>;
  let translateService: TranslateService;
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', [
    'success',
    'loading',
    'remove',
    'error',
  ]);

  const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('CustomizableTableColumnService',
    ['getSelectedDataPoints', 'resetTableColumnAndDataPoints', 'updateTableConfig', 'setDataPointList', 'handleQueryParameterChange']);

  const customizableTableParameterServiceSpy: jasmine.SpyObj<CustomizableTableParametersService> = jasmine.createSpyObj('CustomizableTableParametersService',
    ['getSortObject', 'getFilterObject', 'getFilterObjectString', 'getFilterObject', 'handleFilters']);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [MiningTableTitleComponent],
      imports: [
        TranslateModule.forRoot({}),
        HttpClientTestingModule,
        RouterTestingModule.withRoutes([]),
        AntDesignImportsModule,
        SharedModule
      ],
      providers: [
        FeatureControllerService,
        DatePipe,
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() },
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
        { provide: CustomizableTableParametersService, useValue: customizableTableParameterServiceSpy},
        JobControllerService,
        TranslateService
      ]
    }).compileComponents();
    userCustomizableTableServiceSpy.getSelectedDataPoints.and.returnValue(of(selectedColumns as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MiningTableTitleComponent);
    translateService = TestBed.inject(TranslateService);
    component = fixture.componentInstance;
    component.tableConfig = tableConfig;
    component.columns = columns;
    component.total = 10;
    component.selectedRecords = new Set();
  });

  it('Should get a valid url', () => {
    component.tableConfig.projectId = 1;
    component.tableConfig.isCustomizable = false;
    component.tableConfig.exportType = 'modules';
    expect(JSON.stringify((component as any).buildParams())).toEqual(
      '{"type":"modules","columns":["Module Name","Type","State"]}'
    );
  });

  it('Should get a valid url with moduleid', () => {
    component.tableConfig.projectId = 1;
    component.tableConfig.exportParameters = {
      moduleId: 2000
    }
    component.tableConfig.isCustomizable = true;
    component.tableConfig.exportType = 'modules';
    expect(JSON.stringify((component as any).buildParams())).toEqual(
      '{"$query":["modules"],"$columns":["content.name","content.metricsDate","content.objectTypeLink.technologyLink","content.objectTypeLink.typeLink","content.modifiedDate"],"filterObject":[null],"moduleId":[2000]}'
    );
  });

  it('Should get a valid url with moduleid and subtype', () => {
    component.tableConfig.projectId = 1;
    component.tableConfig.exportParameters = {
      moduleId: 2000,
      subType: 'annotations'
    }
    component.tableConfig.exportType = 'modules';
    component.tableConfig.isCustomizable = false;
    expect(JSON.stringify((component as any).buildParams())).toEqual(
      '{"type":"modules","columns":["Module Name","Type","State"],"moduleId":2000,"subType":"annotations"}'
    );
  });

  it('Should get a valid url with filters', () => {
    component.tableConfig.projectId = 1;
    component.tableConfig.exportType = 'modules';
    component.tableConfig.filters = 'objectTypeLink.typeLink.name=in=(COBOL)';
    component.tableConfig.isCustomizable = false
    expect(JSON.stringify((component as any).buildParams())).toEqual(
      '{"type":"modules","columns":["Module Name","Type","State"],"filters":"objectTypeLink.typeLink.name=in=(COBOL)","moduleId":2000,"subType":"annotations"}'
    );
  });

  it('should clear the tooltip for the clicked option', () => {
    const bulkAction: BulkAction = { label: 'Option 1', tooltip: 'Tooltip Text' };
    component.tableConfig.bulkActions = [bulkAction]
    component.clearTooltip(bulkAction);
    expect(bulkAction.tooltip).toBe('');
  });

  it('should not clear the tooltip for an option if no bulkActions are defined', () => {
    const bulkAction: BulkAction = { label: 'Option 1', tooltip: 'Tooltip Text' };
    component.tableConfig.bulkActions = [];
    component.clearTooltip(bulkAction);
    expect(bulkAction.tooltip).toBe('Tooltip Text');
  });

  it('export external file path', () => {
    component.tableConfig.projectId = 1;
    component.tableConfig.exportType = 'modules';
    component.tableConfig.filters = 'objectTypeLink.typeLink.name=in=(COBOL)';
    component.tableConfig.exportParameters = {
      moduleId: 2000,
      subType: 'annotations'
    }
    component.tableConfig.isCustomizable = true;
    component.tableConfig.isClientSideExport = true;
    (component as any).translateService.currentLang = 'en-US';
    spyOn((component as any), 'clientSideExport');
    spyOn(FileSaveSupport, 'save');
    component.export('fileformat');
    expect((component as any).clientSideExport).toHaveBeenCalled();
    const fileName = (component as any).getExportFileName(component.tableConfig.exportType, component.tableConfig.projectId, 'fileFormat');
    const formattedDate = formatDate(new Date(), 'yyyyMMddhhmmss', (component as any).translateService.currentLang);
    const expectedFileName = [component.tableConfig.exportType, component.tableConfig.projectId, formattedDate].join('_') + '.fileFormat';
    expect(fileName).toBe(expectedFileName);
  });

  it('should not modify the tooltip if the conditions are not met', () => {
    const bulkAction: BulkAction = {
      label: 'SomeOtherLabel',
      subActions: [
        {
          label: 'Identify Candidates',
          tooltip: 'Old Tooltip',
        },
      ],
    };
    const instantSpy = spyOn(translateService, 'instant');
    component.restoreTooltip(bulkAction);
    expect(bulkAction.subActions[0].tooltip).toBe('Old Tooltip');
  });

  it('should disable bulk-action button if no module is selected', () => {
    component.tableConfig.bulkActions = [{ label: 'a', id: 'generate-annotation-descriptions-from-module' }, { label: 'b', id: 'generate-module-descriptions' }];
    component.selectedRecords = new Set();
    fixture.detectChanges();
    const annotationbutton = fixture.debugElement.nativeElement.querySelector('#generate-annotation-descriptions-from-module');
    const modulebutton = fixture.debugElement.nativeElement.querySelector('#generate-module-descriptions');
    expect(annotationbutton.disabled).toBeTruthy();
    expect(modulebutton.disabled).toBeTruthy();
  });

  it('should build params with sort object', () => {
    customizableTableParameterServiceSpy.getSortObject.and.returnValue('key: value');
    const params = (component as any).buildParams();
    expect(params['sortObject']).toBeDefined();
  });

  xit('should build params with sort object null', () => {
    customizableTableParameterServiceSpy.getSortObject.and.returnValue(null);
    const params = (component as any).buildParams();
    expect(params['sortObject']).toBeUndefined();
  });

  it('Should get a valid url with moduleId and columns', () => {
    component.tableConfig.projectId = 1;
    component.moduleIdForPreFiltering = [2000];
    component.tableConfig.exportType = 'annotations';
    const actual = (component as any).buildParamsForBusinessRules();
    const expected = {
      "$query": "annotations",
      "$columns": ["content.name", "content.metricsDate", "content.objectTypeLink.technologyLink", "content.objectTypeLink.typeLink", "content.modifiedDate", "content.sourceAttachment"],
      "moduleId": '2000'
    };
    expect(actual).toEqual(jasmine.objectContaining(expected));
  });

});
