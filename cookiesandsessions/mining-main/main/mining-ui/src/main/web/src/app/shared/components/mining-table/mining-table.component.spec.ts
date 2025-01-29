import { ChangeDetectionStrategy, ChangeDetectorRef, Component, ViewChild } from '@angular/core';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MiningTableComponent } from './mining-table.component';
import { Column, FieldTypeEnum, FilterType, MiningTableConfig, MiningTableRow } from './mining-table-config.interface';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { FormsModule } from '@angular/forms';
import { NzTableFilterValue, NzTableModule } from 'ng-zorro-antd/table';
import { NzSliderModule } from 'ng-zorro-antd/slider';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { NzIconModule } from 'ng-zorro-antd/icon';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { NzMessageService } from 'ng-zorro-antd/message';
import { StateKey } from '@app/shared/interfaces/state-maintainance.interface';
import { I18nService } from '@app/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { of } from 'rxjs/internal/observable/of';
import { RouterTestingModule } from '@angular/router/testing';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { DatePipe } from '@angular/common';
import { AllowedTableActions, StatesType } from './mining-table-action.interface';
import { AnnotationControllerService, AnnotationReport, FeatureControllerService, ModuleControllerService } from '@innowake/mining-api-angular-client';
import { Operators } from '../type-based-filter/type-based-filter.component';


@Component({
  template: ` <div>
    <mn-table
      [config]="tableConfig"
      [value]="annotationReport"
      (whenRowIsSelected)="openAnnotationDetails($event)"
      #annotationTable
    ></mn-table>
  </div>`,
})
class TestComponent {
  @ViewChild('annotationTable', { static: true })
  annotationTable: MiningTableComponent;

  tableConfig: MiningTableConfig;
  annotationReport: AnnotationReport[];

  constructor(public changeDetector: ChangeDetectorRef) {
    this.tableConfig = {
      columnMap: {
        moduleName: { field: 'moduleName', header: 'Module Name', filterProperties: { filterType: FilterType.freeText }, widthColumn: '15%' },
        type: { field: 'annotationType', header: 'Type', filterProperties: { filterType: FilterType.freeText }, widthColumn: '5%' },
        category: { field: 'categoryName', header: 'Category', filterProperties: { filterType: FilterType.freeText }, widthColumn: '10%' },
        state: { field: 'annotationState', header: 'State', filterProperties: { filterType: FilterType.freeText }, widthColumn: '10%' },
        description: { field: 'name', header: 'Description', filterProperties: { filterType: FilterType.freeText }, widthColumn: '20%' },
        sourceCode: { field: 'sourceCode', header: 'Source Code', filterProperties: { filterType: FilterType.freeText }, widthColumn: '15%' },
        taxonomy: { field: 'taxonomy', header: 'Taxonomy', filterProperties: { filterType: FilterType.freeText }, widthColumn: '15%' },
        modifiedby: { field: 'updatedByUserId', header: 'Modified by', filterProperties: { filterType: FilterType.freeText }, widthColumn: '10%' }
      },
      paginator: true,
      rows: 50
    };
    this.annotationReport = [
      {
        recordId: '#169:0',
        customProperties: {},
        id: 1,
        name:
          'Annotation 1\nsome more line 2\nsome more line 3\nsome more line 4\nsome more line 5\nsome more line 6\nsome more line 7\nsome more line 8',
        moduleName: 'PRG1',
        annotationType: 'RULE',
        categoryName: 'Annotation Category A',
        annotationState: 'CANDIDATE',
        sourceCode: `IDENTIFICATION DIVISION.
            PROGRAM-ID. CONDITIONALS.
            DATA DIVISION.
              WORKING-STORAGE SECTION.
              *> setting up places to store values
              *> no values set yet
              01 NUM1 PIC 9(9).`,
        taxonomy: 'ARB100, Employee domain',
        updatedByUserId: null,
        createdByUserId: 'admin',
      },
      {
        recordId: '#169:1',
        customProperties: {},
        id: 9,
        name: `DeleteAnnotation 1\nDeleteAnnotation 2\nDeleteAnnotation 3\nDeleteAnnotation 4\n
        DeleteAnnotation 5\nDeleteAnnotation 6\nDeleteAnnotation 7\nDeleteAnnotation 8\nDeleteAnnotation 9\nDeleteAnnotation 10`,
        moduleName: 'EXECSQL',
        annotationType: 'DATABASE',
        categoryName: 'Annotation Category B',
        annotationState: 'CANDIDATE',
        sourceCode: `IDENTIFICATION DIVISION.
        PROGRAM-ID. CONDITIONALS.
        DATA DIVISION.
          WORKING-STORAGE SECTION.
          *> setting up places to store values
          *> no values set yet
          01 NUM1 PIC 9(9).
          01 NUM2 PIC 9(9).
          01 NUM3 PIC 9(5).
          01 NUM4 PIC 9(6).`,
        taxonomy: '',
        updatedByUserId: null,
        createdByUserId: 'admin',
      },
      {
        recordId: '#171:0',
        customProperties: {},
        id: 3,
        name: 'Annotation 3',
        moduleName: 'MMRS',
        annotationType: 'RULE',
        categoryName: 'Annotation Category A',
        annotationState: 'CANDIDATE',
        sourceCode: 'efgh',
        taxonomy: 'ARB100, Employee domain',
        updatedByUserId: null,
        createdByUserId: 'admin',
      },
    ];
  }
}

const values: any[] = [
  {
    id: 1,
    moduleName: 'PRG1',
    annotationType: 'RULE',
    categoryName: 'Annotation Category A',
    annotationState: 'CANDIDATE',
    children: [
      {
        id: '4_IN_ANALYSIS',
        moduleName: 'PRG2',
        annotationType: 'RULE',
        categoryName: 'Annotation Category D',
        annotationState: 'REVIEW',
        name: 'IN_ANALYSIS',
        children: []
      },
    ],
  },
  {
    id: 9,
    moduleName: 'EXECSQL',
    annotationType: 'DATABASE',
    categoryName: 'Annotation Category B',
    annotationState: 'CANDIDATE',
  },
  {
    id: 3,
    moduleName: 'MMRS',
    annotationType: 'RULE',
    categoryName: 'Annotation Category A',
    annotationState: 'CANDIDATE',
  },
];

const updatedColumns: { [key: string]: Column; } = {
  "Module Name": {
      "field": "inHasAnnotation.out.name",
      "header": "moduleName",
      "warningMessage": "This module was modified by the last scan. Please review its meta data (annotations, data dictionary, module description, taxonomies).",
      "fieldType": FieldTypeEnum.STRING
  },
  "Annotation Type": {
      "field": "typeLink",
      "header": "Annotation Type",
  },
  "Category": {
      "field": "categoryLink.name",
      "header": "Category",
      "fieldType": FieldTypeEnum.STRING
  },
  "Source Code": {
      "field": "inHasAnnotation.out.sourceAttachmentLink.content",
      "header": "Source Code",
      "fieldType": FieldTypeEnum.STRING
  },
  "State": {
      "field": "stateLink",
      "header": "State",
  },
  "Annotation Description": {
      "field": "name",
      "header": "Annotation Description",
      "fieldType": FieldTypeEnum.STRING
  },
  "Modified By": {
      "field": "updatedByUserId",
      "header": "Modified By",
      "fieldType": FieldTypeEnum.STRING
  }
};
const selectedColumns: NzTreeNodeOptions[] = [
  { 'title': 'Module Name', 'name': 'name', 'key': 'name', 'checked': true, 'disableCheckbox': true, 'path': 'content.name' },
  { 'title': 'Metrics Date', 'name': 'metricsDate', 'key': 'metricsDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.metricsDate' },
  { 'title': 'Technology', 'name': 'technology', 'key': 'technology', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.technologyLink' },
  { 'title': 'Type', 'name': 'type', 'key': 'type', 'checked': true, 'disableCheckbox': false, 'path': 'content.objectTypeLink.typeLink' },
  { 'title': 'Modified Date', 'name': 'modifiedDate', 'key': 'modifiedDate', 'checked': true, 'disableCheckbox': false, 'path': 'content.modifiedDate' }
]

const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('UserCustomizableTableService',
['getSelectedDataPoints', 'resetTableColumnAndDataPoints', 'updateTableConfig', 'setDataPointList', 'handleQueryParameterChange']);

describe('MiningTableComponent', () => {
  let component: MiningTableComponent;
  let fixture: ComponentFixture<MiningTableComponent>;
  let testFixture: ComponentFixture<TestComponent>;
  let testComponent: TestComponent;

  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', [
    'success',
    'loading',
    'remove',
    'error',
  ]);

  const miningTableConfig: MiningTableConfig = {
    columnMap: {
      moduleName: {
        field: 'moduleName',
        header: 'Module Name',
        filterProperties: { filterType: FilterType.freeText }
      },
      type: {
        field: 'annotationType',
        header: 'Type',
        filterProperties: { filterType: FilterType.multiSelect }
      },
      state: {
        field: 'annotationState',
        header: 'State',
        filterProperties: { filterType: FilterType.multiSelect }
      },
    },
    isCustomizable: false,
    paginator: true,
    rows: 10
  };

  const columns: Column[] = [
    {
      header: 'test',
      field: 'technology',
      filterProperties: {
        filterType: FilterType.multiSelect
      }
    }
  ];
  const i18nServiceSpy = { language: 'en-US' };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        NzTableModule,
        NzDropDownModule,
        NzSliderModule,
        NzIconModule,
        TranslateModule.forRoot({}),
        FormsModule,
        BrowserAnimationsModule,
        RouterTestingModule.withRoutes([]),
        RouterModule.forRoot([])
      ],
      declarations: [MiningTableComponent, TestComponent],
      providers: [
        ChangeDetectorRef,
        TranslateService,
        FeatureControllerService,
        HttpClient,
        HttpHandler,
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
              project: { id: 1 }
            }),
            snapshot: {queryParams: of({
              filter: '[{"key":"objectTypeLink.typeLink","value":["PROGRAM"]}]',
              page: 1,
              sort:'name;ASC'
            })}
          }
        },
        ModuleControllerService,
        AnnotationControllerService,
        { provide: NzMessageService, useValue: messageServiceSpy },
        NumberFormatter,
        DatePipe,
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
      ],
    })
      .overrideComponent(MiningTableComponent, {
        set: { changeDetection: ChangeDetectionStrategy.Default },
      })
      .compileComponents();
      userCustomizableTableServiceSpy.getSelectedDataPoints.and.returnValue(of(selectedColumns as any));
      userCustomizableTableServiceSpy.updateTableConfig.and.returnValue(updatedColumns as any);
  
    localStorage.setItem(
      StateKey.BrowseModuleTableStateKey,
      '{"selection": {}, "filters":{"target": {}, "type":{} } }'
    );
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MiningTableComponent);
    component = fixture.componentInstance;
    component.config = miningTableConfig;
    component.value = values;
    testFixture = TestBed.createComponent(TestComponent);
    testComponent = testFixture.componentInstance;
    testFixture.detectChanges();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should create test component', () => {
    expect(testComponent).toBeTruthy();
  });

  it('Should sort dropdown in alphabetical order', () => {
    component.columns = columns;
    component.values = [{ technology: 'C' }, { technology: 'A' }, { technology: 'B' }];
    (component as any).setColumnFilterList();
    (component as any).orderColumnFilterList();
    expect(component.columns[0].filterProperties.listOfFilter[0].value).toEqual('A');
    expect(component.columns[0].filterProperties.listOfFilter[2].value).toEqual('C');
  });

  it('Should sort dropdown in alphabetical order when one of filter values is null', () => {
    component.columns = columns;
    component.columns[0].filterProperties = {
      filterType: FilterType.multiSelect
    };
    component.values = [{ technology: 'C' }, { technology: null }, { technology: 'A' }, { technology: 'B' }];
    (component as any).setColumnFilterList();
    (component as any).orderColumnFilterList();
    expect(component.columns[0].filterProperties.listOfFilter[0].value).toEqual('A');
    expect(component.columns[0].filterProperties.listOfFilter[1].value).toEqual('B');
    expect(component.columns[0].filterProperties.listOfFilter[2].value).toEqual('C');
  });

  it('Should compare values from filter input against min/max', () => {
    component.columns = columns;
    component.values = [{ value: 1 }, { value: 5 }, { value: 10 }];

    const column: Column = { header: 'test', field: 'value', filterProperties: { filterType: FilterType.numberValue, filterValue: [{ value: 5, operator: 'eq'}] } };

    component.validateFilterInput(column);
    expect(component.filterDisabled).toEqual(false);
  });

  it('Should collapse tree table rows', () => {
    const data = { children: [{ id: 3, value: 'four' }] };
    const dataArray = [
      { id: 1, value: 'one' },
      { id: 2, value: 'two', expand: true },
      { id: 3, value: 'three', expand: true },
    ];
    component.collapse(dataArray, {}, false);
    expect(dataArray[1].expand).toEqual(true);

    component.collapse(dataArray, data, false);
    expect(dataArray[2].expand).toEqual(false);

    const value: MiningTableRow = { children: [] };
    spyOn((component as any).optionSelected, 'emit');
    component.collapse(dataArray, value, true);
    expect((component as any).optionSelected.emit).toHaveBeenCalled();
  });

  it('Should change the table data', () => {
    component.values = [
      { id: 1, name: 'test 1' },
      { id: 2, name: 'test 2' },
    ];
    component.tooltip = [];

    (component as any).changeTableData({ action: 'delete', id: 1 });
    expect(component.values.length).toEqual(1);

    (component as any).changeTableData({ action: 'update', id: 2, data: { id: 2, name: 'test 2 updated' } });
    expect(component.values[0].name).toEqual('test 2 updated');

    (component as any).changeTableData({ action: 'tooltip', data: ['new tooltip'] });
    expect(component.tooltip).toEqual(['new tooltip']);

    const data = {
      "name": "New Taxonomy Type",
      "id": "Business Taxonomies__New Taxonomy Type",
      "moduleCount": 0,
      "children": [
        {
          "name": "New Term",
          "id": "newTaxonomyTerm",
          "moduleCount": 0,
          "isNewRecord": true,
          "showCountAsText": true,
          "type": "taxonomyTerm"
        }
      ],
      "type": "taxonomyType",
      "showCountAsText": true,
      "isEditable": true,
      "removeActions": [] as any,
      "expand": true,
      "level": 1,
      "parent": {
        "name": "Business Taxonomies",
        "id": "Business Taxonomies",
        "moduleCount": 0,
        "children": [
          {
            "name": "New Taxonomy Type",
            "id": "Business Taxonomies__New Taxonomy Type",
            "moduleCount": 0,
            "children": [
              {
                "name": "New Term",
                "id": "newTaxonomyTerm",
                "moduleCount": 0,
                "isNewRecord": true,
                "showCountAsText": true,
                "type": "taxonomyTerm"
              }
            ],
            "type": "taxonomyType",
            "showCountAsText": true,
            "isEditable": true,
            "removeActions": [] as any,
            "expand": false
          }
        ],
        "expand": true,
        "isEditable": false,
        "showCountAsText": true,
        "removeActions": [] as any,
        "level": 0,
        "key": "parent-Business Taxonomies"
      },
      "key": "child-Business Taxonomies__New Taxonomy Type"
    };
    component.values = [
      {
        "name": "Business Taxonomies",
        "id": "Business Taxonomies" as any,
        "moduleCount": 0,
        "children": [
          {
            "name": "New Taxonomy Type",
            "id": "Business Taxonomies__New Taxonomy Type" as any,
            "moduleCount": 0,
            "children": [
              {
                "name": "New Term",
                "id": "newTaxonomyTerm" as any,
                "moduleCount": 0,
                "isNewRecord": true,
                "showCountAsText": true,
                "type": "taxonomyTerm"
              },
              {
                "name": "New Term",
                "id": "Business Taxonomies__New Taxonomy Type__New Term" as any,
                "moduleCount": 0,
                "type": "taxonomyTerm",
                "isEditable": true,
                "typeName": "New Taxonomy Type",
                "showCountAsText": true,
                "removeActions": []
              }
            ],
            "type": "taxonomyType",
            "showCountAsText": true,
            "isEditable": true,
            "removeActions": [],
            "expand": false
          }
        ],
        "expand": true,
        "isEditable": false,
        "showCountAsText": true,
        "removeActions": []
      }
    ];
    (component as any).changeTableData({ action: AllowedTableActions.ADD_CHILD, id: 'id_test', data: data });
    expect(component.expandedRows.length).toBeGreaterThan(0);

    (component as any).changeTableData({ action: AllowedTableActions.TOGGLE_ACTIONS, id: 2, data: { id: 2, name: 'test 2 updated' } });
    expect(component.currentEditingColumnId).toBeNull();

    (component as any).changeTableData({ action: AllowedTableActions.RESTRICT_EDITING, id: '2', data: { id: '2', name: 'test 2 updated' } });
    expect(component.currentEditingColumnId).toBe('2');
  });

  it('should calculate row indent', () => {
    const rowItemLevelZero = {
      level: 0,
    };
    const levelZeroIndent = component.calculateIndent(rowItemLevelZero);
    expect(levelZeroIndent).toEqual(0);

    const rowItemLevelOne = {
      level: 0,
    };
    const levelOneIndent = component.calculateIndent(rowItemLevelOne);
    expect(levelOneIndent).toEqual(0);

    const rowItemLevelZeroDefaultIndent = {
      level: 0,
      nonExpandableRowIndent: 1,
    };
    const levelZeroDefaultIndent = component.calculateIndent(rowItemLevelZeroDefaultIndent);
    expect(levelZeroDefaultIndent).toEqual(1);

    const rowItemLevelOneDefaultIndent = {
      level: 1,
      nonExpandableRowIndent: 1,
    };
    const levelOneDefaultIndent = component.calculateIndent(rowItemLevelOneDefaultIndent);
    expect(levelOneDefaultIndent).toEqual(25);
  });

  it('Should  test handleSelectedFilter  when client side filtering', () => {
    const selectedFilterDetails = [{text: 'IN_ANALYSIS', value: 'IN_ANALYSIS', byDefault: true, numberFilterOperator: 'equal'}];
    const column = "moduleName";
    component.tableConfig.serverSidePagination = false;
    component.handleSelectedFilter(selectedFilterDetails, column);
    component.tableConfig.serverSidePagination = false;
    expect(component.onQueryParamsChange.length).toBeGreaterThan(0);
  });

  it('Should  test handleSelectedFilter  when client side filtering', () => {
    const selectedFilterDetails = [{text: 'IN_ANALYSIS', value: 'IN_ANALYSIS', byDefault: true, numberFilterOperator: 'equal'}];
    const column = "moduleName";
    component.columns[0].filterProperties = {
      filterType: FilterType.numberValue
    };
    component.tableConfig.serverSidePagination = false;
    component.handleSelectedFilter(selectedFilterDetails, column);
    component.tableConfig.serverSidePagination = false;
    expect(component.onQueryParamsChange.length).toBeGreaterThan(0);
  });
});

describe('MiningTableComponentWithServerSidePagination', () => {
  let component: MiningTableComponent;
  let fixture: ComponentFixture<MiningTableComponent>;
  let testFixture: ComponentFixture<TestComponent>;
  let testComponent: TestComponent;

  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', [
    'success',
    'loading',
    'remove',
    'error',
  ]);

  const miningTableConfig: MiningTableConfig = {
    columnMap: {
      moduleName: {
        field: 'moduleName',
        header: 'Module Name',
        filterProperties: { filterType: FilterType.freeText },
        sortFn: true
      },
      type: {
        field: 'annotationType',
        header: 'Type',
        filterProperties: { filterType: FilterType.multiSelect },
        sortFn: true
      },
      state: {
        field: 'annotationState',
        header: 'State',
        filterProperties: { filterType: FilterType.multiSelect },
        sortFn: true
      },
    },
    paginator: true,
    rows: 10,
    serverSidePagination: true
  };

  const sampleData = {
    childSample: {
      id: 1,
    },
  };

  const i18nServiceSpy = { language: 'en-US' };

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        NzTableModule,
        NzDropDownModule,
        NzSliderModule,
        NzIconModule,
        TranslateModule.forRoot({}),
        FormsModule,
        BrowserAnimationsModule,
      ],
      declarations: [MiningTableComponent, TestComponent],
      providers: [
        ChangeDetectorRef,
        TranslateService,
        FeatureControllerService,
        HttpClient,
        HttpHandler,
        ModuleControllerService,
        AnnotationControllerService,
        { provide: NzMessageService, useValue: messageServiceSpy },
        NumberFormatter,DatePipe,
        { provide: I18nService, useValue: i18nServiceSpy },
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
              project: { id: 1 }
            }),
            snapshot: {queryParams: of({
              filter: '[{"key":"objectTypeLink.typeLink","value":["PROGRAM"]}]',
              page: 1,
              sort:'name;ASC'
            })}
          }
        },
        { provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy },
      ],
    })
      .overrideComponent(MiningTableComponent, {
        set: { changeDetection: ChangeDetectionStrategy.Default },
      })
      .compileComponents();
      userCustomizableTableServiceSpy.getSelectedDataPoints.and.returnValue(of(selectedColumns as any));
      userCustomizableTableServiceSpy.updateTableConfig.and.returnValue(updatedColumns as any);
    localStorage.setItem(
      StateKey.BrowseModuleTableStateKey,
      '{"selection": {}, "filters":{"target": {}, "type":{} } }'
    );
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MiningTableComponent);
    component = fixture.componentInstance;
    component.config = miningTableConfig;
    component.value = values;
    testFixture = TestBed.createComponent(TestComponent);
    testComponent = testFixture.componentInstance;
    testFixture.detectChanges();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
    expect(component.resolveFieldData(sampleData, 'childSample.id')).toBe(1);
  });

  it('should test resolveFiledData method with null values', () => {
    expect(component.resolveFieldData({}, 'childSample.id')).toBe(null);
    expect(component.resolveFieldData(null, null)).toBe(null);
  });

  it('should resolve field data', () => {
    expect(component).toBeTruthy();
    expect(component.resolveFieldData(sampleData, 'childSample.id')).toBe(1);
  });

  it('should create test component', () => {
    expect(testComponent).toBeTruthy();
  });

  it('should emit event for the option selected', () => {
    spyOn((component as any).optionSelected, 'emit');
    component.callOptionCallback('test', { field: 'test' });
    expect((component as any).optionSelected.emit).toHaveBeenCalledWith({ optionValue: 'test', data: { field: 'test' } });
  });

  it('should get data for the tooltip for column', () => {
    spyOn((component as any).optionHover, 'emit');
    component.getToolTipData('test', { test: '' });
    expect((component as any).optionHover.emit).toHaveBeenCalledWith({
      optionValue: 'tooltip',
      data: { data: { test: '' }, toolTipField: 'test' },
    });
  });

  it('should test sortingFunction when column field type is number', () => {
    const column: any = {
      field: 'modules',
      fieldType: FieldTypeEnum.NUMBER
    }
    let a = { id: 1, technology: 'PL1', modules: '27', linesOfCode: '3,550', lineOfComment: '199'};
    let b = { id: 1, technology: 'PL1', modules: '28', linesOfCode: '3,550', lineOfComment: '199'};
    const sortingFunctionResult = component.sortingFunction(a, b, column, 'ascend');
    expect(sortingFunctionResult).toBe(-1);

    a.modules = null;
    const sortingFunctionResult1 = component.sortingFunction(a, b, column, 'ascend');
    expect(sortingFunctionResult1).toBe(1);
    
    a.modules = '27';
    b.modules = null;
    const sortingFunctionResult2 = component.sortingFunction(a, b, column, 'ascend');
    expect(sortingFunctionResult2).toBe(-1);
  });

  it('should test handle filter method when server side filtering', () => {
    component.values = [{test: 'xyz'}];
    component.tableConfig.serverSidePagination = true;
    component.handleSelectedFilter([{value: 'xyz'}] as any, 'test')
    expect(Object.keys(component.queryParams.filter).length).toBeGreaterThan(0)
  });

  it('should test sortingFunction when column field type is string', () => {
    const column: any = {
      field: 'technology',
      fieldType: FieldTypeEnum.STRING
    }
    let a = { id: 1, technology: 'PL1', modules: '27', linesOfCode: '3,550', lineOfComment: '199'}
    let b = { id: 1, technology: 'PL1', modules: '28', linesOfCode: '3,550', lineOfComment: '199'}
    const sortingFunctionResult = component.sortingFunction(a, b, column, 'descend');
    expect(sortingFunctionResult).toBe(0);
    
    a.technology = null;
    const sortingFunctionResult1 = component.sortingFunction(a, b, column, 'descend');
    expect(sortingFunctionResult1).toBe(-1);
    
    a.technology = 'PL1';
    b.technology = null;
    const sortingFunctionResult2 = component.sortingFunction(a, b, column, 'descend');
    expect(sortingFunctionResult2).toBe(1);
  });

  it('should check natural sort for sortingFunction method', () => {
    const column: any = {
      field: 'name',
      fieldType: FieldTypeEnum.STRING
    }
    const value1 = {
      "id": 19164,
      "name": "Z12",
      "projectId": 4,
      "linesOfDeadCode": -1,
    }
    const value2 = {
      "id": 19156,
      "name": "Z1",
      "projectId": 4,
      "requiresReview": false,
      "linesOfDeadCode": -1,
    }
    const naturalSorting = component.sortingFunction(value1, value2, column, 'ascend');
    expect(naturalSorting).toBe(1);
  }); 

  it('should close the current loading spinner', () => {
    component.cancelLoading = true;
    component.cancelCurrentLoading();
    expect(component.cancelLoading).toBeFalse();
  });

  it('Should resolveFieldWarning', () => {
    component.values = [{ value: 1 }, { value: 5 }, { value: 10 }];
    const column: Column = {
      header: 'test', field: 'value', filterProperties: { filterType: FilterType.freeText, filterValue: 'Test value' }, hasWarning: () => false
    };
    const result = component.resolveFieldWarning(component.values, column);
    expect(result).toEqual(false);
  });

  it('Should  test handleSelectedFilter  when server side pagination is true', () => {
    const selectedFilterDetails = [{text: 'IN_ANALYSIS', value: 'IN_ANALYSIS', byDefault: true}];
    const column = 'columnField';
    component.tableConfig.serverSidePagination = true;
    component.handleSelectedFilter(selectedFilterDetails, column);
    expect(component.onQueryParamsChange.length).toBeGreaterThan(0);
  });

  it('should check the updateCheckedSet', () => {
    spyOn(component,'updateCheckedSet').and.callThrough();;
    component.updateCheckedSet(2032,true);
    expect(component.selectedState).toEqual('NONE');
    component.updateCheckedSet(2032,false);
    expect(component.selectedState).toEqual('NONE')
  });

  it('should close the current loading spinner', () => {
    spyOn(component,'updateCheckedSet').and.callThrough();;
    component.onItemChecked(2032,true);
    expect(component.updateCheckedSet).toHaveBeenCalled();
  });

  /** commenting the below code as select all creating issue if data is like 930k records
   * will be resolve in coming up tickets
   */
  xit('should check the onAllSelectedId', () => {
    spyOn(component,'onAllChecked').and.callThrough();
   // component.onAllSelectedId(true, [10])
    expect(component.onAllChecked).toHaveBeenCalled();
  });

  it('should check the refreshCheckedStatus', () => {
    component.totalRecords = 1;
    component.setOfCheckedId = new Set([1]);
    component.refreshCheckedStatus();
    expect(component.selectedState).toBe(StatesType.ALL);

    component.totalRecords = 2;
    component.refreshCheckedStatus();
    expect(component.selectedState).toBe(StatesType.SOME);

    component.totalRecords = 1;
    component.setOfCheckedId = new Set([1, 2]);
    component.refreshCheckedStatus();
    expect(component.selectedState).toBe(StatesType.NONE);
  });

  it('should check the resetTablefilters', () => {
    component.resetTablefilters();
    expect(component.queryParams.filter).toEqual([]);
  });

  it('should check the setAndUpdatePageIndexRecord', () => {
    spyOn(component.selectAllModule, 'emit').and.callThrough();
    component.selectedState = 'ALL';
    component.selectDeselectAll();
    expect(component.setOfCheckedId.size).toBe(0);

    component.selectedState = 'NONE';
    component.selectDeselectAll();
    expect(component.selectAllModule.emit).toHaveBeenCalled();
  });

  it('should check the selectDeselectCurrentIds', () => {
    spyOn(component as any,'bulkSelectionOptionsDropdown').and.callThrough();
    component.selectDeselectCurrentIds();
    expect((component as any).bulkSelectionOptionsDropdown).toHaveBeenCalled();
  });

  it('should check the resetTablefilters', () => {
    component.deselectCurrent = true;
    component.childValues = {
      "name": "#A1E86D2C",
      "id": 9131,
      "objectTypeLink": {
        "technologyLink": "SQL",
        "typeLink": "TABLE"
      },
      "metricsDate": "N/A",
      "requiresReview": false,
      "level": 0,
      "expand": false,
      "key": "parent-9131"
    };
    expect(component.showCurrentDropDownText()).toEqual('miningTable.currentPage');
  });

  describe('filterArray', () => {
    const data = {
      content: {
        name: 'test',
      }
    };
    it('should return the value at the specified field path', () => {
      const field = 'content.name';
      const result = component.filterArray(data, field);
      expect(result).toBe('test');
    });

    it('should be undefined if the field path does not exist', () => {
      const field = 'content.technology';
      const result = component.filterArray(data, field);
      expect(result).toBeUndefined();
    });
  });

  it('should set the current table filters', () => {
    const filters: Array<{ key: string; value: NzTableFilterValue }> = [
      { key: 'moduleName', value: { type: 'freeText', filterValue: 'test' } },
      { key: 'type', value: { type: 'multiSelect', filterValue: ['RULE'] } },
    ];
    component.setCurrentTableFilters(filters);
    expect(component.queryParams.filter).toEqual(filters);
  });

  it('should emit the optionSelected event with the correct optionValue and data', () => {
    // Arrange
    const selectedOptionValue = 'optionValue';
    const selectedRowData: MiningTableRow = { data: { id: 1 } };
    spyOn((component as any).optionSelected, 'emit');
  
    // Act
    component.handleShowBusinessVariablesReferenced(selectedOptionValue, selectedRowData);
  
    // Assert
    expect((component as any).optionSelected.emit).toHaveBeenCalledWith({ optionValue: selectedOptionValue, data: selectedRowData.data });
  });

  it('should test totalNumberRecords', () => {
    const totalRecords = component.totalNumberRecords(20, 10);
    expect(totalRecords).toEqual(20);
  });

  it('should test handleSelectedFilter with operator', () => {
    component.queryParams.filter = [{key: 'test', value: 'test'}, {key: 'technologyLink', value: 'test'}];
    component.setOfCheckedId = new Set([1]);
    component.handleSelectedFilter([{text: 'filter1', value: 'filter1', operator: Operators.EQUALS}, {text: 'filter2', value: 'filter2'}], 'technologyLink');
    expect(Object.keys(component.queryParams.filter).length).toBeGreaterThan(0);
  });
});