import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { UntypedFormBuilder, Validators } from '@angular/forms';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { FieldTypeEnum, ViewMode } from '../mining-table-config.interface';
import { DisplayValueAsPerTypeComponent } from './display-value-as-per-type.component';
import { LinkType } from '../mining-table-action.interface';
import { Params } from '@angular/router';
import { I18nService } from '@app/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';

describe('StringRepeaterComponent', () => {
  let component: DisplayValueAsPerTypeComponent;
  let fixture: ComponentFixture<DisplayValueAsPerTypeComponent>;
  let translateService: TranslateService;
  const editOptions = {
    enableEditing: false,
    validations: [Validators.required],
    validationMessages: {'required' : 'Mandatory Field'},
    onEditingStart: (data: any) => {},
    onCancel: (data: any) => {},
    onSubmit:(data: any) => {}
  };
  
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [DisplayValueAsPerTypeComponent],
      imports: [TranslateModule.forRoot({})],
      providers: [UntypedFormBuilder, NumberFormatter, I18nService, TranslateService]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(DisplayValueAsPerTypeComponent);
    translateService = TestBed.inject(TranslateService);
    component = fixture.componentInstance;
    component.stringList = ['z1', 'z11', 'z2'];
    component.data = {
      name: "File Access",
      id: "Technical Taxonomies__File Access",
      moduleCount: 3,
      children: [
          {
              "name": "Read",
              "id": "Technical Taxonomies__File Access__Read",
              "moduleCount": 3,
              "type": "taxonomyTerm",
              "isEditable": false,
              "typeName": "File Access"
          },
          {
              "name": "Write",
              "id": "Technical Taxonomies__File Access__Write",
              "moduleCount": 1,
              "type": "taxonomyTerm",
              "isEditable": false,
              "typeName": "File Access"
          }
      ],
      type: "taxonomyType",
      isEditable: false,
      removeActions: [
          "addTaxonomyTerm"
      ],
      expand: false,
      level: 1,
      parent: {
          name: "Technical Taxonomies",
          id: "Technical Taxonomies",
          moduleCount: 137,
          children: [
              {
                  name: "DB Access",
                  id: "Technical Taxonomies__DB Access",
                  moduleCount: 137,
                  children: [
                      {
                          name: "Delete",
                          id: "Technical Taxonomies__DB Access__Delete",
                          moduleCount: 137,
                          type: "taxonomyTerm",
                          isEditable: false,
                          typeName: "DB Access"
                      },
                      {
                          name: "Read",
                          id: "Technical Taxonomies__DB Access__Read",
                          moduleCount: 2,
                          type: "taxonomyTerm",
                          isEditable: false,
                          typeName: "DB Access"
                      },
                      {
                          name: "Store",
                          id: "Technical Taxonomies__DB Access__Store",
                          moduleCount: 0,
                          type: "taxonomyTerm",
                          isEditable: false,
                          typeName: "DB Access"
                      },
                      {
                          name: "Update",
                          id: "Technical Taxonomies__DB Access__Update",
                          moduleCount: 0,
                          type: "taxonomyTerm",
                          isEditable: false,
                          typeName: "DB Access"
                      }
                  ],
                  type: "taxonomyType",
                  isEditable: false,
                  removeActions: [
                      "addTaxonomyTerm"
                  ],
                  expand: false
              },
              {
                  name: "File Access",
                  id: "Technical Taxonomies__File Access",
                  moduleCount: 3,
                  children: [
                      {
                          name: "Read",
                          id: "Technical Taxonomies__File Access__Read",
                          moduleCount: 3,
                          type: "taxonomyTerm",
                          isEditable: false,
                          typeName: "File Access"
                      },
                      {
                          name: "Write",
                          id: "Technical Taxonomies__File Access__Write",
                          moduleCount: 1,
                          type: "taxonomyTerm",
                          isEditable: false,
                          typeName: "File Access"
                      }
                  ],
                  type: "taxonomyType",
                  isEditable: false,
                  removeActions: [
                      "addTaxonomyTerm"
                  ],
                  expand: false
              },
              {
                  name: "Program Type",
                  id: "Technical Taxonomies__Program Type",
                  moduleCount: 12,
                  children: [
                      {
                          name: "Batch",
                          id: "Technical Taxonomies__Program Type__Batch",
                          moduleCount: 4,
                          type: "taxonomyTerm",
                          isEditable: false,
                          typeName: "Program Type"
                      },
                      {
                          name: "Library",
                          id: "Technical Taxonomies__Program Type__Library",
                          moduleCount: 6,
                          type: "taxonomyTerm",
                          isEditable: false,
                          typeName: "Program Type"
                      },
                      {
                          name: "MQ",
                          id: "Technical Taxonomies__Program Type__MQ",
                          moduleCount: 0,
                          type: "taxonomyTerm",
                          isEditable: false,
                          typeName: "Program Type"
                      },
                      {
                          name: "UI",
                          id: "Technical Taxonomies__Program Type__UI",
                          moduleCount: 2,
                          type: "taxonomyTerm",
                          isEditable: false,
                          typeName: "Program Type"
                      }
                  ],
                  type: "taxonomyType",
                  isEditable: false,
                  removeActions: [
                      "addTaxonomyTerm"
                  ],
                  expand: false
              }
          ],
          expand: true,
          isEditable: false,
          removeActions: [
              "addTaxonomyType"
          ],
          level: 0,
          key: "parent-Technical Taxonomies"
      },
      key: "child-Technical Taxonomies__File Access"
  }
    component.column = {
      "field": "moduleCount",
      "header": "assignedModules",
      "sortFn": false,
      "fieldType": FieldTypeEnum.NUMBER,
      "options": [
          {
              "value": "moduleCount",
              "icon": "info-circle",
              "title": "moduleCountTooltip",
              "styleClass": "ant-helper-secondary-text"
          }
      ],
      "columnAction": {
          "type": LinkType.HYPERLINK,
          resolveURL: (data: {type: "taxonomyType"}, index: 1) => {
           return 'modules';
          },
          resolveURLParams: (data: {type: "taxonomyType"},  index: 1) => {
            let params: Params;
           return params; 
          }
      },
      "displayAs": "link"
  };
    fixture.detectChanges();
  });

  let data = "New Taxonomy,name";
  
  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should check ngOnInit', waitForAsync(() => {
    component.displayAs = ViewMode.TAG;
    component.editOptions = editOptions;
    component.ngOnInit();
    expect(component.checkArray).toBeTruthy();
  }));

  it('should check ngOnInit with display as icon', waitForAsync(() => {
    component.ngOnInit();
    component.displayAs = ViewMode.ICON;
    component.stringList = {test : 'test'} as any;
    expect(component.checkArray).toBeTrue();
  }));

  it('should check ngOnInit to call changeEditableState', waitForAsync(() => {
    component.data.isNewRecord = true;
    spyOn(component, 'changeEditableState').and.callThrough();
    component.ngOnInit();
    expect(component.changeEditableState).toHaveBeenCalled();
  }));

  it('should sort the array naturally', waitForAsync(() => {
    const naturallySorted = (component as any).naturalSort(component.stringList);
    expect(naturallySorted).toEqual(['z1', 'z2', 'z11']);
  }));

  it('should check if checkArray is true', waitForAsync(() => {
    component.ngOnInit();
    expect(component.checkArray).toBeTruthy();
  }));

  it('should check if ViewMode is date', waitForAsync(() => {
    component.displayAs = ViewMode.DATE;
    component.stringList ="some test string";
    component.ngOnInit();
    expect(component.stringList).toBeDefined();
  }));

  it('should check if ViewMode is disabled link', waitForAsync(() => {
    component.displayAs = ViewMode.DISABLEDLINK;
    component.stringList ="some test string";
    component.ngOnInit();
    expect(component.stringList).toBeDefined();
  }));

  it("should display url as external link", waitForAsync(() => {
    component.displayAs = ViewMode.EXTERNALLINK;
    component.stringList = "http://innowake.com";
    component.ngOnInit();
    expect(component.stringList).toBeDefined();
  }));

  it('should submit form', waitForAsync(() => {
    component.isBulkInsertEnabled = false;
    component.data.isNewRecord = true;
    component.showInputType = true;
    component.fieldForm.controls.textField.setValue(component.convertToTagOnEnter(data));
    component.onFormSubmit();
    expect(component.editOptions?.enableEditing).toBeUndefined();
    component.editOptions = editOptions;
    component.onFormSubmit();
    expect(component.editOptions?.enableEditing).toBeFalsy();
  }));

  it('should change editable state', waitForAsync(() => {
    component.changeEditableState();
    expect(component.editOptions?.enableEditing).toBeUndefined();
    
    component.editOptions = editOptions;
    component.isEditable = {enableEditing: true};
    component.changeEditableState();
    expect(component.isEditable.enableEditing).toBeFalsy();
    
    component.textFieldInput = { nativeElement: { focus: () => {} }};
    component.changeEditableState();
    expect(component.isEditable.enableEditing).toBeTruthy();
  }));
  it('should test convertToTag with normal string', waitForAsync(() => {
    component.fieldForm.controls.textField.setValue('name,id');
    component.convertToTagOnEnter(data);
    expect(component.fieldForm.controls.textField).toBeDefined();
  }));

  it('should test convertToTag with string array', waitForAsync(() => {
    component.fieldForm.controls.textField.setValue('sd,dd,"ad,ad"');
    component.convertToTagOnEnter(data);
    expect(component.fieldForm.controls.textField).toBeDefined();
  }));

  it('should test checkNewRecord', waitForAsync(() => {
    spyOn(component as any, 'convertToTagOnEnter').and.callThrough();
    component.data.isNewRecord = true;
    component.isBulkInsertEnabled  = true;
    component.checkNewRecord();
    expect(component.convertToTagOnEnter).toHaveBeenCalled();
  }));

  it('should set DDId if matchedBusinessElement exists', () => {
    component.data = {
      linkedBusinessDataDictionary: [
        { id: '1', name: 'Element1' },
        { id: '2', name: 'Element2' },
      ],
      linkedNonBusinessDataDictionary: [],
      linkedBusinessRules: [{ id: '100' }, { id: '200' }],
    };
    component.showDetailsOfBusinessVariableReferenced(0, 'Element1');
    expect(component.data.selectedDDId).toBe('1');
  });

  it('should set selectedAnnotationId if matchedBusinessElement does not exist', () => {
    component.data = {
      linkedBusinessDataDictionary: [],
      linkedNonBusinessDataDictionary: [],
      linkedBusinessRules: [{ id: '100' }, {id: '200' }],
    };
    component.showDetailsOfBusinessVariableReferenced(0, 'NonExistentElement');
    expect(component.data.selectedAnnotationId).toBe('100');
  });

  it('should check placeholder while adding and editing the Taxonomy', () => {
    component.data.isNewRecord = true;
    component.isBulkInsertEnabled = true;
    component.ngOnInit();
    expect(component.placeholder).toEqual(translateService.instant('configuration.placeHolder'));
    component.data.isNewRecord = false;
    component.ngOnInit();
    expect(component.placeholder).toEqual(translateService.instant('configuration.taxonomyEditPlaceHolder'));
  });
});