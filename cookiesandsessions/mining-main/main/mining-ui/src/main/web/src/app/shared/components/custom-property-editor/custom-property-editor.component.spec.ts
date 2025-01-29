import { HttpClient, HttpHandler } from '@angular/common/http';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { UntypedFormBuilder, UntypedFormControl, UntypedFormGroup, ReactiveFormsModule } from '@angular/forms';
import { By } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { I18nService } from '@app/core';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { of } from 'rxjs';
import { CustomPropertyEditorComponent } from './custom-property-editor.component';
import { CustomPropertyMetadata, MetamodelControllerService, ProjectControllerService } from '@innowake/mining-api-angular-client';

xdescribe('CustomPropertyEditorComponent', () => {
  let component: CustomPropertyEditorComponent;
  let fixture: ComponentFixture<CustomPropertyEditorComponent>;

  const metamodelControllerServiceSpy = jasmine.createSpyObj<MetamodelControllerService>('MetamodelControllerService', ['defineCustomProperty', 'deleteCustomProperty']);
  const notificationSpy: jasmine.SpyObj<NzNotificationService> = jasmine.createSpyObj<NzNotificationService>('NzNotificationService', ['create']);

  const authServiceSpy = new NoAuthorizationService();
  const projectServiceSpy = jasmine.createSpyObj<ProjectControllerService>('ProjectControllerService', ['setAutoCompletionList', 'renameAutoCompletionValue', 'deleteAutoCompletionValue', 'setEnumValues']);
  const metaData: any = [{
    dataType: 'STRING',
    fieldType: "DEFAULT",
    label: "Some custom meta information",
    name: "customMetaInfo",
    autoCompletionKey: null
  }, {
    dataType: "STRING",
    fieldType: "DEFAULT",
    label: "Custom Annotation Property",
    name: "customAnnotationProperty",
    autoCompletionKey: null
  }, {
    dataType: "EMBEDDEDLIST",
    fieldType: "TAG",
    label: "Annotation Colors",
    autoCompletionKey: "colorTags"
  }];

  const annotationFields: { [k: string]: UntypedFormControl } = {};
  let val = new UntypedFormGroup(annotationFields);
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [CustomPropertyEditorComponent],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        ReactiveFormsModule,
        TranslateModule.forRoot(),
        AntDesignImportsModule,
        BrowserAnimationsModule,
      ],
      providers: [
        TranslateService,
        NumberFormatter,
        I18nService,
        HttpTestingController,
        HttpClient,
        HttpHandler,
        UntypedFormBuilder,
        { provide: MetamodelControllerService, useValue: metamodelControllerServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: authServiceSpy },
        { provide: ProjectControllerService, useValue: projectServiceSpy }
      ]
    }).compileComponents();
    metamodelControllerServiceSpy.defineCustomProperty.and.returnValue(of('' as any));
    metamodelControllerServiceSpy.deleteCustomProperty.and.returnValue(of('' as any));
    projectServiceSpy.setEnumValues.and.returnValue(of('' as any));
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CustomPropertyEditorComponent);
    component = fixture.componentInstance;
    component.projectId = 1;
    component.propertiesMetaModel = [{
      id: 0,
      position: 1,
      title: "Toto",
      type: "String Repeater",
      name:"",
      tags: null,
      defaultValues: ['test'],
      disabledActions: [],
      buttonToolTip: null,
      customCategory: "c1"
    },
    {
      id: 2,
      position: 2,
      title: null,
      type: "String Repeater",
      name:"",
      tags: null,
      defaultValues: ['test'],
      disabledActions: [],
      buttonToolTip: null,
      customCategory: null
    }];
    component.selectedCustomProperty = { id: 1, position: 1, title: 'My Custom Property', customCategory: '', type: 'Input String', name:"", defaultValues: ['test1'], disabledActions: [], buttonToolTip: null };
    fixture.detectChanges();
  });

  it('should create', () => {
    component.ngOnInit();
    expect(component).toBeTruthy();
  });

  it('should test camelize method ', () => {
    let str = "My name is"
    let strValue = (component as any).transformTextToCamelCase(str);
    expect(strValue).toBe('myNameIs');
  });

  it('should test submitForm method with tags', () => {
    const formData = {
      value: {
        position: 8,
        title: 'New property',
        type: 'Select Tag',
        defaultValues: ['test1', 'test2']
      },
    };
    component.creationMode = true;
    spyOn(component, 'submitForm').and.callThrough();
    component.submitForm(formData as UntypedFormGroup);
    spyOn(component.hideShowTab, 'emit');
    component.closeEditDetails();
    expect(component.hideShowTab.emit).toHaveBeenCalledWith(false);
  });

  it('should test submitForm method with creation mode as false', () => {
    const formData = {
      value: {
        position: 8,
        title: 'New property',
        type: 'Select Tag',
        defaultValues: ['test1', 'test2']
      },
    };
    component.creationMode = false;
    component['previousDefaultValues'] = [{index: 1 , value:'value1', removedDef: false}];
    spyOn(component, 'submitForm').and.callThrough();
    component.selectedCustomProperty.defaultValues = '';
    component.submitForm(formData as UntypedFormGroup);
    spyOn(component.hideShowTab, 'emit');
    component.closeEditDetails();
    expect(component.hideShowTab.emit).toHaveBeenCalledWith(false);
  });

  it('should test submitForm method with string', () => {
    const formData = {
      value: {
        position: 8,
        title: 'New property',
        type: 'Input String',
      },
    };
    component.creationMode = true;
    spyOn(component, 'submitForm').and.callThrough();
    component.submitForm(formData as UntypedFormGroup);
    spyOn(component.hideShowTab, 'emit');
    component.closeEditDetails();
    expect(component.hideShowTab.emit).toHaveBeenCalledWith(false);
  });

  it('should test submitForm method with url', () => {
    const formData = {
      value: {
        position: 9,
        title: 'Amazon',
        type: CustomPropertyMetadata.FieldTypeEnum.URL,
        url: 'http://amazon.com'
      }
    };
    component.creationMode = true;
    spyOn(component, 'submitForm').and.callThrough();
    component.submitForm(formData as UntypedFormGroup);
    spyOn(component.hideShowTab, 'emit');
    component.closeEditDetails();
    expect(component.hideShowTab.emit).toHaveBeenCalledWith(false);
    expect(projectServiceSpy.setEnumValues).toHaveBeenCalledWith(1, 'amazonDefaultValueKey', new Set([ 'http://amazon.com' ]));
  }); 

  it ('should test URL validator with a valid URL', () => {
    const formData = {
      value: {
        position: 10,
        title: 'valid url',
        type: CustomPropertyMetadata.FieldTypeEnum.URL
      }
    };
    component.CustomPropertyEditorForm.patchValue(formData);
    component.CustomPropertyEditorForm.patchValue({url: 'http://innowake.com'});

    expect(component.validateUrl.valid).toBeTruthy();
  });

  it ('should test URL validator with an invalid URL', () => {
    const formData = {
      value: {
        position: 10,
        title: 'invalid url',
        type: CustomPropertyMetadata.FieldTypeEnum.DEFAULT
      }
    };

    component.CustomPropertyEditorForm.patchValue(formData);
    component.CustomPropertyEditorForm.patchValue({
      type: CustomPropertyMetadata.FieldTypeEnum.URL,
      url: 'not a valid url'
    });

    expect(component.validateUrl.valid).toBeFalsy();
  });

  it('should test onSavingCustomProperty method ', () => {
    const title = 'New Property';
    (component as any).onSavingCustomProperty(title);
    expect(component.isSave).toBeFalsy();
  });

  it('should test setAutoCompletionList method ', () => {
    const title = 'New Property';
    (component as any).setAutoCompletionList(null, title);
    spyOn(component as any, 'onSavingCustomProperty');
  });

  it('should test errorNotification method', () => {
    (component as any).errorNotification();

  })

  it('should test onDelete method', () => {
    spyOn(component, 'onDelete').and.callThrough();
    component.onDelete();
    spyOn(component.hideShowTab, 'emit');
    component.closeEditDetails();
    expect(component.hideShowTab.emit).toHaveBeenCalledWith(false);
  });

  it('should test setPositionValue', () => {
    spyOn(component, 'setPositionValue').and.callThrough();
    component.setPositionValue();
    expect(component.CustomPropertyEditorForm.value.position).toBe(1);
  });

  it('should test the position value is <= actual no of custom properties available.', () => {
    component.CustomPropertyEditorForm.patchValue({
      position: undefined
    });
    component.creationMode = true;
    component.length = 8;
    spyOn(component, 'setPositionValue').and.callThrough();
    fixture.detectChanges();
    component.setPositionValue();
    fixture.whenStable().then(() => {
      fixture.detectChanges();
      let input = fixture.debugElement.query(By.css('.custom-property-field__setposition'));
      let el = input.nativeElement;
      el.value = 10
      input.nativeElement.dispatchEvent(new Event('focusout'));
      el.value = 8;
      expect(input.nativeElement.value).toBeLessThanOrEqual(component.length);
    });
  });

  it('Should test position value can not be 0.', () => {
    component.CustomPropertyEditorForm.patchValue({
      position: undefined
    });
    component.creationMode = false;
    component.length = 8;
    spyOn(component, 'setPositionValue').and.callThrough();
    component.setPositionValue();
    fixture.whenStable().then(() => {
      fixture.detectChanges();
      let input = fixture.debugElement.query(By.css('.custom-property-field__setposition'));
      let el = input.nativeElement;
      el.value = -2
      input.nativeElement.dispatchEvent(new Event('focusout'));
      el.value = 8;
      expect(input.nativeElement.value).toBeGreaterThan(0);
    });
  });

  it('should test ngOnintit', () => {
    component.selectedCustomProperty = null;
    component.ngOnInit()
    expect(component.length).toBe(3);
  });
  it('it should call addField', () => {
    component.addDefaultValues();
    expect(component.validateDefaultValues).toBeDefined();
  });

  it('it should call removeField', () => {
    component.removeDefaultValues(0);
    expect(component.validateDefaultValues).toBeDefined();
  });

  it('it should validate duplicate value in customCategory', () => {
    component.customCategoryList = ['other', 'test'];
    expect(component.CustomPropertyEditorForm.valid).toBeTruthy;

    component.CustomPropertyEditorForm.patchValue({
      customCategory: 'test'
    });
    expect(component.CustomPropertyEditorForm.valid).toBeFalsy;
  });

  xit('it should test form control enable and disable for NewCustomCategory', () => {
    component.selectedCustomProperty = null;
    component.ngOnInit();
    const newCustomCategory = component.CustomPropertyEditorForm.get('newCustomCategory');
    expect(newCustomCategory.enabled).toBe(false);
    component.onChangeCustomCategory('other'); 
    expect(newCustomCategory.enabled).toBe(true);
    component.onChangeCustomCategory('');
    expect(newCustomCategory.enabled).toBe(false);
  });
});
