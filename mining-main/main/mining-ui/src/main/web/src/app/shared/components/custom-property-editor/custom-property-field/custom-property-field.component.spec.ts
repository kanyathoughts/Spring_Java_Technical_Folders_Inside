import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { CustomPropertyFieldComponent } from './custom-property-field.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule } from '@ngx-translate/core';
import { UntypedFormBuilder } from '@angular/forms';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { of } from 'rxjs/internal/observable/of';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { CustomPropertiesService } from '@app/core/services/custom-properties/custom-properties.service';
import { CustomPropertyMetadata } from '@innowake/mining-api-angular-client';

const customAnnotationProperty = 'customAnnotationProperty';

describe('CustomPropertyFieldComponent', () => {
  let component: CustomPropertyFieldComponent;
  let fixture: ComponentFixture<CustomPropertyFieldComponent>;
  const currentClient: ClientProjectRelationship = new ClientProjectRelationship(1, 'test', 1, 'test');
  const clientProjectRelationshipServiceSpy = jasmine.createSpyObj<ClientProjectRelationshipService>('ClientProjectRelationshipService',
    ['getClientProjectObservable']);
  const customPropertiesService = jasmine.createSpyObj<CustomPropertiesService>('CustomPropertiesService', ['getCustomPropertiesMetadataForClass']);
  const formBuilder: UntypedFormBuilder = new UntypedFormBuilder();
  const formBuilderStub = {};
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      schemas: [NO_ERRORS_SCHEMA],
      declarations: [CustomPropertyFieldComponent],
      imports: [
        HttpClientTestingModule,
        TranslateModule.forRoot({})
      ],
      providers: [
        { provide: UntypedFormBuilder, useValue: formBuilderStub },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService()},
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy},
        { provide: CustomPropertiesService, useValue: customPropertiesService }
      ]
    }).compileComponents();
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(currentClient as any));
    customPropertiesService.getCustomPropertiesMetadataForClass.and.returnValue(of([] as any))
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CustomPropertyFieldComponent);
    component = fixture.componentInstance;
    component.form = formBuilder.group({
      type: "RULE",
      name: "test",
      state: "INVALID",
      categoryId: 2,
      category: 'test',
      [customAnnotationProperty]: '["tag1"]',
      customProperties: formBuilder.array([])
    });
    component.dataType = CustomPropertyMetadata.FieldTypeEnum;
    component.customProperty = {
        customPropertyClassName: "Annotations",
        customViewIndex: 0,
        customViewNames: [],
        dataSource: null,
        fieldType: CustomPropertyMetadata.FieldTypeEnum.TAG,
        description: "A custom property for the Annotation class",
        label: "Custom Annotation Property",
        mandatory: false,
        max: null,
        min: null,
        name: customAnnotationProperty,
        inputName: customAnnotationProperty,
        optionList: ['hi','hello'],
        pluginVisible: true,
        readOnly: false,
        showWhen: { },
        validationErrorMessage: null,
        validationRegex: null
      };
  });

  it('can load instance', () => {
    expect(component).toBeTruthy();
  });

  it('ngOnInit', () => {
    customPropertiesService
    .getCustomPropertiesMetadataForClass('Annotation', 1)
    .subscribe(result => expect(result).toBeTruthy());
    component.ngOnInit();
    expect(component.finalType).toBeDefined();
  });

  it('should test else condition of finalType', () => {
    component.customProperty = {
      customPropertyClassName: "Annotations",
      customViewIndex: 0,
      customViewNames: [],
      dataSource: null,
      dataType: CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST,
      description: "A custom property for the Annotation class",
      label: "Custom Annotation Property",
      mandatory: false,
      max: null,
      min: null,
      name: customAnnotationProperty,
      inputName: customAnnotationProperty,
      optionList: ['hi','hello'],
      pluginVisible: true,
      readOnly: false,
      showWhen: { },
      validationErrorMessage: null,
      validationRegex: null
    };
    component.ngOnInit();
    expect(component.finalType).toBe('EMBEDDEDLIST');
  });

  it('it should call addField', () => {
    component.addField( { inputName: 'customProperties',  customPropertyClassName: "Annotations", } );
    expect(component.dataType).toEqual(CustomPropertyMetadata.FieldTypeEnum);
  });

  it('it should call removeField', () => {
    component.removeField( { inputName: 'customProperties',  customPropertyClassName: "Annotations", } , 0);
    expect(component.dataType).toEqual(CustomPropertyMetadata.FieldTypeEnum);
  });

  describe('onModelChange', () => {
    it('it should not change the value if tags have no leading or tailing blanks', () => {
      const oldValue = component.form.get(customAnnotationProperty).value;
      component.onModelChange( ['1', '2', '3', '4 4'] );
      /* list of tags contains no blanks so no new value is set so we check for the old value */
      expect(component.form.get(customAnnotationProperty).value).toEqual( oldValue );
    });

    it('it should trim items with blanks and set trimmed items', () => {
      component.onModelChange( ['1', ' 2', '3 ', ' 4 4 '] );
      expect(component.form.get(customAnnotationProperty).value).toEqual( ['1', '2', '3', '4 4'] );
    });

    it('it should remove duplicates after trim', () => {
      /* fst case: '2' + ' 2' */
      /* snd case: '3 ' + '3' */
      component.onModelChange( ['1', '2', ' 2', '3 ', '3', ' 4 4 '] );
      expect(component.form.get(customAnnotationProperty).value).toEqual( ['1', '4 4'] );
    });

    it('it should remove empty items after trim', () => {
      component.onModelChange( ['1', '', ' ', '  ', ' 4 4 '] );
      expect(component.form.get(customAnnotationProperty).value).toEqual( ['1', '4 4'] );
    });

    it('it should have no items after trim', () => {
      component.onModelChange( ['', ' ', '  '] );
      expect(component.form.get(customAnnotationProperty).value).toEqual( [] );
    });
  });
});
