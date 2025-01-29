import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { CustomPropertiesService } from '@app/core/services/custom-properties/custom-properties.service';
import { CustomPropertiesCardComponent } from './custom-properties-card.component';
import { CustomPropertyFieldListComponent } from '@app/shared/components/custom-property-editor/custom-property-field-list/custom-property-field-list.component';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';
import { of, Subject } from 'rxjs';
import { CustomPropertyDetails } from '@app/shared/interfaces/custom-property-input.interface';
import { ModulePojo, ProjectPojoCustomProperties } from '@innowake/mining-api-angular-client';
import { get } from 'lodash';

describe('CustomPropertiesCardComponent', () => {
    let component: CustomPropertiesCardComponent;
    let fixture: ComponentFixture<CustomPropertiesCardComponent>;

    const customPropertyServiceSpy: jasmine.SpyObj<CustomPropertiesService> = jasmine.createSpyObj('CustomPropertiesService', 
    ['getCustomPropertiesMetadataForClass']);
    const modalSpy: jasmine.SpyObj<NzModalService> = jasmine.createSpyObj('NzModalService', ['create','afterClose']);
    const currentClient: ClientProjectRelationship = new ClientProjectRelationship(1, 'test', 1, 'test');
    const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['afterClose', 'getContentComponent']);

    const customProperties: { [key: string]: ProjectPojoCustomProperties[] } = {
        'test': [{
            'name': 'newCustomProperty',
            'value': 'test[dfds',
            'dataType': 'STRING',
        }, 
        {
            'name': 'test',
            'value': 'test[dfds',
            'dataType': 'STRING',
        }]
    }

    const customPropertiesDetails: CustomPropertyDetails[] = [
        {
            "name": "newCustomProperty",
            "value": "test",
            "dataType": "STRING",
            "label": "New Custom property",
            "fieldType": "DEFAULT",
            "customViewIndex": 1
        },
        {
            "name": "myCustomProperty",
            "value": "option1",
            "dataType": "STRING",
            "label": "My custom property",
            "fieldType": "DEFAULT",
            "customViewIndex": 3
        },
        {
            "name": "newCustomProp2",
            "value": "option2",
            "dataType": "STRING",
            "label": "New custom prop 2",
            "fieldType": "DEFAULT",
            "customViewIndex": 2
        }
    ];

    const moduleValue: ModulePojo = {
        uid: '#136:600',
        customProperties: {'ModuleCustomProperties': [{
            'name': 'newCustomProperty',
            'value': 'test',
            'dataType': 'STRING'
        }, {
            name: 'myCustomProperty',
            value: 'option1',
            dataType: 'STRING'
        }, {
            name: 'newCustomProp2',
            value: 'option2',
            dataType: 'STRING'
        }] ,
        'test': [{
            'name': 'newCustomProperty',
            'value': 'test',
            'dataType': 'STRING'
        }, {
            name: 'myCustomProperty',
            value: 'option1',
            dataType: 'STRING'
        }, {
            name: 'newCustomProp2',
            value: 'option2',
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
          complexityMcCabe: null,
        },
        content: null,
        sourceCodeAvailable: false
      };

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            declarations: [CustomPropertiesCardComponent, CustomPropertyFieldListComponent],
            imports: [
                FormsModule,
                TranslateModule.forRoot({}),
                HttpClientTestingModule,
                BrowserAnimationsModule
            ],
            providers: [
                TranslateService,
                { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService() },
                { provide: NzModalService, useValue: modalSpy },
                { provide: CustomPropertiesService, useValue: customPropertyServiceSpy }
            ]
        }).compileComponents();
        customPropertyServiceSpy.getCustomPropertiesMetadataForClass.and.returnValue(of([] as any));
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(CustomPropertiesCardComponent);
        component = fixture.componentInstance;
        component.currentClient = currentClient;
        component.customProperties = customProperties;
        component.moduleCustomProperties = [];
        component.selectedModule = moduleValue;
        component.customPropertiesDetails = customPropertiesDetails;
        fixture.detectChanges();
    });

    it('should create', () => {
        component.ngOnInit();
        component.ngOnChanges();
        expect(component).toBeTruthy();
    });
    it('should open edit custom properties modal', () => {
        nzModalRefSpy.afterClose = new Subject();
        nzModalRefSpy.afterClose.next({});
        nzModalRefSpy.getContentComponent.and.returnValue({});
        modalSpy.create.and.returnValue(nzModalRefSpy);
        
        component.editCustomProperties();
        expect(modalSpy.create).toHaveBeenCalled();
    });

    it('should test setCustomPropertiesDetails', () => {
        let data = [{ customPropertyClassName: 'test', optionList: ['test'] }]
        component.moduleCustomProperties = data;
        spyOn((component as any), 'setCustomPropertiesDetails').and.callThrough();
        component.ngOnChanges();
        (component as any).setCustomPropertiesDetails();
        expect((component as any).setCustomPropertiesDetails).toHaveBeenCalled();
    });
})