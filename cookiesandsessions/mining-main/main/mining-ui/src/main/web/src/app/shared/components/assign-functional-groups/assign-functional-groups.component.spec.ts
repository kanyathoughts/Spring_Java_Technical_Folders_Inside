import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { SharedModule } from '@app/shared';
import { TranslateModule } from '@ngx-translate/core';
import { AssignFunctionalGroupComponent } from './assign-functional-groups.component';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { of } from 'rxjs';
import { NzModalRef } from 'ng-zorro-antd/modal';

describe('AssignFunctionalGroupComponent', () => {
    let component: AssignFunctionalGroupComponent;
    let fixture: ComponentFixture<AssignFunctionalGroupComponent>;
    const graphQlControllerServiceStub: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj<GraphQlControllerService>
    ('GraphQlControllerService', ['graphQl']);
    const modalRef = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['destroy']);
    const graphQlData: any =  {"data": { "functionalBlocks": {"content": [] }
  }};
    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
          declarations: [AssignFunctionalGroupComponent],
          imports: [SharedModule,
            RouterTestingModule,
            BrowserAnimationsModule,
            TranslateModule.forRoot({}),
            HttpClientTestingModule],
          providers: [
            { provide: GraphQlControllerService, useValue: graphQlControllerServiceStub },
            { provide: NzModalRef, useValue: modalRef }
          ]
        }).compileComponents();
        graphQlControllerServiceStub.graphQl.and.returnValue(of(graphQlData as any));
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(AssignFunctionalGroupComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should load instance', () => {
        expect(component).toBeTruthy();
    });
    
    it('should test removeItem', () => {
      component.mappedData = {
        'test' : {
          'annotations': ['test', 'test2']
        }
      }
      component.removeItem('test', 234);
      expect(component.mappedData['test'].updated).toBeTruthy();
    });

    it('should test removeItem when annotation type is functional', () => {
      component.mappedData = {
        'test' : {
          'annotations': ['test', 'test2']
        }
      }
      component.removeItem('test', 234);
      spyOn(component, 'isFunctionalTypeAnnotation').and.returnValue(true);
      expect(component.mappedData['test'].updated).toBeTruthy();
    });
        
})