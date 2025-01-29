import { ComponentFixture, TestBed } from '@angular/core/testing';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { AnnotationPojo, AnnotationControllerService } from '@innowake/mining-api-angular-client';
import { IdentifyBusinessRelatedModalComponent } from './identify-business-related-modal.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { HttpEvent } from '@angular/common/http';
import { throwError, of } from 'rxjs';

describe('IdentifyBusinessRelatedModalComponent', () => {
    let component: IdentifyBusinessRelatedModalComponent;
    let fixture: ComponentFixture<IdentifyBusinessRelatedModalComponent>;

    const codeAnnotationsValue: AnnotationPojo[] = [
        {
          customProperties: {
            'Property1': [{
              name: 'customAnnotationProperty',
              value: 'A value for the custom Annotation property',
              dataType: 'STRING'
            }]
          },
          id: 1,
          name: 'Annotation 1',
          projectId: 1,
          state: 'CANDIDATE',
          type: 'RULE',
          categoryId: 1,
          categoryName: 'Annotation Category A',
          createdByUserId: 'admin',
          updatedByUserId: null,
          sourceAttachment: 'abcd'
        }
      ];
    
    const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', [
        'loading',
        'remove',
        'error',
        'success',
    ]);
    const translateServiceSpy = jasmine.createSpyObj<TranslateService>('TranslateService', ['instant']);
    const annotationControllerServiceSpy: jasmine.SpyObj<AnnotationControllerService> = jasmine.createSpyObj<AnnotationControllerService>
        ('AnnotationControllerService', ['updateCategoryAndMetadataOfAnnotation']);
    const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['success']);

    beforeEach(() => {
        TestBed.configureTestingModule({
            declarations: [IdentifyBusinessRelatedModalComponent],
            imports: [TranslateModule.forRoot()],
            providers: [
                { provide: NzModalRef, useValue: jasmine.createSpyObj('NzModalRef', ['destroy']) },
                { provide: AnnotationControllerService, useValue: annotationControllerServiceSpy },
                { provide: TranslateService, useValue: translateServiceSpy },
                { provide: NzModalService, useValue: modalServiceSpy },
                { provide: NzMessageService, useValue: messageServiceSpy },
            ]
        });
        fixture = TestBed.createComponent(IdentifyBusinessRelatedModalComponent);
        component = fixture.componentInstance;
    });
    annotationControllerServiceSpy.updateCategoryAndMetadataOfAnnotation.and.returnValues(of([] as any));

    it('should create the component', () => {
        expect(component).toBeTruthy();
    });

    it('should call updateCategoryAndMetadataOfAnnotation and handle success response', () => {
        component.onSubmit();
        expect(annotationControllerServiceSpy.updateCategoryAndMetadataOfAnnotation).toHaveBeenCalled();
        expect(modalServiceSpy.success).toHaveBeenCalled();
    });

    it('handleCancel should call modalRef.destroy', () => {
        component.handleCancel();
        const modalRef = TestBed.inject(NzModalRef);
        expect(modalRef.destroy).toHaveBeenCalled();
    });

})