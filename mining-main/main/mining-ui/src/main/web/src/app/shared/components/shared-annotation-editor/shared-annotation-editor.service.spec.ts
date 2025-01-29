import { TestBed } from '@angular/core/testing';
import { NzDrawerRef } from 'ng-zorro-antd/drawer';
import { Observable, Subject } from 'rxjs';
import { SharedAnnotationEditorService } from './shared-annotation-editor.service';

class MockNzDrawerRef {
    private afterCloseSubject = new Subject<any>();
    afterClose: Observable<any> = this.afterCloseSubject.asObservable();
    close(result?: any): void {
      this.afterCloseSubject.next(result);
    }
    private afterOpenSubject = new Subject<void>();
    afterOpen: Observable<void> = this.afterOpenSubject.asObservable();
  
    open(): void {
      this.afterOpenSubject.next();
    }
  }

describe('SharedAnnotationEditorService', () => {
  let service: SharedAnnotationEditorService;
  let drawerRefMock: MockNzDrawerRef;

  beforeEach(() => {
    drawerRefMock = new MockNzDrawerRef();

    TestBed.configureTestingModule({
      providers: [
        SharedAnnotationEditorService,
        { provide: NzDrawerRef, useValue: drawerRefMock }
      ],
    });

    service = TestBed.inject(SharedAnnotationEditorService);
  });

  it('should set editor state when setEditorState is called', () => {
    service.setEditorState(true);
    expect(service.getEditorState()).toBeTrue();
  });

  it('should set editor form state when setEditorFormState is called', () => {
    service.setEditorFormState(true);
    expect(service.getEditorFormState()).toBeTrue();
  });
});
