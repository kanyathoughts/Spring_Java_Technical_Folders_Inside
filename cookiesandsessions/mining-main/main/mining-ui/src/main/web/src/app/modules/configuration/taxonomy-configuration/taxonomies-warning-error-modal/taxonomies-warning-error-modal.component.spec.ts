import { HttpClientTestingModule } from '@angular/common/http/testing';
import { async, ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzMessageModule, NzMessageService } from 'ng-zorro-antd/message';
import { NzModalModule, NzModalRef } from 'ng-zorro-antd/modal';
import {  Subject, Subscription } from 'rxjs';
import { of } from 'rxjs/internal/observable/of';
import { TaxonomiesErrorWarningModalComponent } from './taxonomies-warning-error-modal.component';
import { TaxonomyModalService } from '@app/modules/configuration/taxonomy-configuration/taxonomy-modal.service';
import { WarningCancel } from '../taxonomy-reporting.model';

describe('TaxonomiesErrorWarningModalComponent', () => {
  let component: TaxonomiesErrorWarningModalComponent;
  let fixture: ComponentFixture<TaxonomiesErrorWarningModalComponent>;
  const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['updateConfig', 'close', 'afterClose']);
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', [
    'success',
    'warning',
    'remove',
    'error',
  ]);

  const taxonomiesServiceSpy = jasmine.createSpyObj('TaxonomyModalService', ['validateImportedFile', 'triggerCancelWarningSubject','downloadLog', 'getValidationResponse']);
  taxonomiesServiceSpy.cancelWarningSubject = new Subject();

  beforeEach(
    waitForAsync(() => {
      TestBed.configureTestingModule({
        declarations: [TaxonomiesErrorWarningModalComponent],
        imports: [TranslateModule.forRoot({}), NzModalModule, NzMessageModule, HttpClientTestingModule],
        providers: [
          { provide: NzMessageService, useValue: messageServiceSpy },
          { provide: NzModalRef, useValue: nzModalRefSpy },
          { provide: TaxonomyModalService, useValue: taxonomiesServiceSpy },
          TranslateService,
        ],
      }).compileComponents();
      messageServiceSpy.success.and.returnValue(null);
      messageServiceSpy.warning.and.returnValue(null);
      taxonomiesServiceSpy.triggerCancelWarningSubject.and.returnValue(of(WarningCancel.WARNING_CONTINUE));
      taxonomiesServiceSpy.validateImportedFile.and.returnValue(of(WarningCancel.WARNING_CANCEL));
      taxonomiesServiceSpy.downloadLog.and.returnValue(null);
    })
  );

  beforeEach(() => {
    fixture = TestBed.createComponent(TaxonomiesErrorWarningModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', async(() => {
    expect(component).toBeTruthy();
  }));

  it('should test onContinueWarning method', async(() => {
    component.onContinueWarning();
  }));

  it('should test onDownloadWarningLog method', () => {
    component.onDownloadWarningLog();
    expect(taxonomiesServiceSpy.downloadLog).toHaveBeenCalled();
  });
});
