import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { I18nService } from '@app/core';
import FileSaveSupport from '@app/core/utils/file-save-support.utils';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateModule, TranslateService } from '@ngx-translate/core';

import { MetricsCardComponent } from './metrics-card.component';

describe('MetricsCardComponent', () => {
  let component: MetricsCardComponent;
  let fixture: ComponentFixture<MetricsCardComponent>;
  const i18nServiceSpy = { language: 'en-US' };
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['error']);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ MetricsCardComponent ],
      providers: [
        NumberFormatter, 
        FileSaveSupport,
        { provide: I18nService, useValue: i18nServiceSpy },
        TranslateService,
        { provide: NzMessageService, useValue: messageServiceSpy }
      ],
      imports: [
        TranslateModule.forRoot({})
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MetricsCardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
