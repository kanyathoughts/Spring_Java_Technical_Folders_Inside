import { TestBed } from '@angular/core/testing';
import { ClipboardService } from './clipboard.service';
import { NzMessageService } from 'ng-zorro-antd/message';
import { DEFAULT_LANGUAGE, TranslateModule, MissingTranslationHandler, TranslateCompiler, TranslateLoader, TranslateParser, TranslateService, TranslateStore, USE_DEFAULT_LANG, USE_EXTEND, USE_STORE } from '@ngx-translate/core';

describe('ClipboardService', () => {
  let clipboardService: ClipboardService;
  const messageServiceSpy = jasmine.createSpyObj('NzMessageService', ['success']);
  const translateServiceSpy = jasmine.createSpyObj('TranslateService', ['instant']);

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [TranslateModule.forRoot({})],
      providers: [
        ClipboardService,
        { provide: TranslateService, useValue: translateServiceSpy },
        TranslateStore,
        MissingTranslationHandler, TranslateCompiler, TranslateLoader, TranslateParser,
        { provide: USE_DEFAULT_LANG,  useValue: undefined },
        { provide: USE_STORE,  useValue: undefined },
        { provide: USE_EXTEND,  useValue: undefined },
        { provide: DEFAULT_LANGUAGE,  useValue: undefined },     
        { provide: NzMessageService, useValue: messageServiceSpy }],
    });
    clipboardService = TestBed.inject(ClipboardService);
  });

  it('should be created', () => {
    expect(clipboardService).toBeTruthy();
  });

  it('should copy content to clipboard', () => {
    const entity = 'annotation';
    const content = '4';
    clipboardService.copyToClipboard(entity, content);
    const clipboardData = clipboardService['getClipboardData']();
    expect(clipboardData[entity]).toEqual(content);
  });

  it('should get content from clipboard', () => {
    const entity = 'annotation';
    const content = '4';
    clipboardService.copyToClipboard(entity, content);
    const retrievedContent = clipboardService.getFromClipboard(entity);
    expect(retrievedContent).toEqual(content);
  });
});
