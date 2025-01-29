import { fakeAsync, flush, TestBed, tick } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { ExportDownloadService } from './export-download.service';
import { of, throwError } from 'rxjs';
import { HttpService } from '@app/core';
import { HttpClient } from '@angular/common/http';

describe('ExportDownloadService', () => {
  let exportDownloadService: ExportDownloadService;
  let http: HttpClient;
  let httpMock: HttpTestingController;

  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', [
    'loading',
    'remove',
    'error',
    'success',
  ]);
  const translateServiceSpy = jasmine.createSpyObj<TranslateService>('TranslateService', ['instant']);
  const mockService = jasmine.createSpyObj<ExportDownloadService>('ExploreDownloadService', ['downloadUrl']);

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        ExportDownloadService,
        { provide: NzMessageService, useValue: messageServiceSpy },
        { provide: TranslateService, useValue: translateServiceSpy },
        { provide: HttpClientTestingModule, useValue: HttpService },
      ],
    });
    exportDownloadService = TestBed.inject(ExportDownloadService);
    http = TestBed.inject(HttpClient);
    httpMock = TestBed.inject(HttpTestingController);

    messageServiceSpy.loading.and.returnValue({ messageId: 'message--0' } as any);
    messageServiceSpy.remove.and.returnValue(null);
    messageServiceSpy.error.and.returnValue('some thing went wrong' as any);
    mockService.downloadUrl.and.returnValue(null);
    translateServiceSpy.instant.and.returnValue({});
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should create instance of exportDownloadService', () => {
    expect(exportDownloadService).toBeTruthy();
  });
  

  it('should test downloadUrl method with incorrect end point', () => {
    spyOn(http, 'get').and.returnValue(throwError('End Point is not Valid'));
    exportDownloadService.downloadUrl('url', 'progress', 'success', 'error');
    expect(messageServiceSpy.remove).toHaveBeenCalledWith('message--0');
    expect(messageServiceSpy.error).toHaveBeenCalled();
  });
});
