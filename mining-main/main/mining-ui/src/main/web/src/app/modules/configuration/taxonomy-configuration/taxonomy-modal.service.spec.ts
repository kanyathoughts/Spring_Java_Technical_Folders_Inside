import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { TaxonomyModalService } from './taxonomy-modal.service';
import { of, Subject } from 'rxjs';
import { HttpService } from '@app/core';
import { HttpClient } from '@angular/common/http';

describe('TaxonomyModalService', () => {
  let taxonomyModalService: TaxonomyModalService;
  let http: HttpClient;
  let httpMock: HttpTestingController;
  const mockService = jasmine.createSpyObj<TaxonomyModalService>('TaxonomyModalService', ['setValidationResponse', 'triggerCancelWarningSubject']);

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [TaxonomyModalService, { provide: HttpClientTestingModule, useValue: HttpService }],
    });
    taxonomyModalService = TestBed.inject(TaxonomyModalService);
    http = TestBed.inject(HttpClient);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should create instance of taxonomyModalService', () => {
    expect(taxonomyModalService).toBeTruthy();
  });

  it('should test setValidationResponse method', () => {
    taxonomyModalService.setValidationResponse('testing' as any);
    expect(taxonomyModalService.validationResponse).toBeDefined();
  });

  it('should test getValidationResponse method', () => {
    taxonomyModalService.setValidationResponse('testing' as any);
    const getValidationResponse = taxonomyModalService.getValidationResponse();
    expect(getValidationResponse).toBeDefined();
  });

  it('should test getProjectID method', () => {
    taxonomyModalService.setProjectId(1 as any);
    const getValidationResponse = taxonomyModalService.getProjectId();
    expect(getValidationResponse).toEqual(1);
  });

  it('should test downloadLog method', () => {
    taxonomyModalService.downloadLog();
    const anchorEle = document.getElementsByTagName('a');
    expect(anchorEle).toBeDefined();
  });

  it('should test setStartValidation method', () => {
    taxonomyModalService.setStartValidation(true);
    expect(taxonomyModalService.validationStart).toBeTruthy();
  });

  it('should test isImportValidationStarted method', () => {
    const response = taxonomyModalService.isImportValidationStarted();
    expect(response).toBeDefined();
     });
});
