import { HttpClient } from '@angular/common/http';
import { TestBed } from '@angular/core/testing';
import { CfgSupportedTypeService } from './cfg-supported-type.service';
import { HttpService } from '../http/http.service';
import { of } from 'rxjs';

export const CFG_SUPPORTED_TYPES = [
  {
    "type": "COMPILATION_UNIT",
    "technology": "JAVA"
  },
  {
    "type": "PROGRAM",
    "technology": "COBOL"
  },
  {
    "type": "SUBPROGRAM",
    "technology": "NATURAL"
  },
  {
    "type": "PROGRAM",
    "technology": "PL1"
  },
  {
    "type": "PROGRAM",
    "technology": "NATURAL"
  },
  {
    "type": "JOB",
    "technology": "JCL"
  },
  {
    "type": "PROGRAM",
    "technology": "C"
  },
  {
    "type": "SUBROUTINE",
    "technology": "NATURAL"
  },
  {
    "type": "MAINPROGRAM",
    "technology": "PL1"
  }
];

describe('CfgSupportedTypeService', () => {

  const httpServiceSpy = jasmine.createSpyObj<HttpService>('HttpService', ['get', 'skipErrorHandler', 'disableApiPrefix']);
  let cfgSupportedService: CfgSupportedTypeService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        CfgSupportedTypeService,
        {
          provide: HttpClient,
          useValue: httpServiceSpy
        },
      ]
    });
    httpServiceSpy.skipErrorHandler.and.returnValue(httpServiceSpy);
    httpServiceSpy.disableApiPrefix.and.returnValue(httpServiceSpy);
    httpServiceSpy.get.and.returnValue(of(CFG_SUPPORTED_TYPES));
    cfgSupportedService = TestBed.inject(CfgSupportedTypeService);
  });

  it('should init', () => {
    cfgSupportedService.init();
    expect(cfgSupportedService.supportedTypes).toEqual(CFG_SUPPORTED_TYPES);
  });

  it('should return true for supported modules', () => {
    cfgSupportedService.supportedTypes = CFG_SUPPORTED_TYPES;
    expect(cfgSupportedService.checkIfSupported('COBOL', 'PROGRAM')).toBeTrue();
  });

  it('should return false for unsupported modules', () => {
    cfgSupportedService.supportedTypes = CFG_SUPPORTED_TYPES;
    expect(cfgSupportedService.checkIfSupported('JS', 'SERVICE')).toBeFalse();
  });

});
