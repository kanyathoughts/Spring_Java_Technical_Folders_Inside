import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { WindowToken } from '@app/core/utils/window';
import { UniversalSearchComponent } from './universal-search.component';
import { of } from 'rxjs/internal/observable/of';
import { UniversalSearchControllerService, UniversalSearchResult } from '@innowake/mining-api-angular-client';

describe('UniversalSearchComponent', () => {
  let component: UniversalSearchComponent;
  let fixture: ComponentFixture<UniversalSearchComponent>;
  let router: Router;
  const routerSpy = jasmine.createSpyObj('Router', ['navigate']);
  const mockWindow = jasmine.createSpyObj('WindowToken', ['open']);
  const universalSearchControllerServiceSpy = jasmine.createSpyObj<UniversalSearchControllerService>('UniversalSearchControllerService', ['searchQueryInProject']);
  const mockedUniversalSearchResult: UniversalSearchResult[] =
    [
      {
        "providedBy": "module-name-path",
        "rank": 0,
        "title": "UAMZNPON",
        "subTitle": null,
        "context": "",
        "type": "Module",
        "links": [
          {
            "type": "MODULE_DETAILS",
            "properties": {
              "moduleId": "1529"
            }
          }
        ]
      },
      {
        "providedBy": "data-dictionary",
        "rank": 0,
        "title": "UAMDTH",
        "subTitle": "src/natural/C/programs/TERMINAT.nsp",
        "context": "UAMDTH",
        "type": "Data Dictionary",
        "links": [
          {
            "type": "CODE_VIEWER",
            "properties": {
              "offset": "6",
              "length": "78",
              "moduleId": "1064"
            }
          }
        ]
      },
      {
        "providedBy": "data-dictionary",
        "rank": 0,
        "title": "UAMDTH",
        "subTitle": "src/natural/C/programs/UAMAPDTE.nsp",
        "context": "UAMDTH",
        "type": "Data Dictionary",
        "links": [
          {
            "type": "CODE_VIEWER",
            "properties": {
              "offset": "6",
              "length": "234",
              "moduleId": "1091"
            }
          }
        ]
      },
      {
        "providedBy": "data-dictionary",
        "rank": 0,
        "title": "UAMP-DT-KEY",
        "subTitle": "src/natural/C/programs/UAMAPDTE.nsp",
        "context": "UAMP-DT-KEY",
        "type": "Data Dictionary",
        "links": [
          {
            "type": "CODE_VIEWER",
            "properties": {
              "offset": "11",
              "length": "282",
              "moduleId": "1091"
            }
          }
        ]
      },
      {
        "providedBy": "data-dictionary",
        "rank": 0,
        "title": "UAMKEY",
        "subTitle": "src/natural/C/programs/GETUAM.nsp",
        "context": "UAMKEY",
        "type": "Data Dictionary",
        "links": [
          {
            "type": "CODE_VIEWER",
            "properties": {
              "offset": "6",
              "length": "187",
              "moduleId": "1063"
            }
          }
        ]
      },
      {
        "providedBy": "data-dictionary",
        "rank": 0,
        "title": "UAMKEY",
        "subTitle": "src/natural/C/programs/FINDUAM.nsp",
        "context": "UAMKEY",
        "type": "Data Dictionary",
        "links": [
          {
            "type": "CODE_VIEWER",
            "properties": {
              "offset": "6",
              "length": "86",
              "moduleId": "1090"
            }
          }
        ]
      },
      {
        "providedBy": "module-name-path",
        "rank": 0,
        "title": "READUAM",
        "subTitle": "src/natural/C/programs/READUAM.nsp",
        "context": "",
        "type": "Module",
        "links": [
          {
            "type": "MODULE_DETAILS",
            "properties": {
              "moduleId": "1053"
            }
          }
        ]
      },
      {
        "providedBy": "module-name-path",
        "rank": 0,
        "title": "UAMAMDM2",
        "subTitle": null,
        "context": "",
        "type": "Module",
        "links": [
          {
            "type": "MODULE_DETAILS",
            "properties": {
              "moduleId": "1534"
            }
          }
        ]
      },
      {
        "providedBy": "module-name-path",
        "rank": 0,
        "title": "STOREUAM",
        "subTitle": "src/natural/C/programs/STOREUAM.nsp",
        "context": "",
        "type": "Module",
        "links": [
          {
            "type": "MODULE_DETAILS",
            "properties": {
              "moduleId": "1051"
            }
          }
        ]
      },
    ];
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [RouterTestingModule],
      declarations: [UniversalSearchComponent],
      providers: [
        { provide: Router, useValue: routerSpy },
        { provide: UniversalSearchControllerService, useValue: universalSearchControllerServiceSpy },
        { provide: WindowToken, useValue: mockWindow }
      ]
    }).compileComponents();
    universalSearchControllerServiceSpy.searchQueryInProject.and.returnValue(of(mockedUniversalSearchResult as any));
  });

  beforeEach(() => {
    router = TestBed.inject(Router);
    fixture = TestBed.createComponent(UniversalSearchComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
