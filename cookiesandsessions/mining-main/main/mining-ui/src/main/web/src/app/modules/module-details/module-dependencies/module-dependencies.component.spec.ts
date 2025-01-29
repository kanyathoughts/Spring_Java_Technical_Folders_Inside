import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { ModuleDependenciesComponent } from './module-dependencies.component';
import {Location} from '@angular/common';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { I18nService } from '@app/core';
import { HttpClient } from '@angular/common/http';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { AnnotationControllerService, DataDictionaryControllerService, FeatureControllerService, JobControllerService, ModuleControllerService, ProjectControllerService, ReferenceControllerService, TaxonomyControllerService } from '@innowake/mining-api-angular-client';

describe('ModuleDependenciesComponent', () => {
  let component: ModuleDependenciesComponent;
  let fixture: ComponentFixture<ModuleDependenciesComponent>;
  let loc: Location;
  let router: Router;
  const i18nServiceSpy = { language: 'en-US' };
  const httpServiceSpy = jasmine.createSpyObj<HttpClient>('HttpClient', ['disableApiPrefix', 'get']);
  const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['init', 'mapLabel']);
  const referenceControllerServiceSpy = jasmine.createSpyObj<ReferenceControllerService>('ReferenceControllerService', ['findAllReferencesForModule']);
  const jobControllerServiceSpy: jasmine.SpyObj<JobControllerService> =jasmine.createSpyObj<JobControllerService>('JobControllerService', [
    'getJobInformation'
  ]);
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [RouterTestingModule,
        TranslateModule.forRoot({}),
        RouterTestingModule,
        BrowserAnimationsModule
      ],
      declarations: [ModuleDependenciesComponent],
      providers: [
        Location,
        {provide: ReferenceControllerService, useValue: referenceControllerServiceSpy},
        TranslateService,
        FeatureToggleService,
        FeatureControllerService,
        NumberFormatter,
        ModuleControllerService,
        AnnotationControllerService,
        ProjectControllerService,
        DataDictionaryControllerService,
        { provide: I18nService, useValue: i18nServiceSpy },
        { provide: HttpClient, useValue: httpServiceSpy },
        { provide: LabelMappingService, useValue:labelMappingServiceSpy },
        TaxonomyControllerService,
        { provide: JobControllerService, useValue: jobControllerServiceSpy }
      ]
    }).compileComponents();

    loc = TestBed.inject(Location);
    router = TestBed.inject(Router);
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ModuleDependenciesComponent);
    component = fixture.componentInstance;
    component.moduleId = 1;
    component.projectId = 1;
    component.moduleHash = 'hjvdjfwgfiwkhgjfaksdbhkjbfkjadkakhjf'
    fixture.detectChanges();
  });

  afterEach(() => {
    fixture.destroy();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
    expect(component.routeDependencies).toEqual('/project-1/module-hjvdjfwgfiwkhgjfaksdbhkjbfkjadkakhjf/dependencies');
    expect(component.additionalGraphQLParams).toEqual({ moduleId: 1 });
  });

});
