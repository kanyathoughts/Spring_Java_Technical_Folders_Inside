import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { DependencyExplorerComponent  } from './dependency-explorer.component';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { WindowToken } from '@app/core/utils/window';
import { of, throwError } from 'rxjs';
import { Location } from '@angular/common';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { OauthtokenService } from '@app/core/authentication/oauthtoken.service';
import { ProjectShellComponent } from '@app/shell/project-shell/project-shell.component';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { License } from 'yfiles';
import { environment } from '@env/environment';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { ContextualToolbarComponent } from '../dependency-graph/context-menu-toolbar/context-menu-toolbar.component';
import { FilterParameters } from '@app/modules/graph/models/yfile-graph-info.model';
import { GraphOverviewPanelComponent } from '../../yfiles-graph-overview-panel/graph-overview-panel.component';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { ApiPrefixInterceptor, ErrorHandlerInterceptor, HttpService } from '@app/core';
import { HttpClient } from '@angular/common/http';
import { NzSelectModule } from 'ng-zorro-antd/select';
import { CancelRequestOnNavigationInterceptor } from '@app/core/http/cancel-request-on-navigation.interceptor';
import { ClientControllerService, DependencyGraph, FeatureControllerService, ModuleControllerService, ProjectControllerService, ReferenceControllerService } from '@innowake/mining-api-angular-client';
import { SetSerializationInterceptor } from '@app/core/http/set-serialization.interceptor';
import { ReachabilityService } from '@app/modules/reachability-ui-product-vision/utils/reachability.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { NO_ERRORS_SCHEMA } from '@angular/core';

export const DEPENDENCY_GRAPH_LINKS: DependencyGraph = {
  modules: [
    {
      id: 100,
      name: 'ABC',
      technology: 'CSD',
      type: 'TRANSACTION',
      description: 'Node ABC'
    },
    {
      id: 200,
      name: 'BCD',
      technology: 'JCL',
      type: 'PROC'
    },
    {
      id: 300,
      name: 'CDE',
      technology: 'ASSEMBLER',
      type: 'ADAPTVIEW'
    }
  ],
  references: [
    { srcModule: '100', dstModule: '200', relationship: 'CALLS' },
    { srcModule: '200', dstModule: '300', relationship: 'CALLS' },
    { srcModule: '300', dstModule: '100', relationship: 'CALLS' }
  ],
  moduleTypes: [
    'CSD TRANSACTION',
    'JCL PROC',
    'ASSEMBLER ASSEMBLER'
  ],
  relationshipTypes: [
    'CALLS'
  ]
};
const EMPTY_DEPENDENCY_GRAPH_LINKS: DependencyGraph = {
  modules: [],
  references: [],
  moduleTypes: [],
  relationshipTypes: []
};
describe('DependencyExplorerComponent', () => {
  License.value = environment.yFilesLicense;
  let component: DependencyExplorerComponent;
  let fixture: ComponentFixture<DependencyExplorerComponent>;
  let mockWindow: any;
  let openedUrl = '';
  const moduleControllerServiceSpy: jasmine.SpyObj<ModuleControllerService> = jasmine.createSpyObj('ModuleControllerService', ['traverseModuleDependencies',
                                                                                                                               'countRequiresReview']);
  const referenceControllerServiceSpy: jasmine.SpyObj<ReferenceControllerService> = jasmine.createSpyObj('ReferenceControllerService', ['findAllReferencesForModule']);

  beforeEach(waitForAsync(() => {
    mockWindow = {
      get location() {
        return {
          href: 'http://localhost:8081/#/browse-modules/1/1/1/explore'
        };
      },
      open: (sUrl: any) => {
        openedUrl = sUrl;
      }
    };
    mockWindow.open.bind(mockWindow);

    TestBed.configureTestingModule({
      schemas: [NO_ERRORS_SCHEMA],
      imports: [
        BrowserAnimationsModule,
        FormsModule,
        HttpClientTestingModule,
        ReactiveFormsModule,
        NzSelectModule,
        TranslateModule.forRoot({}),
      ],
      declarations: [
        DependencyExplorerComponent
      ],
      providers: [
        { provide: ModuleControllerService, useValue: moduleControllerServiceSpy },
        { provide: ReferenceControllerService, useValue: referenceControllerServiceSpy},
        OauthtokenService,
        { provide: WindowToken, useValue: mockWindow },
        ProjectShellComponent,
        ClientControllerService,
        ProjectControllerService,
        History,
        Location,
        ClientProjectRelationshipService,
        TranslateService,
        SetSerializationInterceptor,
        DeepLinkService,
        FeatureControllerService,
        ApiPrefixInterceptor,
        ErrorHandlerInterceptor,
        CancelRequestOnNavigationInterceptor,
        ReachabilityService,
        GraphQlControllerService,
        {
            provide: HttpClient,
            useClass: HttpService
        },
        {
          provide: ActivatedRoute,
          useValue: {
            data: of({
                module: {
                    id: 1,
                    projectId: 1,
                    path: 'test'
                }
            }),
            snapshot: {
              paramMap: {
                get: () => 1,
              }
            }
          }
        }
      ]
    }).compileComponents();
    moduleControllerServiceSpy.traverseModuleDependencies.and.returnValue(of(DEPENDENCY_GRAPH_LINKS as any));
    moduleControllerServiceSpy.countRequiresReview.and.returnValue(of(1 as any));
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DependencyExplorerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
    expect(component.loadState).toBe(LoaderState.success);
    expect(component.isRedirected).toBeFalsy();
    expect(localStorage.getItem('1-depth')).toBeFalsy();
  });

  it('should have values', () => {
    expect(component.sliderVal).toBe(1);
    setTimeout(() => {
      expect(component.loadState).toBe(LoaderState.success);
    }, 0);
  });

  it('should change depth of the graph', () => {
    component.loadState = LoaderState.loading;
    component.depthChange(2);
    expect(component.loadState).toBe(LoaderState.success);
  });

  it('should change layout of the graph', () => {
    component.layout = 'HIERARCHIC_LAYOUT';
    component.changeLayout('ORGANIC_LAYOUT');
    expect(component.layout).toBe('ORGANIC_LAYOUT');
  });

  it('should create filter query and get graph', () => {
    const onlyModuleFilterParams: FilterParameters = {
      moduleFilter: ['COBOL_COPYBOOK'],
      relationshipFilter: []
    };
    const onlyRelationshipFilterParams: FilterParameters = {
      moduleFilter: [],
      relationshipFilter: ['CALLS']
    };
    const emptyFilterParams: FilterParameters = {
      moduleFilter: [],
      relationshipFilter: []
    };
    const bothFilterParams: FilterParameters = {
      moduleFilter: ['COBOL_COPYBOOK'],
      relationshipFilter: ['CALLS']
    };
    component.changeFilter(onlyModuleFilterParams);
    expect(moduleControllerServiceSpy.traverseModuleDependencies).toHaveBeenCalledWith(1, 1, 1, 800,'modules=in=(COBOL_COPYBOOK)', true);

    component.changeFilter(onlyRelationshipFilterParams);
    expect(moduleControllerServiceSpy.traverseModuleDependencies).toHaveBeenCalledWith(1, 1, 1, 800, 'relationships=in=(CALLS)', true);

    component.changeFilter(emptyFilterParams);
    expect(moduleControllerServiceSpy.traverseModuleDependencies).toHaveBeenCalledWith(1, 1, 1, 800, undefined, true);

    component.changeFilter(bothFilterParams);
    expect(moduleControllerServiceSpy.traverseModuleDependencies).toHaveBeenCalledWith(1, 1, 1, 800, 'modules=in=(COBOL_COPYBOOK) and relationships=in=(CALLS)', true);

    component.changeFilter({} as any);
    expect(moduleControllerServiceSpy.traverseModuleDependencies).toHaveBeenCalledWith(1, 1, 1, 800, undefined, true);
  });

  describe('empty module list', () => {
    beforeEach(() => {
      fixture = TestBed.createComponent(DependencyExplorerComponent);
      component = fixture.componentInstance;
      moduleControllerServiceSpy.traverseModuleDependencies.and.returnValue(of (EMPTY_DEPENDENCY_GRAPH_LINKS as any));
      fixture.detectChanges();
    });

    it('should have load state as error because module list is empty', () => {
      fixture.detectChanges();
      expect(component.loadState).toBe(LoaderState.error);
    });
  });

  describe('error', () => {
    beforeEach(() => {
      fixture = TestBed.createComponent(DependencyExplorerComponent);
      component = fixture.componentInstance;
      moduleControllerServiceSpy.traverseModuleDependencies.and.returnValues(throwError('TEST_ERROR'));
      fixture.detectChanges();
    });

    it('should have load state as error because rest call gave error', () => {
      fixture.detectChanges();
      expect(component.loadState).toBe(LoaderState.error);
    });
  });
});
