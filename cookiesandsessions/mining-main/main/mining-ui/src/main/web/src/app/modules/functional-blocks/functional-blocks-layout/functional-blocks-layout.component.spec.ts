import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FunctionalBlocksLayoutComponent } from './functional-blocks-layout.component';
import { GraphQlControllerService } from '@app/core/services/graphql.service'
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { of } from 'rxjs/internal/observable/of';
import { ActivatedRoute } from '@angular/router';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { Title } from '@angular/platform-browser';

class MockActivatedRoute {
  params = of({ uuid: 'sample_uuid' });
}

describe('FunctionalBlocksLayoutComponent', () => {
  let component: FunctionalBlocksLayoutComponent;
  let fixture: ComponentFixture<FunctionalBlocksLayoutComponent>;
  const graphQlData: any =  {"data": { "project": {"customPropertyClasses": { Annotation: ["AnnotationCustomProperties"], Client: ["ClientCustomProperties"], Module: ["ModuleCustomProperties"], Project: ["ProjectCustomProperties"], Taxonomy: ["TaxonomyCustomProperties"] } }
  }};
  const graphQlControllerServiceSpy: jasmine.SpyObj<GraphQlControllerService> = jasmine.createSpyObj(
    'GraphQlControllerService',
    ['graphQl']
  );
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable']);
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [TranslateModule.forRoot({})],
      declarations: [ FunctionalBlocksLayoutComponent ],
      providers: [
        TranslateService,
        Title,
        { provide: GraphQlControllerService, useValue: graphQlControllerServiceSpy },
        { provide: ActivatedRoute, useClass: MockActivatedRoute },
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy }
      ]
    })
    .compileComponents();
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(new ClientProjectRelationship(1, 'client', 1, 'project')));
    graphQlControllerServiceSpy.graphQl.and.returnValue(of(graphQlData as any));
  });

    beforeEach(() => {
        fixture = TestBed.createComponent(FunctionalBlocksLayoutComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});


