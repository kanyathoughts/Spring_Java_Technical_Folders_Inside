import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { ActivatedRoute } from '@angular/router';
import { of } from 'rxjs';
import { ExtensionHostComponent } from './extension-host.component';
import { DomSanitizer } from '@angular/platform-browser';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { MiningUiExtensionsControllerService } from '@innowake/mining-api-angular-client';

describe('ExtensionHostComponent', () => {
  let component: ExtensionHostComponent;
  let fixture: ComponentFixture<ExtensionHostComponent>;

  const fakeActivatedRoute = { 
    snapshot: { paramMap: { get(): string { return 'test'; } } },
    data: of({ project : { id: 1, clientId: 1}})
  };
  const extensionData = [{
    name: 'Test',
    pageIdentifier: 'test',
    properties : {
      IFRAME_SRC: '/test?clientId=${clientId}&projectId=${projectId}'
    },
    styleSheets: ['test'],
    hostElementStyleNames: ['test'],
    inlineStyles: 'testing'
  }];
  let domSanitizer: DomSanitizer;
  
  const extensionServiceSpy = jasmine.createSpyObj('MiningUiExtensionsControllerService', ['getWebUiExtensions']);
  const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(1, 'TestClient', 1, 'TestProject');
  const clientProjectRelationshipServiceSpy: jasmine.SpyObj<ClientProjectRelationshipService> = jasmine.createSpyObj('ClientProjectRelationshipService',
    ['getClientProjectObservable', 'setClientProjectRelationship']);
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ ExtensionHostComponent ],
      providers: [
        { provide: ClientProjectRelationshipService, useValue: clientProjectRelationshipServiceSpy },
        { provide: ActivatedRoute, useValue: fakeActivatedRoute },
        { provide: MiningUiExtensionsControllerService, useValue: extensionServiceSpy }
      ]
    })
    .compileComponents();
    clientProjectRelationshipServiceSpy.getClientProjectObservable.and.returnValue(of(clientProjectRelationship as any));
  }));

  beforeEach(() => {
    extensionServiceSpy.getWebUiExtensions.and.returnValue(of(extensionData as any));
    fixture = TestBed.createComponent(ExtensionHostComponent);
    domSanitizer = TestBed.inject(DomSanitizer);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should set the src url', () => {
    (component as any).setSrcUrl();
    expect((component as any).srcUrl).toEqual(domSanitizer.bypassSecurityTrustResourceUrl('/test?clientId=1&projectId=1'));
  });
});
