import { TestBed, ComponentFixture, waitForAsync } from '@angular/core/testing';
import { ClientProjectRelationshipService } from './client-project-relationship.service';
import { Component } from '@angular/core';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ClientPojo, ProjectPojo } from '@innowake/mining-api-angular-client';

const clientProjectRelationship: ClientProjectRelationship = new ClientProjectRelationship(1, 'TestClient', 1, 'TestProject');

const client: ClientPojo = {
  id: 1,
  name: 'TestClient'
};
const project: ProjectPojo = {
  id: 1,
  name: 'TestProject'
}

@Component({
  template: ''
})
class TestComponent {
  clientProjectRelationship: ClientProjectRelationship;
  constructor(private clientProjectRelationshipService: ClientProjectRelationshipService) {
    this.clientProjectRelationshipService.setClientProject(client, project);
  }
  getClientProjectObservable() {
    return this.clientProjectRelationshipService.getClientProjectObservable();
  }
}

describe('ClientProjectRelationshipService', () => {
  let fixture: ComponentFixture<TestComponent>;
  let component: TestComponent;
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [TestComponent],
      providers: [TestComponent]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TestComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    const service: ClientProjectRelationshipService = TestBed.inject(ClientProjectRelationshipService);
    expect(service).toBeTruthy();
    service.currentClient = clientProjectRelationship;
    service.setProject(project);
    expect(component).toBeTruthy();
  });
});
