import { TestBed } from '@angular/core/testing';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';

import { NoAuthorizationService } from './no-authorization.service';

describe('NoAuthorizationService', () => {
  let service: NoAuthorizationService;
  const clientProjectRelatiomshipMock: ClientProjectRelationship = new ClientProjectRelationship(1, 'Client test', 1, 'Project Test');

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        NoAuthorizationService
      ]
    });
    service = TestBed.inject(NoAuthorizationService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should return true for admin role', () => {
    const result = service.isAdmin();
    expect(result).toBeTrue();
  });

  it('should return true for client admin role', () => {
    const result = service.isClientAdmin();
    expect(result).toBeTrue();
  });

  it('should return true for user role', () => {
    const result = service.hasUserRole();
    expect(result).toBeTrue();
  });

  it('should return true for project nature', () => {
    const result = service.hasProjectNature();
    expect(result).toBeTrue();
  });
});
