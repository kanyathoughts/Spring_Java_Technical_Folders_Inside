/* eslint-disable @typescript-eslint/no-unused-vars */
import { Injectable } from '@angular/core';
import { AuthorizationService } from './authorization-service.interface';

/**
 * This service is used to replace the Authorization service when the app isn't using Authorization
 */
@Injectable()
export class NoAuthorizationService implements AuthorizationService {
  isAdmin(): boolean {
    return true;
  }

  isClientAdmin(): boolean {
    return true;
  }

  hasAccessToClientProjects(): boolean {
    return true;
  }

  hasUserRole(): boolean {
    return true;
  }

  hasProjectNature(): boolean {
    return true;
  }
}
