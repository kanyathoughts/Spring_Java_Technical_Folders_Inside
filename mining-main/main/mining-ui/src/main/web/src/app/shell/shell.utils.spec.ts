import { TestBed } from '@angular/core/testing';

import { AuthenticationGuard, AuthenticationService } from '@app/core';
import { ProjectShellComponent } from './project-shell/project-shell.component';
import { Shell } from './shell.utils';
import { BaseHeaderShellComponent } from './base-shell/base-header-shell.component';
import { Route } from '@angular/router';

describe('Shell utils', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [BaseHeaderShellComponent, ProjectShellComponent],
      providers: [
        AuthenticationGuard,
        { provide: AuthenticationService }
      ]
    });
  });

  describe('projectShellChildRoutes', () => {
    it('should create routes as children of the project shell', () => {
      // Prepare
      const testRoutes = [{ path: 'test' }];

      // Act
      const result: Route = Shell.projectShellChildRoutes(testRoutes);

      // Assert
      expect(result.path).toBe('');
      expect(result.component).toBe(ProjectShellComponent);
    });
  });

  describe('headerShellChildRoutes', () => {
    it('should create routes as children of the header shell', () => {
      // Prepare
      const testRoutes = [{ path: 'test' }];
      // Act
      const result = Shell.headerShellChildRoutes(testRoutes);
      // Assert
      expect(result.path).toBe('');
      expect(result.component).toBe(BaseHeaderShellComponent);
    });
  });
});
