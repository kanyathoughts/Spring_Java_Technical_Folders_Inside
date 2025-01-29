import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Router } from '@angular/router';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ClientProjectRelationshipService } from '../services/client-project-relationship/client-project-relationship.service';
import { Observable, ObservableInput } from 'rxjs';
import { catchError, map, switchMap, tap } from 'rxjs/operators';
import { of } from 'rxjs';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { ClientControllerService, ClientPojo, EntityId, ProjectControllerService, ProjectPojo } from '@innowake/mining-api-angular-client';

/**
 * This guard provide a Details of client and project from the paraMap.
 */
@Injectable({
    providedIn: 'root'
})
export class ClientProjectGuard  {
  private currentClient: ClientProjectRelationship;

  constructor(
    private router: Router,
    private clientService: ClientControllerService,
    private clientProjectRelationshipService: ClientProjectRelationshipService,
    private projectService: ProjectControllerService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private messageService: NzMessageService,
    private translateService: TranslateService) {
    this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.currentClient = response;
    });
  }

  /*
    The guard is for Calling API to fetch project and client details in case currentClient not present
    through ClientProjectRelationshipservice.
  */
  canActivate(next: ActivatedRouteSnapshot): Observable<boolean> {
    const paramData = next.paramMap.get('projectId') ? next.paramMap.get('projectId') : next.paramMap.get('clientId');
    const params = paramData.split('-');
    const id = Number(params[1]);
    let clientProjectObservable: Observable<any>;
    if (params.length !== 2 || isNaN(id)) {
      void this.router.navigate(['/clients']);
      this.messageService.error(`${this.translateService.instant('dataResolverError', { dataType: params[0] })}`);
    }
    if (params[0] === 'project') {
      if (!this.currentClient || id !== this.currentClient.getProjectId()) {
        clientProjectObservable = this.projectService.findProjectById(id).pipe(
          switchMap((project: ProjectPojo) => {
            if (!this.currentClient || project.clientId !== this.currentClient.getClientId()) {
              return this.clientService.findClientById(project.clientId).pipe(map((client: ClientPojo) => {
                this.clientProjectRelationshipService.setClientProject(client, project);
                return true;
              }));
            } else {
              this.clientProjectRelationshipService.setProject(project);
              return of(true);
            }
          }
          ));
      } else {
        return of(true);
      }
    } else if (params[0] === 'client') {
      if (!this.currentClient || id !== this.currentClient.getClientId() as EntityId) {
        clientProjectObservable = this.clientService.findClientById(id).pipe(
          tap((client: ClientPojo) => {
            this.clientProjectRelationshipService.setClientProject(client);
          })
        );
      } else {
        return of(true);
      }
    }
    return clientProjectObservable.pipe(map(
      (response) => response ? true : false
    ), catchError((): ObservableInput<boolean> => {
      void this.router.navigate(['/clients']);
      this.messageService.error(`${this.translateService.instant('dataResolverError', { dataType: params[0] })}`);
      return of(false);
    }));
  }

  canActivateChild(next: ActivatedRouteSnapshot): Observable<boolean> {
    return this.canActivate(next);
  }
}
