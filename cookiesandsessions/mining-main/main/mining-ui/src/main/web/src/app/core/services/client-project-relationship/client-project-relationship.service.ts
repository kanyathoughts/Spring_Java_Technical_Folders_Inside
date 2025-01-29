import { Injectable } from '@angular/core';
import { Observable, BehaviorSubject } from 'rxjs';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { StateMaintainenceService } from '../state-maintenance/state-maintainence.service';
import { ClientPojo, ProjectPojo } from '@innowake/mining-api-angular-client';

@Injectable({
  providedIn: 'root'
})
export class ClientProjectRelationshipService {

  static readonly MINING_CURRENT_CLIENT = 'miningCurrentClient';
  currentClient: ClientProjectRelationship;
  currentClientProjectRelationObserver: BehaviorSubject<ClientProjectRelationship> = new BehaviorSubject
    <ClientProjectRelationship>(null);

  constructor(private stateMaintainenceService: StateMaintainenceService) {
    if (JSON.parse(localStorage.getItem(ClientProjectRelationshipService.MINING_CURRENT_CLIENT))) {
      const miningCurrentClient: ClientProjectRelationship = JSON.parse(localStorage.getItem(ClientProjectRelationshipService.MINING_CURRENT_CLIENT));
      this.currentClient = new ClientProjectRelationship(
        miningCurrentClient['clientId'],
        miningCurrentClient['clientName'],
        miningCurrentClient['projectId'],
        miningCurrentClient['projectName'],
        miningCurrentClient['metricsDate']
      );
      this.currentClientProjectRelationObserver.next(this.currentClient);
    }
  }

  setClientProject(client: ClientPojo, project?: ProjectPojo): void {
    const clientProjectRelation = new ClientProjectRelationship(
      client.id,
      client.name,
      project?.id,
      project?.name,
      project?.metricsDate
    );
    if (this.currentClient && project?.id !== this.currentClient.getProjectId()) {
      this.stateMaintainenceService.clearStateOnProjectChange();
    }
    this.setClientProjectRelationship(clientProjectRelation);
  }

  setProject(project: ProjectPojo): void {
    const client: ClientPojo = {
      id: this.currentClient.getClientId(),
      name: this.currentClient.getClientName()
    };
    this.setClientProject(client, project);
  }

  setClientProjectRelationship(clientProject: ClientProjectRelationship): void {
    localStorage.setItem(ClientProjectRelationshipService.MINING_CURRENT_CLIENT, JSON.stringify(clientProject));
    this.currentClientProjectRelationObserver.next(clientProject);
    this.currentClient = new ClientProjectRelationship(
      clientProject?.getClientId(),
      clientProject?.getClientName(),
      clientProject?.getProjectId(),
      clientProject?.getProjectName(),
      clientProject?.getLastScanDate()
    );
  }

  getClientProjectObservable(): Observable<ClientProjectRelationship> {
    return this.currentClientProjectRelationObserver;
  }
}
