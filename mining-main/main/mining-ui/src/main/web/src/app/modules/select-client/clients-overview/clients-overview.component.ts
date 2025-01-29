import { Component, OnInit } from '@angular/core';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { ClientFormModalComponent } from '@app/modules/admin-client-project/client-form-modal/client-form-modal.component';
import { TranslateService } from '@ngx-translate/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientControllerV2Service, ClientPojo, PagedClientPojo } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'mn-clients-overview',
  templateUrl: './clients-overview.component.html'
})
export class ClientsOverviewComponent implements OnInit {

  clientList: ClientPojo[];

  loading: boolean;

  constructor(
    public authorizationService: KeycloakAuthorizationService,
    private clientService: ClientControllerV2Service,
    private modalService: NzModalService,
    private translateService: TranslateService,
    private clientProjectRelationshipService: ClientProjectRelationshipService
  ) { }

  ngOnInit(): void {
    this.clientProjectRelationshipService.setClientProjectRelationship(null);
    this.updateClientList();
  }

  /**
   * Handles modals closure
   * @param event result after the modal closure
   */
  onClientUpdate(event: string): void {
    if ( ! event || event === 'cancel') {
      return;
    }

    this.updateClientList();
  }

  /**
   * Opens a modal for Client creation
   */
  openCreateModal(): void {
    const createModal = this.modalService.create({
      nzTitle: this.translateService.instant('clientForm.createModalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzContent: ClientFormModalComponent
    });
    this.setSubscriptionForClientUpdate(createModal);
  }

  private setSubscriptionForClientUpdate(createModal: NzModalRef) {
    createModal.afterClose.subscribe((result: string) => {
      this.onClientUpdate(result);
    });
  }

  private updateClientList(): void {
    this.loading = true;
    const sortBy: string[]= ['name;ASC'];
    this.clientService.getAllClients(0,0,sortBy).subscribe((clientPage: PagedClientPojo) => {
      this.clientList = clientPage.content;
      this.loading = false;
    });
  }
}
