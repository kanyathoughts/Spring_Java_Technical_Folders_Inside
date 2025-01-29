import { Component, OnInit, Input } from '@angular/core';
import { UntypedFormGroup, UntypedFormControl, Validators } from '@angular/forms';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { BaseFormModalComponent } from '@app/shared/components/base-form-modal/base-form-modal.component';
import { ClientControllerService, ClientControllerV2Service, ClientPojo, ClientPojoPrototype } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'mn-client-form-modal',
  templateUrl: './client-form-modal.component.html'
})
export class ClientFormModalComponent extends BaseFormModalComponent implements OnInit {

  @Input() client: ClientPojo;

  @Input() clientLogo: string;

  clientForm = new UntypedFormGroup({
    name: new UntypedFormControl('', Validators.required)
  });

  inputFile: File[] = [];

  logoSrc: string;

  isLoading = false;

  constructor(
    public modal: NzModalRef,
    public messageService: NzMessageService,
    private clientLogoService: ClientControllerV2Service,
    private clientV2Service: ClientControllerV2Service,
    private clientService: ClientControllerService,
    private translateService: TranslateService
  ) {
    super(modal, messageService);
  }

  get clientName(): string {
    return this.clientForm.get('name').value;
  }

  ngOnInit(): void {
    if (this.client) {
      this.mode = 'edit';
      this.clientForm.get('name').setValue(this.client.name);
      if (this.clientLogo) {
        this.logoSrc = this.clientLogo;
      }
    }
  }

  /**
   * Method called by the submit button
   */
  onSubmit(): void {
    this.isLoading = true;
    if (this.mode === 'create') {
      this.createClient();
    } else {
      this.updateClient();
    }
  }

  /**
   * Handles the file submited and block the automatic upload of the component
   * @param file NG-Zorro file
   */
  beforeImageUpload = (file: File): boolean => {
    const reader = new FileReader();
    reader.readAsDataURL(file);
    reader.onload = () => {
      this.logoSrc = reader.result as string;
    };
    this.inputFile = this.inputFile.concat(file);
    return false;
  };

  /**
   * Method called to delete the client logo in the form
   */
  deleteLogo(): void {
    this.inputFile = [];
    this.logoSrc = undefined;
  }

  private createClient(): void {
    const newClient: ClientPojoPrototype = {
      name: this.clientName
    };
    this.clientV2Service.createClientV2(newClient).subscribe(updatedClient => {
      if (this.inputFile.length === 1) {
        this.clientLogoService.createLogo(updatedClient.id, this.inputFile[0]).subscribe(() => {
          this.closeModal('success', `${this.translateService.instant('successCreate', {name: updatedClient.name})}`, updatedClient);
        }, () => {
          this.closeModal('warning', `${this.translateService.instant('clientForm.errorCreateLogo')}`, updatedClient);
        });
      } else {
        this.closeModal('success', `${this.translateService.instant('successCreate', {name: updatedClient.name})}`, updatedClient);
      }
      this.isLoading = false;
    }, () => {
      this.closeModal('error', `${this.translateService.instant('errorCreate', {name: newClient.name})}`, false);
      this.isLoading = false;
    });
  }

  private updateClient(): void {
    const updateClient = {...this.client};
    updateClient.name = this.clientName;
    this.clientService.updateClient(updateClient.id, updateClient as ClientPojoPrototype).subscribe(updatedClient => {
      this.client = updatedClient;
      if (this.inputFile.length === 1) {
        if (this.clientLogo) {
          this.clientLogoService.updateLogo(updatedClient.id, this.inputFile[0]).subscribe(() => {
            this.closeModal('success', `${this.translateService.instant('successEdit', {name: updatedClient.name})}`, updatedClient);
          }, () => {
            this.closeModal('warning', `${this.translateService.instant('clientForm.errorEditLogo')}`, updatedClient);
          });
        } else {
          this.clientLogoService.createLogo(updatedClient.id, this.inputFile[0]).subscribe(() => {
            this.closeModal('success', `${this.translateService.instant('successEdit', {name: updatedClient.name})}`, updatedClient);
          }, () => {
            this.closeModal('warning', `${this.translateService.instant('clientForm.errorCreateLogo')}`, false);
          });
        }
      } else {
        if (this.clientLogo && !this.logoSrc) {
          this.clientLogoService.deleteLogo(updatedClient.id).subscribe(() => {
            this.closeModal('success', `${this.translateService.instant('successEdit', {name: updatedClient.name})}`, updatedClient);
          }, () => {
            this.closeModal('warning', `${this.translateService.instant('clientForm.errorDeleteLogo')}`, updatedClient);
          });
        } else {
          this.closeModal('success', `${this.translateService.instant('successEdit', {name: updatedClient.name})}`, updatedClient);
        }
      }
      this.isLoading = false;
    }, () => {
      this.closeModal('error', `${this.translateService.instant('errorEdit', {name: this.client.name})}`, false);
      this.isLoading = false;
    });
  }
}
