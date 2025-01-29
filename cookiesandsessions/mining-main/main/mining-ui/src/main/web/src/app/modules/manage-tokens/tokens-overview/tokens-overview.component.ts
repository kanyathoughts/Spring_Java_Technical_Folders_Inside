import { Component, OnInit, TemplateRef, ViewChild } from '@angular/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { ActivatedRoute } from '@angular/router';
import { UntypedFormBuilder, UntypedFormControl, UntypedFormGroup, ValidationErrors, Validators } from '@angular/forms';
import { TranslateService } from '@ngx-translate/core';
import { ConfirmDeleteModalComponent, DELETE_MODAL_CONFIRMED } from '@app/shared/components/confirm-delete-modal/confirm-delete-modal.component';
import { NzModalService } from 'ng-zorro-antd/modal';
import { AuthControllerService } from '@app/core/services/auth-controller/auth-controller.service';
import { TokenInfo } from '@app/core/services/auth-controller/tokenInfo';

@Component({
  selector: 'app-tokens-overview',
  templateUrl: './tokens-overview.component.html',
  styleUrls: ['./tokens-overview.component.less']
})
export class TokensOverviewComponent implements OnInit {
  @ViewChild('removeModalTemplate') removeModalContent: TemplateRef<any>;
  storedTokens: TokenInfo[] = [];
  modalToken: string;
  modalTokenId: string;
  modalTokenDescription: boolean;
  modalTokenValidated: boolean;
  modalProbeToken: string;
  saveTokenModalForm: UntypedFormGroup;

  constructor(
    private message: NzMessageService,
    private route: ActivatedRoute,
    private authService: AuthControllerService,
    private translateService: TranslateService,
    private fb: UntypedFormBuilder,
    private modalService: NzModalService
  ) { }

  ngOnInit(): void {
    this.loadTokens();
    this.route.queryParamMap.subscribe(params => {
      if (params.has('token')) {
        this.authService.getToken(params.get('token')).subscribe({
          next: (token) => this.modalToken = token.bearerToken,
          error: () => this.message.error(this.translateService.instant('manageTokens.retrieveTokenError') as string)
        });
      }
    });
    this.saveTokenModalForm = this.fb.group({
      tokenDescription: ['', [Validators.required, this.checkDuplicateDescription.bind(this)]],
    });
  }

  /**
   * Fetch the tokens for the table
   */
  loadTokens(): void {
    this.authService.getTokens().subscribe({
      next: (result) => this.storedTokens = result
    });
  }

  /**
   * method handles the token operations based on user request
   * @param token to perform action based on user request
   * @param action to identify which type of operation should perform
   */
  handleTokenOperations(token: TokenInfo, action: string): void {
    switch (action) {
      case 'probeToken':
        this.probeToken(token.id);
        break;
      case 'deleteToken':
        this.deleteToken(token.id);
        break;
      case 'editToken':
        this.editToken(token.id, token.description);
        break;
      case 'showToken':
        this.modalToken = token.bearerToken;
        break;
    }
  }

  /**
   *delets tokens from token over view table
   *@param tokenDescription gives the token to be deleted
   */
  deleteToken(tokenDescription: string): void {
    const deleteTokenModal = this.modalService.create<ConfirmDeleteModalComponent>({
      nzTitle: this.translateService.instant('manageTokens.deleteModalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzOkLoading: true,
      nzContent: ConfirmDeleteModalComponent
    });
    const instance = deleteTokenModal.getContentComponent();
    instance.modalContent = this.removeModalContent;
    instance.confirmationButtonText = 'btnLabel.delete';
    instance.isConfirmationReq = false;
    deleteTokenModal.afterClose.subscribe((result: string) => {
      if (result === DELETE_MODAL_CONFIRMED) {
        this.authService.deleteToken(tokenDescription).subscribe({
          next: () => this.loadTokens(),
          error: () => this.message.error(this.translateService.instant('manageTokens.deleteTokenError') as string)
        });
      }
    });
  }

  /**
   * Method updates the selected token
   * @param id as string
   * @param description as string
   */
  editToken(id: string, description: string): void {
    this.saveTokenModalForm.markAsUntouched();
    this.saveTokenModalForm.markAsPristine();
    this.modalTokenId = id;
    this.saveTokenModalForm?.controls['tokenDescription'].setValue(description);
    this.modalTokenDescription = true;
  }

  /**
   * Method opens the create new token modal
   */
  openCreateModal(): void {
    this.modalTokenId = null;
    this.saveTokenModalForm?.controls['tokenDescription'].setValue('Unnamed token ' + new Date().toISOString());
    this.modalTokenDescription = true;
  }

  /**
   * Method saves or updates the token as user requested
   */
  submitTokenRequest(): void {
    if (this.modalTokenId) {
      // edit existing
      this.authService.updateToken({
        id: this.modalTokenId,
        description: this.saveTokenModalForm.get('tokenDescription').value
      }).subscribe({
        next: () => this.loadTokens(),
        error: () => this.message.error(this.translateService.instant('manageTokens.updateTokenError') as string)
      });
      this.modalTokenDescription = false;
    } else {
      // create new
      this.authService.initOffline({
        description: this.saveTokenModalForm.get('tokenDescription').value,
        return_uri: window.location.href.split('?')[0] + '?token='
      }).subscribe({
        next: (redir) => {
          window.location.href = redir;
        },
        error: () => this.message.error(this.translateService.instant('manageTokens.initiateTokenError') as string)
      });
      this.modalTokenDescription = false;
    }
  }

  /**
   * Method checks for the validaty of the token
   * @param id as string
   */
  probeToken(id: string): void {
    this.modalTokenValidated = null;
    this.modalProbeToken = id;
    setTimeout(() => {
      const id = this.modalProbeToken;
      if (id && this.modalTokenValidated == null) {
        this.authService.probeToken(id).subscribe({
          next: () => this.modalTokenValidated = true,
          error: () => this.modalTokenValidated = false
        });
      }
    }, 500);
  }

  /**
   * Method sets the modal on closing the modal
   */
  closeModal(): void {
    this.modalTokenDescription = null;
    this.saveTokenModalForm.markAsUntouched();
    this.saveTokenModalForm.markAsPristine();
  }

  private checkDuplicateDescription(control: UntypedFormControl): ValidationErrors | null {
    const titleIndex = this.storedTokens.findIndex(existingDesc => existingDesc.description.toLowerCase() === control.value.toLowerCase());
    return (titleIndex !== -1) ? { 'duplicate': true } : null;
  }
}
