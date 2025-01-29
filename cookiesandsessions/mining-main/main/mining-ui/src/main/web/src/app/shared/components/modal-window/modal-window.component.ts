import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { ModalWindowParams } from '../../models/modal-window-params.model';
import { UntypedFormGroup, UntypedFormBuilder } from '@angular/forms';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { BaseFormModalComponent } from '../base-form-modal/base-form-modal.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { ModuleBadgeUpdateService } from '@app/core/services/module-details/module-badge-update.service';
import { BadgeCountUpdateOperation } from '@app/modules/module-details/models/module-complexity-details';
import { Subscription } from 'rxjs';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { TranslateService } from '@ngx-translate/core';
import { EntityId, ModuleControllerService, ProjectRole } from '@innowake/mining-api-angular-client';
@Component({
  selector: 'app-modal-window',
  templateUrl: './modal-window.component.html'
})
export class ModalWindowComponent  extends BaseFormModalComponent implements OnInit, OnDestroy {
  @Input() customDynamicDialogConfig: any;
  @Input() placeHolderText: string;
  @Input() projectId: number;
  @Input() moduleId: EntityId;
  descriptionGenerating = false;
  gptTranslateActive = false;
  disableSaveButton: boolean;
  modalForm: UntypedFormGroup;
  display = false;
  editDetails: ModalWindowParams[] = [];
  selectedValue: string;
  isDestroyed = false;
  isManager: boolean;
  showDeleteModal = false;
  isGenerationSuccess = false;
  private clientProjectSubscription: Subscription;

  constructor(public modal: NzModalRef,
    public messageService: NzMessageService,
    private formBuilder: UntypedFormBuilder,
    private authorizationService: KeycloakAuthorizationService,
    private relationshipService: ClientProjectRelationshipService,
    private moduleBadgeUpdateService: ModuleBadgeUpdateService,
    private moduleService: ModuleControllerService,
    private featureToggleService: FeatureToggleService,
    private translateService: TranslateService) {
    super(modal, messageService);
  }

  ngOnInit(): void {
    this.editDetails = this.customDynamicDialogConfig.data;
    const keyValue = {};
    this.editDetails.forEach((editDetail: ModalWindowParams) => {
      keyValue[editDetail.key] = editDetail.value;
    });
    this.modalForm = this.formBuilder.group(keyValue);
    this.clientProjectSubscription = this.relationshipService.getClientProjectObservable().subscribe(currentClient => {
    this.isManager = this.authorizationService.hasUserRole(currentClient, ProjectRole.UserRoleEnum.MANAGER);
    });
    this.featureToggleService.isActive('generativeAiTranslations').subscribe((isActive: boolean) => {
      this.gptTranslateActive = isActive;
    });
  }
  /**
   * Saves the edited details.
   */
  save(): void {
    this.customDynamicDialogConfig.saveCallBack(this.modalForm.value);
  }

  generateDescription(): void{
    this.disableSaveButton = true;
    this.descriptionGenerating = true;
    this.modalForm.controls[this.translateService.instant('description')].disable();
    this.moduleService.generateModuleDescription(this.projectId, this.moduleId).subscribe(
      response => {
        this.descriptionGenerating = false;
        this.isGenerationSuccess = true;
        this.modalForm.controls[this.translateService.instant('description')].setValue(response.description);
        this.modalForm.controls[this.translateService.instant('description')].enable();
        this.modalForm.controls[this.translateService.instant('description')].markAsPristine();
        this.disableSaveButton = false;
      }, (error) => {
        this.descriptionGenerating = false;
        this.isGenerationSuccess = false;
        this.messageService.error(error.error.message as string);
      });
  }

  /**
   * Cancels the editing operation.
   */
  cancel(): void {
    if (this.modalForm.dirty) {
      this.display = true;
    } else {
      this.display = false;
      super.onCancel();
    }
  }

  /**
   * Closes the modal window with a confirmation dialog.
   * @param continueCancel confirmation to close the modal window.
   */
  closeEditModal(continueCancel: boolean): void {
    this.display = false;
    if (continueCancel) {
      super.onCancel();
    }
  }

  /**
   * Delete the subject being edited on confirmation.
   */
  delete(shouldDelete: boolean): void {
    this.showDeleteModal = true;
    if (shouldDelete) {
      this.customDynamicDialogConfig.deleteCallBack();
      this.showDeleteModal = false;
      this.moduleBadgeUpdateService.updateAnnotationDataDictionary({operation: BadgeCountUpdateOperation.ANNOTATION_DELETED, count: 1});
    }
  }

  openInEclipse(): void {
    this.customDynamicDialogConfig.openInEclipseCallback();
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
  }
}
