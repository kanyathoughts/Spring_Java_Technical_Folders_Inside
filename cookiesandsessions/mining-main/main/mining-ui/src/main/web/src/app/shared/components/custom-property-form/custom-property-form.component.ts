import { Component, Input, OnInit, ViewChild } from '@angular/core';
import { UntypedFormBuilder, UntypedFormGroup } from '@angular/forms';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { BaseFormModalComponent } from '../base-form-modal/base-form-modal.component';
import { CustomPropertyFieldListComponent } from '../custom-property-editor/custom-property-field-list/custom-property-field-list.component';
import { ModuleControllerService, ModulePojo, ProjectPojoCustomProperties } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-custom-property-form',
  templateUrl: './custom-property-form.component.html',
  styleUrls: ['./custom-property-form.component.less']
})
export class CustomPropertyFormComponent extends BaseFormModalComponent implements OnInit {

  @ViewChild(CustomPropertyFieldListComponent) customPropertyList: CustomPropertyFieldListComponent;
  @Input() customProperties: { [key: string]: ProjectPojoCustomProperties[]; };
  @Input() currentClient: ClientProjectRelationship;
  @Input() selectedModule: ModulePojo;
  customPropertiesForm: UntypedFormGroup;
  isLoading = false;
  isCancel = false;

  constructor(
    public modal: NzModalRef,
    private notification: NzNotificationService,
    private translateService: TranslateService,
    private fb: UntypedFormBuilder,
    private moduleControlService: ModuleControllerService,
    public messageService: NzMessageService) {
    super(modal, messageService);
     }

  ngOnInit(): void {
    this.customPropertiesForm = this.fb.group({});
    this.isCancel = false;
  }

  /**
   * Closes the modal on cancel.
   */
   handleCancel(): void {
    this.isCancel = true;
    this.customPropertiesForm.reset();
    super.onCancel();
  }

  /**
   * Returns if the form can be submitted. This is the case if its status is VALID and if the user made changes.
   * Otherwise `false` is returned.
   * @returns `true` if the form is valid. Otherwise `false` for the edit.
   */
  canSubmitForm(): boolean {
    return this.customPropertiesForm.valid && this.customPropertiesForm.dirty;
  }

  /**
   * Saves the edited values of custom properties.
   */
   saveCustomProperties(): void {
    this.isLoading = true;
    this.selectedModule.customProperties = this.customPropertyList?.getSubmitValue();
    this.moduleControlService.updateModule(this.currentClient.getProjectId(), this.selectedModule.id, this.selectedModule)
      .subscribe((resp) => {
        if (resp) {
          this.customProperties = resp.customProperties;
          this.closeModal('success', `${this.translateService.instant('customPropSavedSuccess')}`, resp);
          this.customPropertiesForm.reset();
          this.isLoading = false;
        }
      }, () => {
        this.customPropertiesForm.reset();
        this.isLoading = false;
        const errorContent: string = this.translateService.instant('contactSupport');
        this.notification.create('error', `<b>${this.translateService.instant('errorNotification.title')}</b>`, errorContent);
        this.onCancel();
      });
  }
}

