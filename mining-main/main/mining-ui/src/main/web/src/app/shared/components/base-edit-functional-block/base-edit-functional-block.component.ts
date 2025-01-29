import { Component, Input, OnInit } from '@angular/core';
import { UntypedFormGroup, UntypedFormBuilder, Validators } from '@angular/forms';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { noWhitespaceValidator } from '@app/core/utils/validator.utils';
import { FunctionalBlockPojoPrototype } from '@innowake/mining-api-angular-client';
import { FunctionalBlockControllerService } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';

export const RESULT_EDITED = 'result_edited';

@Component({
  selector: 'app-base-edit-functional-block',
  templateUrl: './base-edit-functional-block.component.html'
})
export class BaseEditFunctionalBlockComponent implements OnInit {

  @Input() projectId: number;
  formGroup: UntypedFormGroup;
  isVisible = true;
  isSaveLoading = false;
  isGenerating = false;
  isGenerationSuccessful = false;

  genAiTranslateActive = false;

  generateModuleDescriptions = false;

  constructor(protected fb: UntypedFormBuilder,
    protected modalRef: NzModalRef,
    protected functionalBlockController: FunctionalBlockControllerService,
    protected translateService: TranslateService,
    protected messageService: NzMessageService,
    protected featureToggleService: FeatureToggleService) {
  }

  ngOnInit(): void {
    this.featureToggleService.isActive('generativeAiTranslations').subscribe((isActive: boolean) => {
      this.genAiTranslateActive = isActive;
    });
    this.modalRef.afterClose?.subscribe(() => {
      this.isVisible = false;
    });
    this.formGroup = this.fb.group({
      name: ['', [Validators.required, noWhitespaceValidator]],
      description: ['']
    });
  }

  /**
   * Handles the save action when a modal form is submitted.
   * Destroys the modal and passes the form data to the caller.
   */
  onSave(): void {
  }

  canSave(): boolean {
    return this.formGroup.valid && this.formGroup.dirty;
  }

  /**
   * Handles the cancel action for the modal.
   * Closes the modal and destroys it.
   */
  handleCancel(): void {
    this.isVisible = false;
    this.modalRef?.destroy();
  }

  successMessage(): string {
    return this.translateService.instant('functionalBlock.functionalBlockEditSuccess');
  }

  errorMessage(): string {
    return this.translateService.instant('functionalBlock.functionalBlockEditError');
  }

  showGenerate(): boolean {
    return false;
  }

  generateDescription(): void {
  }

  getTooltipText(): string {
    return this.translateService.instant('functionalBlock.generateTooltipFunctional');
  }

  showGenerateModuleDescriptionsCheckbox(): boolean {
    return false;
  }

  protected saveUpdate(functionalBlock: { uid: string; name: string; description: string }): void {
    this.isSaveLoading = true;
    const requestBodyFunctionalGroup: FunctionalBlockPojoPrototype =
    {
      name: this.formGroup.value.name,
      description: this.formGroup.value.description
    };
    this.functionalBlockController.updateFunctionalBlock(this.projectId, functionalBlock.uid, requestBodyFunctionalGroup).subscribe(() => {
      this.messageService.create('success', this.successMessage());
      this.isSaveLoading = false;
      this.modalRef?.destroy(RESULT_EDITED);
    }, () => {
      this.messageService.create('error', this.errorMessage());
      this.isSaveLoading = false;
      this.modalRef?.destroy();
    });
  }

}
