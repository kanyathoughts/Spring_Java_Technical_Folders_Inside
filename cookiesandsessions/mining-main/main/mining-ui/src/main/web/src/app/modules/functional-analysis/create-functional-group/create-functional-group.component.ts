import { Component, Input, OnInit } from '@angular/core';
import { UntypedFormGroup, UntypedFormBuilder, Validators } from '@angular/forms';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { noWhitespaceValidator } from '@app/core/utils/validator.utils';
import { FunctionalBlockControllerService } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';

export const RESULT_EDITED = 'result_edited';

@Component({
  selector: 'app-create-functional-group',
  templateUrl: './create-functional-group.component.html'
})
export class CreateFunctionalGroupComponent implements OnInit {
  @Input() functionalGroupDetails: Array<{ uid: string, type: string, parent: NzTreeNodeOptions}> = [];
  @Input() projectId: number;
  formGroup: UntypedFormGroup;
  isVisible = false;
  isSaveLoading = false;
  isGenerating = false;

  genAiTranslateActive = false;

  constructor(protected fb: UntypedFormBuilder,
    protected modalRef: NzModalRef,
    protected functionalBlockController: FunctionalBlockControllerService,
    protected translateService: TranslateService,
    protected messageService: NzMessageService,
    protected featureToggleService: FeatureToggleService
    ) {
  }

  ngOnInit(): void {
    this.featureToggleService.isActive('generativeAiTranslations').subscribe((isActive: boolean) => {
      this.genAiTranslateActive = isActive;
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
    if (this.functionalGroupDetails.length > 1) {
      const children: string[] = [];
      const types: string[] = [];
      const parents: NzTreeNodeOptions[] = [];
      for (const functionalGroupDetail of this.functionalGroupDetails) {
        if ( ! types.includes(functionalGroupDetail.type)) {
          types.push(functionalGroupDetail.type);
        }
        if ( ! parents.includes(functionalGroupDetail.parent) && functionalGroupDetail?.parent != null) {
          parents.push(functionalGroupDetail.parent);
        }
        if ( ! children.includes(functionalGroupDetail.uid) ) {
          children.push(functionalGroupDetail.uid);
        }
      }
      const name: string = this.formGroup.get('name').value;
      const description: string = this.formGroup.get('description').value;
      const requestBodyFunctionalGroup = {
        name,
        description,
        flags: { TYPE: ['FUNCTIONAL_GROUP'] },
        children
      };
      if (types.length === 1  && new Set(parents?.map(parent => parent.uid)).size === 1) {
        this.createFunctionalGroupAndAddToParentGroup(requestBodyFunctionalGroup, parents, children);
      } else {
        this.createFunctionalGroup(requestBodyFunctionalGroup);
      }
    }
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
    this.modalRef?.destroy({OPERATION: 'CANCEL'});
  }

  private createFunctionalGroupAndAddToParentGroup(requestBodyFunctionalGroup:
    { name: string; description: string; flags: { TYPE: string[]; }; children: string[]; }, parents: NzTreeNodeOptions[], children: string[]) {
    this.functionalBlockController.createFunctionalBlock(this.projectId, requestBodyFunctionalGroup).subscribe((response) => {
      if (response) {
        const parentChildren = parents[0]?.children?.map((child: any) => child.uid) || [];
        const parentFilteredChildren = parentChildren.filter((child: string) => ! children.includes(child));
        parentFilteredChildren.push(response.uid);
        const parentRequest = { children: parentFilteredChildren };
        this.functionalBlockController.updateFunctionalBlock(this.projectId, parents[0].uid as string, parentRequest).subscribe((parentResponse) => {
          if (parentResponse) {
            this.computeFunctionalBlock(new Set([response.uid, parentResponse.uid]), response);
          }
        });
      }
    });
  }

  private createFunctionalGroup(requestBodyFunctionalGroup: { name: string; description: string; flags: { TYPE: string[]; }; children: string[]; }) {
    this.functionalBlockController.createFunctionalBlock(this.projectId, requestBodyFunctionalGroup).subscribe((response) => {
      if (response) {
        this.computeFunctionalBlock(new Set([response.uid]), response);
      }
    });
  }

  private computeFunctionalBlock(UIDs: Set<string>, response: any){
    this.functionalBlockController.computeFunctionalBlock(this.projectId, UIDs).subscribe(() => {
      this.modalRef?.destroy({ OPERATION: 'SUCCESS', uid: response.uid, title: response.name, type: response.flags.TYPE[0]});
    });
  }
}
