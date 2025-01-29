import { Component, Input, OnInit } from '@angular/core';
import { UntypedFormBuilder, UntypedFormControl, UntypedFormGroup, ValidationErrors, Validators } from '@angular/forms';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { noWhitespaceValidator } from '@app/core/utils/validator.utils';
import { AnnotationToFunctionalBlockControllerService, FunctionalBlockControllerService,
  FunctionalBlockPojoPrototype } from '@innowake/mining-api-angular-client';
import { AnnotationControllerService } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';

export const RESULT_EDITED = 'group_edited';
export const RESULT_DELETED = 'group_deleted';
export const RESULT_CANCELLED = 'group_cancelled';

@Component({
  selector: 'app-create-edit-functional-group',
  templateUrl: './create-edit-functional-group.component.html'
})
export class CreateEditFunctionalGroupComponent implements OnInit {
  /* Only set when modal is used for editing */
  @Input() annotationGroupToEdit?: any;
  /* For editing this will be set automatically based on the annotation group */
  @Input() annotationIds: number[] = [];
  @Input() projectId: number;
  @Input() listOfFgsByModule: any[] = [];
  createFunctionGroupForm: UntypedFormGroup;
  isVisible = false;
  isSaveLoading = false;
  isDeleteLoading = false;
  isEditModal = false;
  functionalBlock: {
    uid: string; name: string; description: string; childrenDeep: {
      content: Array<{ generatedFrom: { annotationId: number } }>
    }
  };
  genAiTranslateActive = false;
  isGenerating = false;
  isGenerationSuccessful = false;
  generateDescriptions = false;

  constructor(
    private fb: UntypedFormBuilder,
    private modalRef: NzModalRef,
    private annotationFunctionalBlockController: AnnotationToFunctionalBlockControllerService,
    private functionalBlockController: FunctionalBlockControllerService,
    private translateService: TranslateService,
    private messageService: NzMessageService,
    private graphQlControllerService: GraphQlControllerService,
    private featureToggleService: FeatureToggleService,
    private annotationControllerService: AnnotationControllerService) {
  }

  ngOnInit(): void {
    this.featureToggleService.isActive('generativeAiTranslations').subscribe((isActive: boolean) => {
      this.genAiTranslateActive = isActive;
    });
    if (this.annotationGroupToEdit) {
      this.isEditModal = true;
      this.getFuntionalBlock();
    }
    this.createFunctionGroupForm = this.fb.group({
      name: ['', [Validators.required, noWhitespaceValidator, this.checkDuplicateFgName.bind(this)]],
      description: ['']
    });
  }

  /**
   * Handles the save action when a modal form is submitted.
   * Destroys the modal and passes the form data to the caller.
   */
  onSave(): void {
    if (this.isEditModal) {
      this.saveUpdate();
    } else {
      this.saveNew();
    }

  }

  canSave(): boolean {
    return this.createFunctionGroupForm.valid && this.createFunctionGroupForm.dirty;
  }

  /**
   * Handles the cancel action for the modal.
   * Closes the modal and destroys it.
   */
  handleCancel(): void {
    this.isVisible = false;
    const result = {
      state: RESULT_CANCELLED,
      buttonState: true
    };
    this.modalRef?.destroy(result);
  }

  generateDescription(): void {
    if ( ! this.isEditModal) {
      return;
    }
    this.isGenerating = true;
    this.annotationControllerService.generateGroupDescription(this.projectId, this.annotationIds, this.generateDescriptions).subscribe((resp) => {
      this.isGenerating = false;
      this.isGenerationSuccessful = true;
      this.createFunctionGroupForm.get('description').setValue(resp);
      this.createFunctionGroupForm.markAsDirty();
    }, (error) => {
      this.isGenerating = false;
      this.isGenerationSuccessful = false;
      this.messageService.create('error',
        `${this.translateService.instant('functionalBlock.generateAnnotationGroupDescriptionError')}: ` + error.error?.message);
    });
  }

  /**
   * Deletes the functional block.
   * @returns void
   */
  onDeleteFg(): void {
    this.isDeleteLoading = true;
    this.functionalBlockController.deleteAutomatedFunctionalBlock(this.projectId, this.functionalBlock.uid).subscribe((response: string[]) => {
      if (response) {
        this.messageService.create('success',
          `${this.translateService.instant('functionalBlock.functionalBlockDeleteSuccess')}`);
        this.isDeleteLoading = false;
        const result = {
          state: RESULT_DELETED,
          buttonState: false
        };
        this.modalRef?.destroy(result);
      }
    }, () => {
      this.messageService.create('error', `${this.translateService.instant('functionalBlock.functionalBlockDeleteError')}`);
      this.modalRef?.destroy();
    });
  }

  private saveUpdate(): void {
    this.isSaveLoading = true;
    const requestBodyFunctionalGroup: FunctionalBlockPojoPrototype =
    {
      name: this.createFunctionGroupForm.value.name,
      description: this.createFunctionGroupForm.value.description
    };
    this.functionalBlockController.updateFunctionalBlock(this.projectId, this.functionalBlock.uid, requestBodyFunctionalGroup).subscribe(() => {
      this.messageService.create('success',
        `${this.translateService.instant('functionalBlock.functionalBlockEditSuccess')}`);
      this.isSaveLoading = false;
      const result = {
        updatedFunctionalGroup: requestBodyFunctionalGroup,
        state: RESULT_EDITED,
        buttonState: false
      };
      this.modalRef?.destroy(result);
    }, () => {
      this.messageService.create('error', `${this.translateService.instant('functionalBlock.functionalBlockEditError')}`);
      this.modalRef?.destroy();
    });
  }

  private saveNew(): void {
    let listOfCurrentFunctionalUnits: string[] = [];
    this.annotationFunctionalBlockController
      .getFunctionalUnitsForAnnotations(this.projectId, this.annotationIds)
      .subscribe((functionalUnits: { [key: string]: any }) => {
        listOfCurrentFunctionalUnits = Object.values(functionalUnits);
        const requestBodyFunctionalGroup: FunctionalBlockPojoPrototype = {
          name: this.createFunctionGroupForm.value.name,
          description: this.createFunctionGroupForm.value.description,
          flags: { TYPE: ['FUNCTIONAL_GROUP'] },
          children: listOfCurrentFunctionalUnits
        };
        this.functionalBlockController.createFunctionalBlock(this.projectId, requestBodyFunctionalGroup).subscribe(
          (functionalGroupRes: { [key: string]: any }) => {
            this.modalRef?.destroy(functionalGroupRes);
            this.messageService.create(
              'success',
              `${this.translateService.instant('functionalBlock.functionalBlockCreationSuccess')}`
            );
          },
          () => {
            this.messageService.create(
              'error',
              `${this.translateService.instant('functionalBlock.functionalBlockCreationError')}`
            );
            this.modalRef?.destroy();
          }
        );
      });
  }

  private getFuntionalBlock(): void {
    const requestQuery = {
      'query': `{
        functionalBlocks(
          projectId:  ${this.projectId},
          filterObject: { content_uid: { eq: "${this.annotationGroupToEdit}" } }
        ) {
          content {
            uid
            name
            description
            childrenDeep(filterObject:{content_type: { eq: FUNCTIONAL_UNIT }}) {
              content {
                generatedFrom {
                  annotationId
                }
              }
            }
          }
        }
      }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: any) => {
      if (response && response?.data?.functionalBlocks) {
        const functionalBlocksRecords: any[] = response?.data?.functionalBlocks?.content;
        this.functionalBlock = functionalBlocksRecords[0];
        this.createFunctionGroupForm.get('name').setValue(this.functionalBlock.name);
        this.createFunctionGroupForm.get('description').setValue(this.functionalBlock.description);
        this.getAnnotationIdsFromFunctionalBlock();
      }
    });
  }

  private getAnnotationIdsFromFunctionalBlock(): void {
    this.annotationIds = this.functionalBlock.childrenDeep.content
      .filter((child: {generatedFrom: {annotationId: number}}) => child.generatedFrom && child.generatedFrom.annotationId)
      .map((child: {generatedFrom: {annotationId: number}}) => child.generatedFrom.annotationId);
  }

  private checkDuplicateFgName(control: UntypedFormControl): ValidationErrors | null {
    if (this.functionalBlock?.name && this.functionalBlock.name.toLowerCase() === control.value.toLowerCase()) {
      return null;
    }
    const titleIndex = this.listOfFgsByModule.findIndex(existingFg => existingFg.name.toLowerCase() === control.value.toLowerCase());
    return (titleIndex !== -1) ? { 'duplicate': true } : null;
  }

}
