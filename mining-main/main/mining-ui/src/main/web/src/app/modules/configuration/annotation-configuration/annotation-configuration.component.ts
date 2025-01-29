import { Component, Input, OnInit } from '@angular/core';
import { Logger } from '@app/core';
import {
  DEFAULT_NUMBER_OF_ROWS, FilterType,
  MiningTableConfig
} from '@app/shared/components/mining-table/mining-table-config.interface';
import { sortNaturally } from '@app/core/utils/sort.util';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { Subject } from 'rxjs';
import { AllowedTableActions, LinkType, MiningTableAction } from '@app/shared/components/mining-table/mining-table-action.interface';
import { AbstractControl, ValidationErrors, Validators } from '@angular/forms';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { TranslateService } from '@ngx-translate/core';
import { HttpErrorResponse } from '@angular/common/http';
import { NzMessageService } from 'ng-zorro-antd/message';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { AnnotationCategory, AnnotationCategoryControllerService, AnnotationPojo, ProjectRole } from '@innowake/mining-api-angular-client';

const logger = new Logger('CustomPropertiesComponent');

@Component({
  selector: 'mn-annotation-configuration',
  templateUrl: './annotation-configuration.component.html'
})

export class AnnotationConfigurationComponent implements OnInit {
  @Input() projectId: number;
  @Input() clientProjectData: ClientProjectRelationship;
  categoryConfig: MiningTableConfig = {
    columnMap: {
      type: {
        field: 'type',
        header: 'type',
        filterProperties: {
          filterType: FilterType.freeText
        },
        isEditable: true
      },
    },
    actionsWidth: '150px',
    paginator: true,
    rows: DEFAULT_NUMBER_OF_ROWS
  };
  categoryList: Array<{ [key: string]: any }> = [];
  confirmModal: NzModalRef;
  id: number;
  dataChangeEvent: Subject<MiningTableAction> = new Subject<MiningTableAction>();
  listOfAnnotationCategories: string [] = [];
  configureAnnotationsNotToBeRemoved = [
    'Close',
    'Read',
    'Declare',
    'Write',
    'Business Rule',
    'Error Processing Rule',
    'Field Computation Rule',
    'Process Rule',
    'Technical Rule',
    'Validation Rule',
  ];

  constructor(
    private annotationCategoryController: AnnotationCategoryControllerService,
    private modalService: NzModalService,
    private messageService: NzMessageService,
    private authorizationService: KeycloakAuthorizationService,
    private translateService: TranslateService) { }

  ngOnInit(): void {
    this.categoryConfig.loading = true;
    const canAddCategory = this.authorizationService.hasUserRole(this.clientProjectData, ProjectRole.UserRoleEnum.MANAGER);
    this.createAnnotationTable();
    if (this.categoryList) {
      if (canAddCategory) {
        this.categoryConfig.columnMap.type.editOption = {
          enableEditing: false,
          validations: [Validators.required, this.checkDuplicateAnnotationName()],
          validationMessages: { required: 'validationMessages.required', duplicate: 'validationMessages.duplicate' },
          onSubmit: (data: Record<any, any>) => this.updateAnnotationCategory(data),
          onCancel: (data: Record<any, any>) => this.cancelAnnotationForm(data),
          onEditingStart: (data: Record<any, any>) => this.editingStarted(data),
        };
      }
      this.categoryConfig.actions = [
        [
          { label: 'category', value: 'addCategory', icon: 'plus', prop: { type: 'default', size: 'default' }, disabled: ! canAddCategory },
        ],
        [
          { type: LinkType.EMPTY },
          { value: 'deleteCategory', icon: 'delete', prop: { isDelete: true, type: 'text', size: 'default' } }
        ]
      ];
    }
  }

  /**
   * Callback method to handle starting of editing process
   * @param data data associated with the field.
   */
  editingStarted(data: Record<any, any>): void {
    this.listOfAnnotationCategories = [];
    this.id = data.rid;
    data.parent.children.forEach((element: { [key: string]: string }) => {
      if (element.id !== data.id) {
        this.listOfAnnotationCategories.push(element.type.toLowerCase());
      }
    });
    this.dataChangeEvent.next({ action: AllowedTableActions.RESTRICT_EDITING, data });
  }

  /**
   * Callback method that checks what action is selected from the Mining table and perform actions accordingly.
   * @param record data of the option selected.
   */
  optionSelected(record: MiningTableOptionSelected): void {
    switch (record.optionValue) {
      case 'reload':
        this.createAnnotationTable(record);
        break;
      case 'addCategory':
        const data = {
          id: 'newAnnotationType',
          isNewRecord: true,
          type: 'New Category'
        };
        if ( ! record.data.children) {
          record.data.children = [];
        }
        record.data.children.unshift(data);
        this.dataChangeEvent.next({ action: AllowedTableActions.ADD_CHILD, data: record.data });
        break;
      case 'deleteCategory':
        const deleteTaxonomyTitle = this.translateService.instant('deleteAnnotation.confirmModalTitleForTerm', {
          term: record.data?.type
        });
        const deleteConfirmBoxBody: string =
          this.translateService.instant('deleteAnnotation.assignModulesTaxonomyModalTxt', { moduleCount: record.data.parent?.type });
        this.confirmModal = this.modalService.confirm({
          nzTitle: deleteTaxonomyTitle,
          nzOkText: this.translateService.instant('btnLabel.delete'),
          nzContent: deleteConfirmBoxBody,
          nzOkType: 'primary',
          nzOkDanger: true,
          nzOkLoading: false,
          nzOnOk: () => this.deleteAnnotationCategory(record),
          nzCancelText: this.translateService.instant('btnLabel.cancel')
        });
    }
  }
  /**
   * method to delete the record from the annotation categories table.
   * @param data data associated with the field needs to be deleted.
   */
  deleteAnnotationCategory(data: MiningTableOptionSelected): void {
    this.confirmModal.updateConfig({
      nzOkLoading: true,
      nzCancelLoading: true,
      nzClosable: false
    });
    this.annotationCategoryController.deleteAnnotationCategory(this.projectId, data.data.rid as number).subscribe(() => {
      this.optionSelected({ optionValue: 'reload', data: { deleted: true, term: data.data.type } });
    });
  }

  /**
   * Callback method to handle the form submission for the inline edited fields.
   * @param updateAnnotationDetails data associated with the field needs to be updated.
   */
  updateAnnotationCategory(updateAnnotationDetails: Record<any, any>): void {
    if (updateAnnotationDetails.isNewRecord) {
      const record: AnnotationCategory = { name: updateAnnotationDetails.newValue, projectId: this.projectId, types: [updateAnnotationDetails.parent.type] };
      this.annotationCategoryController.createAnnotationCategory(this.projectId, record).subscribe(() => {
        this.optionSelected({ optionValue: 'reload', data: {} });
        const successContent: string = this.translateService.instant('annotationFields.saveSuccess', { propertyTitle: 'Annotation' });
        this.messageService.success(successContent);
      }, () => {
        this.showErrorMsg(updateAnnotationDetails.newValue as string);
      });
    } else {
      const record: AnnotationCategory = {
        name: updateAnnotationDetails.newValue,
        id: this.id, types: [updateAnnotationDetails.parent.type],
        projectId: this.projectId
      };
      this.annotationCategoryController.updateAnnotationCategory(this.projectId, this.id, record).subscribe(() => {
        this.optionSelected({ optionValue: 'reload', data: {} });
        const successContent: string = this.translateService.instant('annotationFields.saveSuccess',
          { propertyTitle: 'Annotation' });
        this.messageService.success(successContent);
      }, () => {
        this.showErrorMsg(updateAnnotationDetails.newValue as string);
      });
    }
  }

  /**
   * Callback method to handle the canceling of form submission
   * @param cancelAnnotationDetails data associated with the field.
   */
  cancelAnnotationForm(cancelAnnotationDetails: Record<any, any>): void {
    this.dataChangeEvent.next({ action: AllowedTableActions.TOGGLE_ACTIONS, data: false });
    if (cancelAnnotationDetails?.isNewRecord) {
      this.optionSelected({ optionValue: 'reload', data: {} });
    }
  }

  private createAnnotationTable(createAnnotationDetails?: MiningTableOptionSelected): void {
    this.annotationCategoryController.findAllAnnotationCategories(this.projectId).subscribe((categories: AnnotationCategory[]) => {
      sortNaturally(categories, 'name');
      const typePerCategory = {};
      this.categoryList = [];
      categories.forEach((category) => {
        if (category.types?.length) {
          const categoryType = category.types[0];
          if ( ! typePerCategory[categoryType]) {
            typePerCategory[categoryType] = [{ name: category.name, rid: category.id, projectId: category.projectId}];
          } else {
            typePerCategory[categoryType] = [...typePerCategory[categoryType], { name: category.name, rid: category.id, projectId: category.projectId}];
          }
        } else {
          this.categoryList.push({
            type: category.name, id: category.name, recordId: category.recordId, nonExpandableRowIndent: 1
          });
        }
      });
      Object.keys(typePerCategory).forEach((typePerCategoryItem) => {
        const child: Array<{ type: string; id: string, rid: number, isEditable: boolean, removeActions: string[] }> = [];
        typePerCategory[typePerCategoryItem].forEach((type: any) => {
          const checkEditable = (type.projectId !== 0);
          const removeDeleteOption = type.projectId === 0 || this.configureAnnotationsNotToBeRemoved.includes(type.name as string) ? ['deleteCategory'] : [];
          child.push({
            type: type.name,
            id: typePerCategoryItem + '__' + type.name,
            rid: type.rid,
            isEditable: checkEditable, removeActions: removeDeleteOption
          });
        });
        this.categoryList.push({
          type: typePerCategoryItem,
          children: child,
          id: typePerCategoryItem,
          isEditable: false,
          name: typePerCategoryItem
        });
      });
      const types: string[] = Object.keys(AnnotationPojo.TypeEnum).map(type => AnnotationPojo.TypeEnum[type]);
      types.forEach((type) => {
        if (Object.keys(typePerCategory).indexOf(type) === -1) {
          this.categoryList.push({
            type, id: type, nonExpandableRowIndent: 1,
            isEditable: false, name: type
          });
        }
      });
      sortNaturally(this.categoryList, 'type');
      this.categoryConfig.loading = false;
      if (createAnnotationDetails?.data?.deleted) {
        const successFullDelete: string = this.translateService.instant('deleteTaxonomy.successFullDelete', {
          term: createAnnotationDetails.data.term
        });
        this.messageService.success(successFullDelete);
      }
    },
      (error: HttpErrorResponse) => {
        this.categoryConfig.loading = false;
        logger.error(error);
      });
  }

  private showErrorMsg(value: string): void {
    const title: string = this.translateService.instant('annotationFields.errorNotification.titleSave',
      { propertyTitle: value });
    this.messageService.error(title);
  }

  private checkDuplicateAnnotationName() {
    return (control: AbstractControl): ValidationErrors | null => {
      const listOfValues = [...new Set(this.listOfAnnotationCategories)];
      const value: string = control.value.toLowerCase();
      const result = listOfValues.includes(value);
      return result ? { 'duplicate': true } : null;
    };
  }
}
