import { HttpErrorResponse } from '@angular/common/http';
import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { AbstractControl, ValidationErrors, Validators } from '@angular/forms';
import { Params } from '@angular/router';
import { Logger } from '@app/core';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { sortNaturally } from '@app/core/utils/sort.util';
import { AllowedTableActions, LinkType, MiningTableAction } from '@app/shared/components/mining-table/mining-table-action.interface';
import {
  DEFAULT_NUMBER_OF_ROWS,
  FieldTypeEnum,
  FilterType,
  MiningTableConfig,
  ViewMode
} from '@app/shared/components/mining-table/mining-table-config.interface';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { NzTableFilterValue } from 'ng-zorro-antd/table';
import { forkJoin, Observable, Subject, Subscription } from 'rxjs';
import { TaxonomiesModalComponent } from './taxonomies-modal/taxonomies-modal.component';
import { TaxonomyModalService } from './taxonomy-modal.service';
import { WarningCancel } from './taxonomy-reporting.model';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { DataPointControllerService, JobInformation, MiningDataPointDefinitionWithPath, ProjectRole,
  TaxonomyControllerService, TaxonomyPojo, TaxonomyPojoPrototype, TaxonomyTypeControllerService,
  TaxonomyTypePojo, TaxonomyTypePojoPrototype } from '@innowake/mining-api-angular-client';

const log = new Logger('TaxonomyConfigurationComponent');

@Component({
  selector: 'mn-taxonomy-configuration',
  templateUrl: './taxonomy-configuration.component.html'
})

export class TaxonomyConfigurationComponent implements OnInit, OnDestroy {
  @Input() clientProjectData: ClientProjectRelationship;
  projectId: number;
  createModal: NzModalRef;
  modalCloseIcon = true;
  taxonomyImportLink: string;
  dataChangeEvent: Subject<MiningTableAction> = new Subject<MiningTableAction>();
  tableConfig: MiningTableConfig;
  taxonomyData: Array<{ [key: string]: any }> = [];
  warningSubscription: Subscription;
  confirmModal: NzModalRef;
  listOfTaxonomies: string[];
  forbiddenNames = ['db access', 'file access', 'program type'];
  defaultColumns: MiningDataPointDefinitionWithPath[] = [];
  taxonomyCategories: { [key: string]: any };
  dataPointsList: MiningDataPointDefinitionWithPath[];
  preFilters: Array<{ key: string, value: NzTableFilterValue }> = [];

  constructor(
    private modalService: NzModalService,
    private translateService: TranslateService,
    private taxonomyModalService: TaxonomyModalService,
    private taxonomyControllerService: TaxonomyControllerService,
    private taxonomyTypeControllerService: TaxonomyTypeControllerService,
    private authorizationService: KeycloakAuthorizationService,
    private messageService: NzMessageService,
    private notification: NzNotificationService,
    private graphQlControllerService: GraphQlControllerService,
    private jobManager: JobManagerService,
    public parametersService: CustomizableTableParametersService,
    public userCustomizableTable: CustomizableTableColumnService,
    public dataPointControllerService: DataPointControllerService
  ) { }

  ngOnInit(): void {
    this.projectId = this.clientProjectData.getProjectId();
    this.getDataPointsForModule();
  }

  /**
   * Change the name of method as per Taxonomies
   * Opens a modal for Client creation
   */
  onOpenImportAssignmentsModal(): void {
    this.createModal = this.modalService.create<TaxonomiesModalComponent>({
      nzTitle: this.translateService.instant('taxonomyReportingComponent.importAssignments'),
      nzClosable: this.modalCloseIcon,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzWrapClassName: 'vertical-center-modal',
      nzContent: TaxonomiesModalComponent,
      nzAfterClose: this.taxonomyModalService.taxonomyModalEventEmitter,
    });
    const instance = this.createModal.getContentComponent();
    instance.downloadTemplate = this.downloadTaxonomiesList.bind(this);
  }

  /*
   * method to download taxonomies list
   */
  downloadTaxonomiesList(): void {
    this.jobManager.invokeExportJob('taxonomy-assignments', {}, this.projectId);
  }

  /**
   * Callback method that checks what action is selected from the Mining table and perform actions accordingly.
   * @param record data of the option selected.
   */
  optionSelected(record: MiningTableOptionSelected): void {
    switch (record.optionValue) {
      case 'reloadData':
        this.setTableData([record?.data]);
        break;
      case 'addTaxonomyType':
        this.addTaxonomy(record, 'taxonomyType');
        break;

      case 'addTaxonomyTerm':
        this.addTaxonomy(record, 'taxonomyTerm');
        break;

      case 'deleteTaxonomy':
        this.deleteTaxonomyBasedOnType(record);
        break;
    }
  }

  /**
   * method to delete term or taxonomy
   * @param data taxonomy data that needs to be deleted.
   */
  deleteTaxonomy(data: MiningTableOptionSelected): void {
    this.confirmModal.updateConfig({
      nzOkLoading: true,
      nzCancelLoading: true,
      nzClosable: false
    });
    let request: Observable<any>;
    if (data.data.type !== 'taxonomyTerm') {
      const deleteTaxonomyCategory: any[] = [];
      if (data.data.level === 0) {
        data.data.children.forEach((taxonomyChildren: any) => {
          deleteTaxonomyCategory.push(this.taxonomyTypeControllerService.deleteTaxonomyType(this.projectId, taxonomyChildren.name as string));
        });
        request = forkJoin(deleteTaxonomyCategory as any);
        this.deleteTaxonomyTypeTerm(request);
      } else {
        request = this.taxonomyTypeControllerService.deleteTaxonomyType(this.projectId, data.data.name as string);
        this.deleteTaxonomyTypeTerm(request);
      }
    } else {
      this.taxonomyControllerService.findAllTaxonomies(this.projectId).subscribe((resp: TaxonomyPojo[]) => {
        const record = resp.find((taxonomy: TaxonomyPojo) => taxonomy.name === data.data.name);
        request = this.taxonomyControllerService.deleteTaxonomy(this.projectId, record?.id);
        this.deleteTaxonomyTypeTerm(request, data.data.name as string);
      });
    }
  }

  /**
   * Callback method to handle the form submission for the inline edited fields.
   * @param data data associated with the field needs to be updated.
   */
  updateTaxonomy(data: Record<any, any>): void {
    const emptyValue = this.isEmptyValue(data);
    if (emptyValue) {
      this.displayErrorMessage(data);
      return;
    }
    const messageId = this.messageService.loading(
      `${this.translateService.instant('taxonomyReportingComponent.savingTaxonomy')}`,
      { nzDuration: 0, }).messageId;
    let request: Observable<TaxonomyPojo | TaxonomyTypePojo | Array<(TaxonomyPojo | TaxonomyTypePojo)>>;
    if (data.isNewRecord) {
      if (data.type === 'taxonomyType') {
        const category = data.parent.id;
        let categoryId = -1;
        for (const cat of this.taxonomyCategories.taxonomyCategories.categories) {
          if (cat.name === category) {
            categoryId = cat.id;
            break;
          }
        }

        const record: TaxonomyTypePojoPrototype = { name: data.newValue, project: this.projectId, categoryId };
        if (Array.isArray(data.newValue)) {
          const createTaxonomyType: Array<Observable<TaxonomyPojo | TaxonomyTypePojo>> = [];
          data.newValue.forEach((records: any) => {
            const record: TaxonomyTypePojoPrototype = { name: records, project: this.projectId, categoryId  };
            createTaxonomyType.push(this.taxonomyTypeControllerService.createTaxonomyType(this.projectId, record));
          });
          request = forkJoin(createTaxonomyType);
        } else {
          record.name = data.newValue.trim();
          request = this.taxonomyTypeControllerService.createTaxonomyType(this.projectId, record);
        }
        this.handleSavingTaxonomy(request, messageId, []);
      } else {
        /* we should use the uids here or search this.taxonomyCategories instead of doing another find */
        this.taxonomyTypeControllerService.findAllTaxonomyTypes(this.projectId).subscribe((resp: TaxonomyTypePojo[]) => {
          const type = resp.find((type: TaxonomyTypePojo) => type.name === data.parent.name).id;
          const record: TaxonomyPojoPrototype = { name: data.newValue, project: this.projectId, type };
          if (Array.isArray(data.newValue)) {
            const createTaxonomy: Array<Observable<TaxonomyPojo | TaxonomyTypePojo>> = [];
            data.newValue.forEach((element: any) => {
              const records: TaxonomyPojoPrototype = { name: element, project: this.projectId, type };
              createTaxonomy.push(this.taxonomyControllerService.createTaxonomy(this.projectId, records));
            });
            request = forkJoin(createTaxonomy);
          } else {
            record.name = data.newValue.trim();
            request = this.taxonomyControllerService.createTaxonomy(this.projectId, record);
          }
          const parentId: string[] = [data.parent.id];
          this.handleSavingTaxonomy(request, messageId, parentId);
        });
      }
    } else {
      data.newValue = data.newValue.trim();
      if (data.type === 'taxonomyType') {
        this.taxonomyTypeControllerService.findAllTaxonomyTypes(this.projectId).subscribe((resp: TaxonomyTypePojoPrototype[]) => {
          const record = resp.find((type: TaxonomyTypePojoPrototype) => type.name === data.actualValue);
          record.name = data.newValue;
          request = this.taxonomyTypeControllerService.updateTaxonomyType(this.projectId, record);
          let expandedIds: string[] = [];
          if (data?.expand) {
            expandedIds = [`${data.parent.name}__${data.newValue}`];
          }
          this.handleSavingTaxonomy(request, messageId, expandedIds);
        });
      } else {
        this.taxonomyControllerService.findAllTaxonomies(this.projectId).subscribe((resp: TaxonomyPojo[]) => {
          const record = resp.find((taxonomy: TaxonomyPojo) => taxonomy.name === data.actualValue);
          const proto: TaxonomyPojoPrototype = {
            name: data.newValue
          };
          request = this.taxonomyControllerService.updateTaxonomy(this.projectId, record.uid, proto);
          const parentId: string[] = [data.parent.id];
          this.handleSavingTaxonomy(request, messageId, parentId);
        });
      }
    }
  }

  /**
   * Callback method to handle the canceling of form submission
   * @param data data associated with the field.
   */
  cancelTaxonomyForm(data: Record<any, any>): void {
    this.dataChangeEvent.next({ action: AllowedTableActions.TOGGLE_ACTIONS, data: false });
    if (data?.isNewRecord) {
      this.optionSelected({ optionValue: 'reloadData', data: [data.parent?.id] });
    }
  }

  /**
   * Callback method to handle starting of editing process
   * @param data data associated with the field.
   */
  editingStarted(data: Record<any, any>): void {
    this.dataChangeEvent.next({ action: AllowedTableActions.RESTRICT_EDITING, data });
    this.listOfTaxonomies = [];
    data.parent.children.forEach((taxonomy: { [key: string]: string }) => {
      if (data.id !== taxonomy.id) {
        this.listOfTaxonomies.push(taxonomy.name.toLowerCase());
      }
    });
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.warningSubscription.unsubscribe();
  }

  private setTableData(expandedIds?: any[], isTaxonomyUpdated?: boolean): void {
    const requestQuery = {
      'query': `{ taxonomyCategories(projectId:${this.projectId})
      { categories { id distinctAssignments name  types { id name distinctAssignments terms { id assignments name } } } } }`};
    this.graphQlControllerService.graphQl(requestQuery, this.projectId).subscribe((response: { [key: string]: any }) => {
      if (response.data) {
        this.taxonomyCategories = response.data;
        const categoryTypeTaxonomies: { [key: string]: any } = {};
        response.data.taxonomyCategories.categories.forEach((res: any) => {
          if (res.types.length) {
            res.types.forEach((type: any) => {
              const key = res.name + '__' + type.name; /* we should use the uids here! */
              if (type.terms.length) {
                type.terms.forEach((term: any) => {
                  const data = {
                    name: term.name,
                    id: key + '__' + term.name, /* we should use the uids here! */
                    moduleCount: term.assignments,
                    type: 'taxonomyTerm',
                    isEditable: this.isEditable(String(res.name)),
                    typeName: type.name,
                    showCountAsText: term.assignments > 0 ? false : true,
                    uid: term.uid
                  };
                  if (categoryTypeTaxonomies[key] && categoryTypeTaxonomies[key].length) {
                    categoryTypeTaxonomies[key].push(data);
                  } else {
                    categoryTypeTaxonomies[key] = [data];
                  }
                });
              } else {
                categoryTypeTaxonomies[key] = [];
              }
            });
          } else {
            categoryTypeTaxonomies[res.name] = [];
          }
        });
        const categories = this.getObjectFromResponse(response.data.taxonomyCategories.categories);
        this.taxonomyData = [];
        /* Creates the tree table data. */
        for (const [key, value] of Object.entries(categoryTypeTaxonomies)) {
          const categoryType = key.split('__');
          const category = categoryType[0];
          const type = categoryType[1];
          const categoryIndex = this.taxonomyData.findIndex((item: any) => item.name === category);
          const isEditable = this.isEditable(category);
          const removeActions = isEditable ? [] : ['addTaxonomyTerm' , 'deleteTaxonomy'];
          const typeIndex = response.data.taxonomyCategories.categories.findIndex((x: any) => x.name === category);
          const types = this.getObjectFromResponse(response.data.taxonomyCategories.categories[typeIndex].types);
          let expand = false;
          if (expandedIds) {
            expand = expandedIds.includes(key);
          }
          const childValues = sortNaturally(value as Array<{ [key: string]: any }>, 'name');
            childValues.forEach((item: { [key: string]: any }) => {
              const parts = item.id.split('__');
              const removeActions = parts[0] === 'Technical Taxonomies' ? ['addTaxonomyTerm', 'deleteTaxonomy'] : [];
              item.removeActions = removeActions;
            });
          const checkChildValues = (childValues?.length > 0);
          const child = {
            name: type, id: key, moduleCount: types[type],
            ...checkChildValues && { children: childValues }, type: 'taxonomyType', showCountAsText: types[type] > 0 ? false : true,
            isEditable, removeActions, expand
          };
          const children = child.name ? sortNaturally([child], 'name') : [];
          const checkChildren = children.length ? true : false;
          if (categoryIndex === -1) {
            const removeActions = this.isEditable(category) ? [] : ['addTaxonomyType', 'deleteTaxonomy'];
            this.taxonomyData.push({
              name: category,
              id: category,
              moduleCount: categories[category],
              ...checkChildren && { children },
              expand: child.name ? true : false,
              isEditable: false,
              showCountAsText: true,
              removeActions
            });
          } else {
            this.taxonomyData[categoryIndex].children?.push(children[0]);
          }
        }
        this.taxonomyData = sortNaturally(this.taxonomyData, 'name');
      }
      if (isTaxonomyUpdated) {
        this.getDataPointsForModule();
      }
      this.tableConfig.loading = false;
    }, (error: HttpErrorResponse) => {
      this.tableConfig.loading = false;
      log.error(error.message);
    });
  }

  private getObjectFromResponse(response: any): any {
    return response.reduce((prevItem: any, item: any) => (
      { ...prevItem, [item['name']]: item['distinctAssignments'] }
    ), {});
  }

  private isEditable(category: string): boolean {
    return category === 'Technical Taxonomies' ? false : true;
  }

  private showErrorMsg(errorTitle: string, messageId: string = null): void {
    if (messageId) {
      this.messageService.remove(messageId);
    }
    const errorBody: string = this.translateService.instant('contactSupport');
    this.notification.error(
      errorTitle,
      errorBody,
      { nzDuration: 0 }
    );
  }

  private deleteTaxonomyTypeTerm(request: Observable<any>, term?: string) {
    request.subscribe((jobProgressId) => {
      if (jobProgressId) {
        this.jobManager.register({ jobId: jobProgressId as unknown as string })?.status$.subscribe((status: JobInformation.StatusEnum) => {
          if (status === JobInformation.StatusEnum.SUCCESS) {
            this.setTableData();
          }
        });
      } else {
        this.setTableData();
        const taxonomyLabel = this.translateService.instant('taxonomy');
        const msg: string = this.translateService.instant('taxonomyReportingComponent.deleteTaxonomyTermSuccess', {term: term ? term: taxonomyLabel});
        this.messageService.success(msg);
      }
    });
  }

  private handleSavingTaxonomy(
    request: Observable<TaxonomyPojo | TaxonomyTypePojo | Array<(TaxonomyPojo | TaxonomyTypePojo)>>,
    messageId: string,
    expandedIds: string[]
  ) {
    request.subscribe({
      next: () => {
        this.setTableData(expandedIds, true);
        this.messageService.remove(messageId);
        const msg: string = this.translateService.instant('taxonomyReportingComponent.savedTaxonomy');
        this.messageService.success(msg);
      },
      error: () => {
        const title: string = this.translateService.instant('taxonomyReportingComponent.savingTaxonomyError');
        this.showErrorMsg(title, messageId);
      }
    });
  }

  private addTaxonomy(record: MiningTableOptionSelected, type: string) {
    const data = {
      name: 'New Term',
      id: 'newTaxonomyTerm',
      moduleCount: 0,
      isNewRecord: true,
      showCountAsText: true,
      type
    };
    if (type === 'taxonomyType') {
      data.name = 'New Taxonomy Type';
      data.id = 'newTaxonomyType';
      data['children'] = [];
    }
    if (record.data.children) {
      record.data.children.unshift(data);
    } else {
      record.data['children'] = [];
      record.data.children.unshift(data);
    }
    this.dataChangeEvent.next({ action: AllowedTableActions.TOGGLE_ACTIONS, data: true });
    this.dataChangeEvent.next({ action: AllowedTableActions.ADD_CHILD, data: record.data });
  }

  /**
   * checkTaxonomyName is used to check the duplicate value and forbidden values.
   * @param type type of validation to consider the duplicated values
   * @returns key value pair with key as true if conditions are met.
   */
  private checkTaxonomyName(type: string) {
    return (control: AbstractControl): ValidationErrors | null => {
      const listOfValues: string[] = type === 'duplicate' ? [...new Set(this.listOfTaxonomies)] : this.forbiddenNames;
      let value: string;
      let result: boolean;
      if (Array.isArray(control.value)) {
        control.value.forEach((element: string) => {
          if (listOfValues.includes(element)) {
            result = true;
          }
        });
      } else {
        value = control.value.toLowerCase();
        result = listOfValues.includes(value);
      }
      return result ? { [type]: true } : null;
    };
  }

  private fetchURLParamsFromData(taxonomyData: { [key: string]: any }): Params {
    if (taxonomyData.moduleCount > 0) {
      this.defaultColumns = [];
      this.getDefaultTableConfig();
      const queryParams: { [key: string]: any } = {};
      queryParams.columns = [];
      queryParams.filter = [];
      this.findPreFilters(taxonomyData);
      if (this.defaultColumns.length) {
        this.defaultColumns.forEach((value: MiningDataPointDefinitionWithPath) => {
          queryParams.columns.push(value.id);
        });
      }
      queryParams.filter = JSON.stringify(this.preFilters);
      if (taxonomyData) {
        return queryParams;
      }
    }
  }

  private addSelectedColumnToDefaultColumns(taxonomyName: string): void {
    const dataPoint = this.dataPointsList.find(datapointelement => datapointelement.displayName === taxonomyName);
    if (dataPoint) {
      this.defaultColumns.push(dataPoint);
    }
  }

  private findPreFilters(filterDetails: { [key: string]: any }): void {
    this.preFilters = [];
    if (filterDetails.type === 'taxonomyType') {
      this.filterByTaxonomyType(filterDetails);
    } else if (filterDetails.type === 'taxonomyTerm') {
      if (filterDetails.parent.type === 'taxonomyType') {
        this.addSelectedColumnToDefaultColumns(filterDetails.parent.name as string);
      }
      const filter = { key: 'taxonomy.' + this.toCamelCase(filterDetails.typeName as string), value: [filterDetails.name] };
      this.preFilters.push(filter);
    } else {
      if (Array.isArray(filterDetails.children)) {
        for (const children of filterDetails.children) {
          if (children.type === 'taxonomyType') {
            this.filterByTaxonomyType(children as { [key: string]: any });
          }
        }
      }
    }
  }

  private filterByTaxonomyType(filterDetails: { [key: string]: any }): void {
    this.addSelectedColumnToDefaultColumns(filterDetails.name as string);
    const filterValues: string[] = [];
    if (filterDetails.hasOwnProperty('children')) {
      filterDetails?.children.forEach((taxonomy: { [key: string]: any }) => {
        if (taxonomy.type === 'taxonomyTerm') {
          filterValues.push(taxonomy.name as string);
        }
      });
      const filter = { key: 'taxonomy.' + this.toCamelCase(filterDetails.name as string), value: filterValues };
      this.preFilters.push(filter);
    }
  }

  private toCamelCase(str: string): string {
    const wordsInStr = str.split(' ');
    if (wordsInStr.length !== 1) {
      str = str.toLowerCase(); // in case of single word, case would be retained.
    }
    return str.replace(/(?:^\w|[A-Z]|\b\w)/g, (word, index) =>
      index === 0 || index === 1 ? word.toLowerCase() : word.toUpperCase()
    ).replace(/\s+/g, '');
  }

  private navigateToDetails(taxonomyData: { [key: string]: any }): string {
    if (taxonomyData.moduleCount > 0) {
      return RouteBuilder.buildProjectRoute(this.projectId, 'modules');
    }
  }

  private isEmptyValue(data: Record<any, any>): boolean {
    return Array.isArray(data.newValue) ? data.newValue.some((record: any) => record.trim() === '') : data.newValue.trim() === '';
  }

  private displayErrorMessage(data: Record<any, any>): void {
    const msg: string = data.type === 'taxonomyType' ? this.translateService.instant('taxonomyReportingComponent.savingTaxonomyError') :
    this.translateService.instant('taxonomyReportingComponent.savingTaxonomyTermError');
    this.messageService.error(msg);
  }

  private getDefaultTableConfig(): void {
    this.userCustomizableTable.selectedColumns = [];
    this.defaultColumns = [];
    this.dataPointsList.forEach((datPoint) => {
      if (this.userCustomizableTable.checkSelectedColumns(datPoint, Usages.MODULETABLE)) {
        this.defaultColumns.push(datPoint);
      }
    });
  }

  private getDataPointsForModule() {
    this.dataPointControllerService.getDataPointsForType(this.projectId, TypeName.PAGEMODULE, [Usages.MODULETABLE]).subscribe(
      (dataPointDefinitions: MiningDataPointDefinitionWithPath[]) => {
        this.dataPointsList = dataPointDefinitions;
        this.setTableConfigAndValue();
      }
    );
  }

  private setTableConfigAndValue() {
    this.tableConfig = {
      columnMap: {
        name: {
          field: 'name', header: 'taxonomy', filterProperties: { filterType: FilterType.freeText }, isEditable: true, widthColumn: '60%'
        },
        moduleCount: {
          field: 'moduleCount', header: 'assignedModules', sortFn: false, fieldType: FieldTypeEnum.NUMBER,
          options: [
            {
              value: 'moduleCount',
              icon: 'info-circle',
              title: 'moduleCountTooltip',
              styleClass: 'ant-helper-secondary-text',
              disableItem: (): boolean => false
            }
          ],
          columnAction: {
            type: LinkType.HYPERLINK,
            resolveURL: (data: { [key: string]: any }) => this.navigateToDetails(data),
            resolveURLParams: (data: { [key: string]: any }) => this.fetchURLParamsFromData(data)
          },
          displayAs: ViewMode.LINK,
        },
      },
      paginator: true,
      rows: DEFAULT_NUMBER_OF_ROWS,
      actionsWidth: '210px',
      disableAllActions: false
    };
    this.tableConfig.loading = true;
    this.taxonomyImportLink = this.taxonomyModalService.taxonomyImportLink;
    this.taxonomyModalService.setProjectId(this.projectId);
    this.setTableData();
    this.warningSubscription = this.taxonomyModalService.cancelWarningSubject.subscribe((resp: string) => {
      if (resp === WarningCancel.TAXONOMY_IMPORTED) {
        this.setTableData();
      }
    });
    if (this.clientProjectData && this.authorizationService.hasUserRole(this.clientProjectData, ProjectRole.UserRoleEnum.MANAGER)) {
      // Add edit options for name column
      this.tableConfig.columnMap.name.editOption = {
        enableEditing: false,
        validations: [Validators.required, this.checkTaxonomyName('duplicate'), this.checkTaxonomyName('forbiddenName')],
        validationMessages: {
          required: 'validationMessages.required',
          duplicate: 'validationMessages.duplicate',
          forbiddenName: 'validationMessages.forbiddenName'
        },
        onSubmit: (data: Record<any, any>) => this.updateTaxonomy(data),
        onCancel: (data: Record<any, any>) => this.cancelTaxonomyForm(data),
        onEditingStart: (data: Record<any, any>) => this.editingStarted(data)
      };

      // Add actions for Add/Delete
      this.tableConfig.actions = [
        [
          { label: 'taxonomy', value: 'addTaxonomyType', icon: 'plus', prop: { type: 'primary', size: 'default' } },
        ],
        [
          { label: 'term', value: 'addTaxonomyTerm', icon: 'plus', prop: { type: 'default', size: 'default' } },
          { value: 'deleteTaxonomy', icon: 'delete', prop: { isDelete: true, type: 'text', size: 'default' } }
        ],
        [
          { type: LinkType.EMPTY },
          { value: 'deleteTaxonomy', icon: 'delete', prop: { isDelete: true, type: 'text', size: 'default' } }
        ]
      ];
    }
  }

  private deleteTaxonomyBasedOnType(record: MiningTableOptionSelected) {
    let deleteTaxonomyTitle = '';
    switch (record.data.type) {
      case 'taxonomyTerm':
        deleteTaxonomyTitle = this.translateService.instant('deleteTaxonomy.confirmModalTitleForTerm', {
          term: record.data.name
        });
        break;
      case 'taxonomyType':
        deleteTaxonomyTitle = this.translateService.instant('deleteTaxonomy.confirmModalTitleForType', {
          term: record.data.name
        });
        break;
      default:
        deleteTaxonomyTitle = this.translateService.instant('deleteTaxonomy.confirmModalTitleForTaxonomy', {
          term: record.data.name
        });
    }
    let deleteConfirmBoxBody: string =
      this.translateService.instant('deleteTaxonomy.assignModulesTaxonomyModalTxt', { moduleCount: record.data.moduleCount });
    if (record.data.moduleCount === 1) {
      deleteConfirmBoxBody = this.translateService.instant('deleteTaxonomy.assignModuleTaxonomyModalTxt', { moduleCount: record.data.moduleCount });
    } else if (record.data.moduleCount === 0) {
      deleteConfirmBoxBody = '';
    }
    this.confirmModal = this.modalService.confirm({
      nzTitle: deleteTaxonomyTitle,
      nzOkText: this.translateService.instant('btnLabel.delete'),
      nzContent: deleteConfirmBoxBody,
      nzOkType: 'primary',
      nzOkDanger: true,
      nzOkLoading: false,
      nzOnOk: () => this.deleteTaxonomy(record),
      nzCancelText: this.translateService.instant('btnLabel.cancel')
    });
  }

}
