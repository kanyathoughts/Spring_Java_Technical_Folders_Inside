import { AfterViewInit, Component, ElementRef, Input, OnInit, ViewChild } from '@angular/core';
import { UntypedFormBuilder, UntypedFormControl, UntypedFormGroup } from '@angular/forms';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { graphQlQuery } from '@app/core/utils/graphql.util';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { LinkType } from '../mining-table/mining-table-action.interface';
import { DEFAULT_NUMBER_OF_ROWS, FieldTypeEnum, MiningTableConfig, ViewMode } from '../mining-table/mining-table-config.interface';
import { MiningTableOptionSelected } from '../mining-table/mining-table-option-selected.interface';
import { Modules, ModuleTableItem, RepeatedAndInvalidModules } from './module-listing.interface';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { convertToTag } from '@app/core/utils/tag-conversion.utils';
import { ListDetail } from '@app/modules/module-details/call-chain-export/call-chain-export.interface';
import { ModuleListingService } from './module-listing.service';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { ModulesGQL } from '@app/graphql/generated/generated';

const repeatedModuleClass = 'module-listing__repeated-module';

@Component({
  selector: 'module-listing',
  templateUrl: './module-listing.component.html',
})
export class ModuleListingComponent implements OnInit, AfterViewInit {

  @ViewChild('tagFieldInput') tagFieldInput: ElementRef;

  // selected module ids or name
  @Input() moduleIds: string[];
  // current project id
  @Input() projectId: number;
  // page to be fetched for the module
  @Input() pageSize = 10;
  @Input() moduleType: string;
  @Input() moduleIdsFromReportingPage: string;
  moduleSearchLoading  = false;
  tableConfig: MiningTableConfig = {
    columnMap: {
      module: {
        field: 'module', header: 'moduleListing.module', displayAs: ViewMode.LINK,
        columnAction: { type: LinkType.HYPERLINK, resolveURL: (data: any) => this.navigateToDetails(data.id as number) },
      },
      technology: { field: 'technology', header: 'technology', fieldType: FieldTypeEnum.STRING },
      type: { field: 'type', header: 'type', fieldType: FieldTypeEnum.STRING },
      path: { field: 'path', header: 'path', fieldType: FieldTypeEnum.STRING },
      validation: { field: 'validationResult', header: 'moduleListing.validationResult' },
    },
    paginator: true,
    rows: DEFAULT_NUMBER_OF_ROWS,
    showTotalCount: true,
    tableBorder: false,
    isExportVisible: false,
    serverSidePagination: false
  };

  tableData: ModuleTableItem[] = [];
  totalRecords: number;
  modules: string[] = [];
  invalidModules: string[] = [];
  invalidMessage = '';
  loadingValidateBtn = false;
  moduleListForm: UntypedFormGroup;
  combinedValues: string[] | string = [];
  listSearchModule: ListDetail[] = [];

  constructor(private graphQlControllerService: GraphQlControllerService,
    private messageService: NzMessageService,
    private translateService: TranslateService,
    public modal: NzModalRef,
    private formBuilder: UntypedFormBuilder,
    public parametersService: CustomizableTableParametersService,
    private moduleListingService: ModuleListingService,
    private modulesGQL: ModulesGQL
  ) { }

  ngOnInit(): void {
    this.tableConfig.actions = [[{ value: 'moduleAssignTable', icon: 'close', prop: { type: 'text', size: 'small' } }],];
    this.moduleListForm = this.formBuilder.group({
      moduleList: this.moduleIds?.length ? new UntypedFormControl([...this.moduleIds]) : new UntypedFormControl([])
    });
    const moduleTableState = this.moduleListingService.getModuleTableState(this.moduleType);
    if (this.moduleListForm.value.moduleList.length && moduleTableState?.length) {
      this.tableData = moduleTableState;
      this.tableConfig.columnMap['validation'].displayAs = ViewMode.ICON;
    } else {
      this.onValidation();
    }
  }

  ngAfterViewInit(): void {
    const modulesName: string[] = this.tableData?.map((tableDataItem: ModuleTableItem) => tableDataItem.module);
    this.addStyleForRepeatedModules(modulesName);
  }

  /**
   * method will be called on search on select
   * @param  search input property will be sent from select
   */
  onNameSearch(search: string): void {
    const dropdownElement = document.querySelectorAll('[id^="cdk-overlay-"]');
    const dropdownClassList = dropdownElement[dropdownElement.length - 1]?.classList;
    dropdownClassList?.add('module-listing__select-dropdown');
    this.listSearchModule = [];
    if (search.trim().length >= 1) {
      dropdownClassList?.remove('module-listing__select-dropdown');
      this.moduleSearchLoading = true;
      const filterObject = {
        content_name: { eq: search }
      };
      this.modulesGQL.fetch({ projectId: this.projectId, size: this.pageSize, page: 0, filterObject }).subscribe((listModule) => {
        this.moduleSearchLoading = false;
        listModule.data.modules.content.forEach((module) => {
          if (this.listSearchModule.findIndex(moduleOpt => moduleOpt.label === module.name) === -1) {
            this.listSearchModule.push({ value: module.name, label: module.name });
          }
        });
      });
    }
  }

  /**
   * method to get tags values on blur
   */
  getNewValues(): void {
    this.combinedValues = this.tagFieldInput['activatedValue'];
  }

  /**
   * method to start validation
   */
  onValidation(allValidationData?: string): void {
    this.combinedValues = this.tagFieldInput ?
      [...this.moduleListForm.controls.moduleList.value, this.tagFieldInput['activatedValue']].join(',')
      : this.moduleListForm.controls.moduleList.value;
    this.modules = this.combinedValues ? convertToTag(this.combinedValues as string) as string[] : [];
    this.moduleListForm.controls.moduleList.setValue([...this.modules]);
    this.getTableData([...this.modules], allValidationData);
  }

  /**
   * method to remove record from the table
   * @param  record record which needs to be removed
   */
  removeRecordFromTable(record: MiningTableOptionSelected): void {
    const removeRecordIndex = this.tableData.findIndex((tableDataItem) => tableDataItem.id === record.data.id);
    this.tableData.splice(removeRecordIndex, 1);
    this.tableData = [...this.tableData];
    this.totalRecords = this.tableData.length;
    this.messageService.success(`${this.translateService.instant('moduleListing.moduleRemoved')}`);
  }

  /**
   * method to send back data to parent component
   */
  sendDataToParentComponent(): void {
    const modulesId: number[] = this.tableData.map((tableDataItem: ModuleTableItem) => tableDataItem.id);
    const modulesName: string[] = this.tableData.map((tableDataItem: ModuleTableItem) => tableDataItem.module);
    let repeatedModuleIds: number[][] = [];
    for (const module of modulesName) {
      const moduleIds = this.tableData.filter((tableItem: ModuleTableItem) => tableItem.module === module).map((filterTableItem) => filterTableItem.id);
      repeatedModuleIds.push(moduleIds);
    }
    // To make unique array of arrays
    repeatedModuleIds = Array.from(new Set(repeatedModuleIds.map(x=>JSON.stringify(x))), x=>JSON.parse(x));
    this.moduleListingService.setModuleTableState(this.tableData, this.moduleType);
    this.modal.close({ modulesId, modulesName, selectedOption: 'name', repeatedModuleIds });
  }

  /**
   * method on closing the modal
   */
  closeModal(): void {
    this.modal.close('');
  }

  /**
   * method to be called on enter hit for tag input
   * @param  value tags present on the input field
   */
  onEnterConvertToTag(value: string[] | string): void {
    this.combinedValues = value ? [...value, this.tagFieldInput['activatedValue']].join(',') : this.tagFieldInput['activatedValue'];
    const convertedTags = convertToTag(this.combinedValues as string) as string[];
    this.moduleListForm.controls.moduleList.setValue(convertedTags);
  }

  /**
   * method to get all repeated modules and invalid modules (made it public to increase the coverage)
   * @param allModules repeated module in the response from API
   * @param validModules valid module present in the response
   * @returns RepeatedAndInvalidModules invalid and repeated modules return
   */
    getInvalidDuplicateModule(allModules: string[], validModules: string[]): RepeatedAndInvalidModules {
    const getTagElement: HTMLCollection = document.getElementById('tags')?.children[0]?.children;
    if (getTagElement) {
      this.invalidModules = this.modules.filter((module: string) => validModules.indexOf(module.toLowerCase()) === -1);
      const duplicateModules: string[] = allModules.filter((repeatedModule, index) => allModules.indexOf(repeatedModule) !== index);
      for (const element of getTagElement as unknown as HTMLElement[]) {
        if (this.invalidModules.indexOf(element.textContent) > -1) {
          element.classList.add('module-listing__invalid-module');
        } else if (duplicateModules.indexOf(element.textContent) > -1) {
          element.classList.add(repeatedModuleClass);
        }
        this.invalidMessage = `${this.translateService.instant('moduleListing.moduleCouldNotFound')} : ${this.invalidModules.join(',')}`;
      }
      return { invalidModules: this.invalidModules, duplicateModules };
    }
  }

  private addStyleForRepeatedModules(modules: string[]): void {
    const getTagElement: HTMLCollection = document.getElementById('tags')?.children[0]?.children;
    if (getTagElement) {
      const duplicateModules: string[] = modules.filter((repeatedModule, index) =>
        modules.indexOf(repeatedModule) !== index);
      for (const element of getTagElement as unknown as HTMLElement[]) {
        if (duplicateModules.indexOf(element.textContent) > -1) {
          element.classList.add(repeatedModuleClass);
        }
      }
    }
  }

  private getTableData(moduleList: string[], allValidationData: string): void {
    const content = [
      { name: 'name', path: 'content.name' },
      { name: 'path', path: 'content.path' },
      { name: 'id', path: 'content.id' },
      { name: 'technology', path: 'content.technology' },
      { name: 'type', path: 'content.type' }
    ];
    const filter: { [key: string]: any } = {};
    if (this.moduleIdsFromReportingPage?.length && !allValidationData && moduleList?.length) {
      filter.content_id = { in: JSON.parse(this.moduleIdsFromReportingPage) };
    } else {
      filter.content_name = { in: moduleList };
    }
    const requestQuery: { [key: string]: any } = {
      'query': graphQlQuery(
        'modules',
        { 'projectId': this.projectId, 'filterObject': '$filter'},
        content,
        ['totalElements', 'size'],
        true
      ),
      'variables': {
        filter
      }
    };
    this.tableConfig.loading = true;
    this.loadingValidateBtn = true;
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      this.loadingValidateBtn = false;
      this.tableData = this.createTable(response.data?.modules as Modules);
      this.totalRecords = this.tableData.length;
      this.tableConfig.loading = false;
    });
  }

  private createTable(modules: Modules): ModuleTableItem[] {
    if ( ! modules) {
      return [];
    }
    const allModules: string[] = [];
    const validModules: string[] = [];
    const modulesTableData: ModuleTableItem[] = [];
      modules.content.forEach((module: any) => {
      this.invalidModules.length = 0;
      allModules.push(module.name as string);
      if (this.modules.indexOf(module.name.toLowerCase() as string) > -1 || this.modules.indexOf(module.name.toUpperCase() as string) > -1) {
        validModules.push(module.name.toLowerCase() as string);
      };
      const moduleTableItem: ModuleTableItem = {
        id: module.id,
        module: module.name,
        path: module.path,
        technology: module.technology,
        type: module.type,
        validationResult: { iconType: 'check', iconTheme: 'outline' }
      };
      this.tableConfig.columnMap['validation'].displayAs = ViewMode.ICON;
      modulesTableData.push(moduleTableItem);
    });
    const { duplicateModules } = this.getInvalidDuplicateModule(allModules, validModules);
    modulesTableData.forEach((moduleData, index: number) => {
      if (duplicateModules.indexOf(moduleData.module) > -1) {
        modulesTableData[index].rowClassName = repeatedModuleClass;
        modulesTableData[index].validationResult = this.translateService.instant('moduleListing.duplicateModule');
        this.tableConfig.columnMap['validation'].displayAs = ViewMode.ICON;
      }
    });
    return modulesTableData;
  }

  private navigateToDetails(module: number): string {
    if (module) {
      return RouteBuilder.buildModuleRoute(this.projectId, module, 'details/overview');
    }
   }
}
