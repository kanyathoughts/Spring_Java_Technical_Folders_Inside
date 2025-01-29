import { AfterViewInit, Component, ElementRef, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { UntypedFormBuilder, UntypedFormControl } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { ModuleListingComponent } from '@app/shared/components/module-listing/module-listing.component';
import { ModuleDetails } from '@app/shared/components/module-listing/module-listing.interface';
import { ModuleListingService } from '@app/shared/components/module-listing/module-listing.service';
import { TranslateService } from '@ngx-translate/core';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { ListDetail } from '../call-chain-export.interface';
import { graphQlQuery } from '@app/core/utils/graphql.util';
import { GraphQlControllerService } from '@app/core/services/graphql.service';

@Component({
  selector: 'module-selection',
  templateUrl: './module-selection.component.html'
})

export class ModuleSelectionComponent implements OnInit, OnDestroy, AfterViewInit {
  @Input() callChainModuleLabel?: string;
  @Input() listModuleTypeOptions: Array<{ value: string, label: string }>;
  @Input() currentModuleDetails: ListDetail[];
  @Input() projectId: number;
  @Input() formModule: string;
  @Output() moduleChange = new EventEmitter();
  moduleSelectionForm = this.fb.group({
    startModule: this.fb.control([]),
    endModule: this.fb.control([])
  });
  allModuleOption = {
    endLabel: 'Any module',
    startLabel: 'callChain.entryModules',
    value: '-1',
    selected: true
  };
  selectedRadio: string;
  Options: string[] = [];
  moduleOptions: ListDetail[] = [];
  selectByValue: string[];
  selectByTypeValue: string[];
  createModalForModuleListing: NzModalRef;
  modulesFromModuleListingCmp: {
    start: string[],
    end: string[]
  } = { start: [], end: [] };
  moduleDetails: Array<{ label: string, value: number[] }> = [];
  formLabel: string;
  enableLoading = false;
  dropdownLabel: string;
  totalModuleTagCount: number;
  private getModuleIdsFromReportingPage: string;
  private isSelectOpen: boolean;

  constructor(private fb: UntypedFormBuilder,
    private moduleListingService: ModuleListingService,
    private modalService: NzModalService,
    private translateService: TranslateService,
    private route: ActivatedRoute,
    private elRef: ElementRef,
    private graphQlControllerService: GraphQlControllerService
  ) { }

  get startModule(): UntypedFormControl {
    return this.moduleSelectionForm.get('startModule') as UntypedFormControl;
  }

  get endModule(): UntypedFormControl {
    return this.moduleSelectionForm.get('endModule') as UntypedFormControl;
  }

  ngOnInit(): void {
    this.formLabel = this.callChainModuleLabel === 'start' ? 'callChain.start' : 'callChain.end';
    this.dropdownLabel = this.callChainModuleLabel === 'start' ? this.allModuleOption.startLabel : this.allModuleOption.endLabel;
    this.route.data.subscribe(() => {
      this.createModalForModuleListing?.destroy();
    });
    if ( ! this.currentModuleDetails) {
      if (this.formModule === 'startModule') {
        this.getModuleDetailsFromReporting();
      }
    } else {
      if (this.formModule === 'startModule') {
        const startMappedLabel = this.currentModuleDetails.map((module: ListDetail) => module.label);
        this.Options = startMappedLabel;
        this.startModule.setValue([...startMappedLabel]);
        this.moduleDetails = this.currentModuleDetails.map((module: ListDetail) => ({
          label: module.label, value: [module.value] as number[]
        }));
        this.selectedRadio = 'name';
      }
    }
    if (this.formModule === 'endModule') {
      this.selectedRadio = this.allModuleOption.value;
      this.endModule.setValue([this.allModuleOption.value]);
    }
    this.moduleListingService.setModuleTableState([], 'start');
    this.moduleListingService.setModuleTableState([], 'end');
  }

  /**
   * Method to get the module details for start based on module ids from module reporting page.
   */
  getModuleDetailsFromReporting(): void {
    this.getModuleIdsFromReportingPage = localStorage.getItem(`${this.projectId}-reporting_moduleIds`);
    if ( ! this.getModuleIdsFromReportingPage) {
      this.startModule.setValue([this.allModuleOption.value]);
      this.selectedRadio = this.allModuleOption.value;
    } else {
      this.enableLoading = true;
      const content = [
        { name: 'name', path: 'content.name' },
        { name: 'id', path: 'content.id' }
      ];
      const requestQuery: { [key: string]: any } = {
        'query': graphQlQuery(
          'modules',
          { 'projectId': this.projectId, 'filterObject': '$filter' },
          content,
          ['totalElements', 'size'],
          true
        ),
        'variables': {
          filter: { content_id: { in: this.getModuleIdsFromReportingPage.slice(1, -1).split(',')}}
        }
      };
      this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
        this.enableLoading = false;
        localStorage.removeItem(`${this.projectId}-reporting_moduleIds`);
        const responseModules = response?.data.modules?.content;
        const uniqueModules: string[] = [];
        responseModules.forEach((module: any) => {
          if ( ! uniqueModules.includes(`${module.name}`)) {
            uniqueModules.push(module.name as string);
          }
        });
        const repeatedModuleIds: number[][] = [];
        for (const module of uniqueModules) {
          const moduleIds: number[] = responseModules.filter((item: any) => item.name === module).map((filterTableItem: any) => filterTableItem.id);
          repeatedModuleIds.push(moduleIds);
        }
        this.modulesFromModuleListingCmp.start = uniqueModules;
        this.Options = uniqueModules;
        this.startModule.patchValue(uniqueModules);
        this.moduleOptions = uniqueModules.map((moduleName: string, index: number) =>
          ({ label: moduleName, value: repeatedModuleIds[index][0], count: repeatedModuleIds[index].length }));
        this.moduleDetails = uniqueModules.map((moduleName: string, index: number) =>
          ({ label: moduleName, value: repeatedModuleIds[index] }));
        this.selectedRadio = 'name';
        this.emitModuleValues('startModule');
      });
    }
  }

  /**
   * Changes Label for start box based on direction value.
   * @param directionValue value used to change the start label.
   */
  changeStartLabel(directionValue: string[]): void {
    const length = directionValue.length;
    if (directionValue.includes('IN') && length === 1) {
      this.allModuleOption.startLabel = 'callChain.endModules';
    } else if (directionValue.includes('OUT') && length === 1) {
      this.allModuleOption.startLabel = 'callChain.entryModules';
    } else {
      this.allModuleOption.startLabel = 'callChain.entry&endModules';
    }
    this.dropdownLabel = this.allModuleOption.startLabel;
  }

  /**
   * Method for setting the radio button selection when click on start/end field.
   */
  handleModuleSelection(): void {
    this.Options = [];
    this[this.formModule].setValue([]);
    switch (this.selectedRadio) {
      case this.allModuleOption.value:
        this[this.formModule].setValue([this.allModuleOption.value]);
        break;
      case 'name':
        this.Options = this.selectByValue;
        this[this.formModule].setValue(this.selectByValue);
        break;
      case 'type':
        const getOptionValue = this.getOptionValue(this.selectByTypeValue, this.listModuleTypeOptions);
        this.Options = this.getLabelFromValue(getOptionValue);
        this[this.formModule].setValue(this.getLabelFromValue(getOptionValue));
        break;
    }
  }

  /**
   * Method to update Type value for start/end input.
   * @param updateValue type value to be updated.
   */
  updateTypeSelect(updateValue: string[]): void {
    switch (this.selectedRadio) {
      case 'type':
        const getOptionValue = this.getOptionValue(updateValue, this.listModuleTypeOptions);
        this.Options = this.getLabelFromValue(getOptionValue);
        this[this.formModule].setValue(this.getLabelFromValue(getOptionValue));
        break;
      case 'name':
        this.Options = updateValue;
        this[this.formModule].setValue(updateValue);
        break;
    }
  }

  /**
   * Method to update values when start/end dropdown expand changes.
   * @param selectOpens boolean value of dropdown status(open/close).
   */
  onOpenSelect(selectOpens: boolean): void {
    this.isSelectOpen = selectOpens;
    if (selectOpens) {
      switch (this.selectedRadio) {
        case 'name':
          this.selectByValue = this[this.formModule].value;
          break;
        case 'type':
          this.selectByTypeValue = this.getSelectByTypeValue(this[this.formModule].value as string[], this.listModuleTypeOptions);
          break;
      }
    } else {
      this.checkModuleValue(this.formModule);
    }
  }

  ngAfterViewInit(): void {
    const input: HTMLInputElement = this.elRef.nativeElement.querySelector('.module-selection_select input');
    input.setAttribute('readonly', 'true');
  }

  /**
   * Checks Module Value and sets it to All Module if it is empty.
   * @param module which modules needs to be set to All Module start or end
   */
  checkModuleValue(module: string): void {
    this.selectByValue = this[this.formModule].value;
    this.checkRemovedModule(this.callChainModuleLabel, this[module] as UntypedFormControl);
    if ( ! this.isSelectOpen && ! this[module].value?.length && this.selectedRadio !== '-1') {
      this[module].setValue([this.allModuleOption.value]);
      this.selectedRadio = this.allModuleOption.value;
    }
    this.emitModuleValues(module);
  }

  /**
   * method for start and end modal
   * @param param decider from start and end module
   */
  openModalForModules(param: string): void {
    this.createModalForModuleListing?.destroy();
    const moduleValues: string[] = this[this.formModule].value?.includes('-1') ? [] : this[this.formModule].value;
    this.createModalForModuleListing = this.modalCreator(moduleValues, param);
    this.createModalForModuleListing.afterClose.subscribe((modulesDetails: ModuleDetails) => {
      this.createModalForModuleListing.destroy();
      if (modulesDetails && Object.keys(modulesDetails).length) {
        this.getModuleIdsFromReportingPage = '';
        const uniqueModules: string[] =
        modulesDetails.modulesName.filter((moduleName: string, index: number) => modulesDetails.modulesName.indexOf(moduleName) === index);
        this.settingModuleDetailsFromModal(uniqueModules, modulesDetails, param);
      }
    });
  }

  ngOnDestroy(): void {
    this.createModalForModuleListing?.destroy();
  }

  private emitModuleValues(module: string) {
    this.moduleChange.emit({
      [module]: this[module].value,
      selectedRadio: this.selectedRadio,
      moduleDetails: this.moduleDetails
    });
  }

  private checkRemovedModule(propertyName: string, module: UntypedFormControl): void {
   this.totalModuleTagCount = this.updatemoduleTagCount(module);
    if (this.modulesFromModuleListingCmp[propertyName]?.length) {
      const removedModule = this.modulesFromModuleListingCmp[propertyName]?.filter((d: any) => ! module.value.includes(d));
      if (removedModule?.length) {
        // This condition is to update the selected module Ids as the same will be passed to Module Lisiting.
        if (propertyName === 'start') {
          const remainingModules: number[] = [];
          module.value.forEach((m: string) => {
            const findModule = this.moduleDetails.find(md => md.label === m);
            remainingModules.push(...findModule.value);
          });
          this.getModuleIdsFromReportingPage = JSON.stringify(remainingModules);
        }
        this.moduleListingService.setModuleTableState([], propertyName);
        this.modulesFromModuleListingCmp[propertyName] = [];
      }
    }
  }

  private updatemoduleTagCount(module: UntypedFormControl): number {
    if( this.selectedRadio === 'name' && module.value.length) {
      let totalTag = 0;
      const moduleLabel = this.moduleDetails.map((moduleItem) => moduleItem.label);
      module.value.forEach((moduleItem: string) => {
        const matchedIndex = moduleLabel.indexOf(moduleItem);
        totalTag = totalTag + this.moduleDetails[matchedIndex]?.value.length;
      });
      return totalTag;
    } else if ( this.selectedRadio === 'type' && module.value.length) {
      return  module.value.length;
    } else if ( this.getModuleIdsFromReportingPage?.length) {
      return JSON.parse(this.getModuleIdsFromReportingPage).length;
    }
  }

  private modalCreator(moduleIds: string[], moduleType: string): NzModalRef {
    const modal = this.modalService.create<ModuleListingComponent>({
      nzTitle: this.translateService.instant('moduleListing.title'),
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzWrapClassName: 'vertical-center-modal',
      nzWidth: '1080px',
      nzContent: ModuleListingComponent
    });
    const instance = modal.getContentComponent();
    instance.moduleIds = moduleIds;
    instance.projectId = this.projectId;
    instance.pageSize = 10;
    instance.moduleType = moduleType;
    instance.moduleIdsFromReportingPage = this.getModuleIdsFromReportingPage;
    return modal;
  }

  private getOptionValue(selectedList: string[], listOptions: ListDetail[]): ListDetail[] {
    const data: ListDetail[] = [];
    selectedList?.forEach(element => {
      data.push(...listOptions.filter(x => x.value === element));
    });
    return data;
  }

  private getLabelFromValue(selectedList: ListDetail[]): string[] {
    return selectedList.map((selectedItem: ListDetail) => selectedItem.label);
  }

  private getSelectByTypeValue(selectedList: string[], listOptions: ListDetail[]): any[] {
    return selectedList?.map(element => listOptions[listOptions.findIndex(x => x.value === element || x.label === element)]?.value);
  }

  private settingModuleDetailsFromModal(uniqueModules: any, modulesDetails: any, param: string) {
    this.modulesFromModuleListingCmp[param] = uniqueModules;
    this.totalModuleTagCount = modulesDetails.modulesId.length;
    this.Options = uniqueModules;
    this.moduleOptions = uniqueModules.map((moduleName: string, index: number) =>
      ({ label: moduleName, value: modulesDetails.repeatedModuleIds[index][0], count: modulesDetails.repeatedModuleIds[index].length }));
    this.moduleDetails = uniqueModules.map((moduleName: string, index: number) =>
      ({ label: moduleName, value: modulesDetails.repeatedModuleIds[index] }));
    this.selectedRadio = modulesDetails.selectedOption;
    this.selectByValue = uniqueModules;
    this[this.formModule].patchValue(uniqueModules);
    this.emitModuleValues(this.formModule);
  }
}
