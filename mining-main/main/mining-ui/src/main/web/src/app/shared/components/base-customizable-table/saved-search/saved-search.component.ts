import { Component, OnInit, ViewChild, Output, Input, EventEmitter, TemplateRef, SimpleChanges, OnChanges, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { SavedSearchMode } from './saved-search.interface';
import { HttpErrorResponse } from '@angular/common/http';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { Logger } from '@app/core';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { UntypedFormBuilder, UntypedFormControl, UntypedFormGroup, ValidationErrors, Validators } from '@angular/forms';
import { Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { ReachabilityService } from '@app/modules/reachability-ui-product-vision/utils/reachability.service';
import { SavedSearchPojo, SavedSearchControllerService } from '@innowake/mining-api-angular-client';

const log = new Logger('DataDictionaryComponent');
const shareOptions = {
  PROJECT: 'Project Team',
  CLIENT: 'Client Team',
  GLOBAL: 'All Mining Users'
};
@Component({
  selector: 'app-saved-search',
  templateUrl: './saved-search.component.html'
})
export class SavedSearchComponent implements OnInit, OnChanges, OnDestroy {

  @ViewChild('saveSearchTemplate') saveSearchTemplate: TemplateRef<any>;
  @ViewChild('nzFooter') nzFooter: TemplateRef<any>;
  @Input() savedSearchList: SavedSearchPojo[] = [];
  @Input() pageTitle: string;
  @Input() selectedSavedSearchName: string;
  @Input() projectId: number;
  @Input() usage: Usages;
  @Input() createNewSavedSearch = false;
  @Input() createSavedSearchTitle = this.translateService.instant('savedSearchModal.buttonTitle');
  @Output() savedSearchStatus: EventEmitter<any> = new EventEmitter();
  shareOptions: Array<{ value: string, label: string }> = [];
  initialTableConfig: { [key: string]: any };
  savedSearchMode = SavedSearchMode;
  selectedSavedSearch: SavedSearchPojo;
  confirmModal: NzModalRef;
  savedSearchForm: UntypedFormGroup;
  savedSearchText: { [key: string]: any };
  checkText: string;
  savedSearchDetails: { savedSearchName: string; initialTableConfig: { [key: string]: any; }; route: ActivatedRoute; };
  shareWith: string;
  scopeSelected = false;
  username: string;
  userId: string;
  modulesWithErrorsAndWarnings: { [key: string]: any };
  constructor(
    public reachabilityService: ReachabilityService,
    public router: Router,
    public route: ActivatedRoute,
    public messageService: NzMessageService,
    public customizableTableColumnService: CustomizableTableColumnService,
    public translateService: TranslateService,
    public modalService: NzModalService,
    public savedSearchControllerService: SavedSearchControllerService,
    public notification: NzNotificationService,
    public parametersService: CustomizableTableParametersService,
    private fb: UntypedFormBuilder,
    private authorizationService: KeycloakAuthorizationService,
    private IAMService: IdentityAccessManagementService) {
    this.shareOptions.push(
      { value: SavedSearchPojo.ScopeEnum.INDIVIDUAL, label: this.translateService.instant('savedSearchModal.shareWithIndividual') },
      { value: SavedSearchPojo.ScopeEnum.PROJECT, label: this.translateService.instant('savedSearchModal.shareWithProject') },
      { value: SavedSearchPojo.ScopeEnum.CLIENT, label: this.translateService.instant('savedSearchModal.shareWithClient') },
    );
    if (this.authorizationService.isClientAdmin()) {
      this.shareOptions.push({ value: SavedSearchPojo.ScopeEnum.GLOBAL, label: this.translateService.instant('savedSearchModal.shareWithGlobal') });
    }
    this.shareWith = this.shareOptions[0].value;
  }

  ngOnInit(): void {
    this.username = this.IAMService.getUsername();
    this.userId = this.IAMService.getUserId();
    this.savedSearchForm = this.fb.group({
      name: ['', [Validators.required, this.checkDuplicateList.bind(this)]],
      shareWith: ['']
    });
    this.modulesWithErrorsAndWarnings = JSON.parse(localStorage.getItem('ModulesWithErrorsAndWarnings'));
  }


  ngOnChanges(changes: SimpleChanges): void {
    if (changes.selectedSavedSearchName?.currentValue || changes.savedSearchList?.currentValue) {
      this.selectedSavedSearch = this.savedSearchList.find(savedSearch => savedSearch.name === this.selectedSavedSearchName);
    }
  }

  ngOnDestroy(): void {
    // @yjs:keep
    localStorage.removeItem('ModulesWithErrorsAndWarnings');
  }

  /**
   * Method to open the modules table with errors/missing dependencies.
   * @param tag error/warning tag.
   */
  openModulesTable(tag: string): void {
    this.reachabilityService.openModulesTable(tag, this.projectId);
  }

  /**
   * Method to triggered the filter on the basis of selection
   * @param filterDetails contains the data of filter.
   */
  onSavedSearchSelection(): void {
    this.selectedSavedSearch = this.savedSearchList.find(savedSearch => savedSearch.name === this.selectedSavedSearchName);
    const defaultSortBy = this.customizableTableColumnService.getDefaultSortBy(this.usage, this.customizableTableColumnService.dataPointsList);
    this.parametersService.setParameters([], 1, defaultSortBy, '');
    this.savedSearchDetails = {
      savedSearchName: this.selectedSavedSearchName,
      initialTableConfig: this.initialTableConfig,
      route: this.route
    };
    this.setSavedSearchSessionStorage(this.pageTitle, this.selectedSavedSearchName);
    this.savedSearchStatus.emit(this.savedSearchDetails);
    if ( ! this.selectedSavedSearch) {
      this.shareWith = this.shareOptions[0].value;
      this.customizableTableColumnService.resetTable();
      return;
    }
    this.setRoute();
    this.setSavedSearchParameter(this.selectedSavedSearch);
  }

  /**
   * Initialize the table configuration with the saved search
   * @param savedSearch Saved Search name
   */
  setSavedSearchParameter(savedSearchDetails: SavedSearchPojo): void {
    this.shareWith = savedSearchDetails.scope;
    const savedSearch = savedSearchDetails.savedSearch;
    const data = savedSearch?.split('&');
    let columns: string[];
    let page: number;
    let sort;
    let filter: Array<{ key: string, value: any }>;
    let filterString: string;
    if (data.find(parameter => parameter.includes('page'))) {
      page = Number.parseInt(data.find(param => param.includes('page=')).replace('page=', ''), 10);
    }
    if (data.find(parameter => parameter.includes('filter'))) {
      filterString = data.find(param => param.includes('filter=')).replace('filter=', '');
      filter = JSON.parse(filterString);
    }
    if (data.find(parameter => parameter.includes('sort'))) {
      sort = data.find(param => param.includes('sort=')).replace('sort=', '');
    }
    this.parametersService.setParameters(filter, page, sort, '');
    if (data.find(parameter => parameter.includes('columns'))) {
      columns = data.filter(param => param.includes('columns='));
      columns = columns.map(param => param.replace('columns=', ''));
      this.customizableTableColumnService.setColumnIdsSelection(columns);
    }
    this.initialTableConfig = this.getTableConfig(columns, filterString, sort);
    this.savedSearchDetails = {
      savedSearchName: this.selectedSavedSearchName,
      initialTableConfig: this.initialTableConfig,
      route: this.route
    };
    this.savedSearchStatus.emit(this.savedSearchDetails);
  };

  /**
   * Method return column filter and the sorting object
   * @param columns columns of table
   * @param filter selected filter
   * @param sort sorting base ascending or descending
   */
  getTableConfig(columns: string[], filter: string, sort: string): { [key: string]: any } {
    return {
      columns: columns?.sort((a: string, b: string) => a.localeCompare(b)).join(','),
      filter,
      sort
    };
  }

  /**
   * Method called by the cancel button
   */
  onCancel(): void {
    this.confirmModal.close('cancel');
  }

  /**
   * Method to move to the different routing page
   */
  setRoute(): void {
    let queryParams: { [key: string]: any } = {};
    if (this.selectedSavedSearchName) {
      queryParams = {
        savedSearch: this.selectedSavedSearchName
      };
    } else {
      queryParams = this.parametersService.getTableParametersForUrl();
      queryParams.columns = this.customizableTableColumnService.selectedColumns;
    }
    this.router.navigate(
      [],
      {
        relativeTo: this.route,
        queryParams,
        replaceUrl: true
      }
    ).catch(() => { });
  }

  /**
   * open the delete save search modal for confirmation
   */
  showDeleteConfirmationModal(): void {
    const selectedSavedSearch = this.savedSearchList.find(savedSearch => savedSearch.name === this.route.snapshot.queryParams.savedSearch);
    const deleteSavedSearchTitle = this.translateService.instant('savedSearchModal.deleteSaveSearchTitle', {
      name: selectedSavedSearch.name
    });
    this.confirmModal = this.modalService.confirm({
      nzTitle: deleteSavedSearchTitle,
      nzContent: this.deleteConfirmModalContent(selectedSavedSearch.scope),
      nzOkText: this.translateService.instant('btnLabel.delete'),
      nzOkType: 'primary',
      nzIconType: 'warning',
      nzOkDanger: true,
      nzOkLoading: false,
      nzOnOk: () => this.deleteSavedSearch(selectedSavedSearch),
      nzCancelText: this.translateService.instant('btnLabel.cancel'),
    });
  }

  /**
   * Method updates, create the save search
   * @param selectedOption as string, if it is create, update or rename
   */
  saveAndUpdateSavedSearch(selectedOption: string, shareWith?: string): void {
    let title = '';
    this.scopeSelected = shareWith ? true : false;
    this.shareWith = shareWith ? shareWith : this.shareWith;
    this.savedSearchForm.controls['shareWith'].setValue(this.shareWith);
    let isSavedSearch = this.route.snapshot.queryParams;
    this.savedSearchText = isSavedSearch;
    this.checkText = selectedOption;
    const checkSavedSearch = this.route.snapshot.queryParams.savedSearch ? this.route.snapshot.queryParams.savedSearch : null;
    let queryParams: { [key: string]: any } = {};
    title = this.setTitle(selectedOption);
    if ((checkSavedSearch && selectedOption === SavedSearchMode.CREATE) || SavedSearchMode.DUPLICATE) {
      queryParams = this.parametersService.getTableParametersForUrl();
      queryParams.columns = this.customizableTableColumnService.selectedColumns;
      isSavedSearch = queryParams;
      this.savedSearchText = isSavedSearch;
    }
    if (checkSavedSearch && selectedOption === SavedSearchMode.CREATE) {
      const updatedSavedSearch = this.savedSearchList.find(savedSearch => savedSearch.name === this.route.snapshot.queryParams.savedSearch);
      updatedSavedSearch.savedSearch = this.savedSearchString(isSavedSearch);
      this.updateSavedSearch(checkSavedSearch as string);
    } else {
      this.openModal(title);
    }
  }

  /**
   * Method will switch to the last saved state
   */
  resetSaveSearch(): void {
    this.onSavedSearchSelection();
  }
  /**
   * Method to get tooltip content for icon
   */
  getTooltipContent(scope?: SavedSearchPojo.ScopeEnum, createdByUserName?: string): string {
    createdByUserName = createdByUserName === '' ? this.translateService.instant('savedSearchModal.createdByUserIdDefault') : createdByUserName;
    return this.translateService.instant('savedSearchModal.tooltipText',
      { createdByUserId: createdByUserName?.charAt(0).toUpperCase() + createdByUserName?.slice(1).toLowerCase(), sharedWith: shareOptions[scope] });
  }


  /**
   * Method called by the submit button
   * @param savedSearchMode as string of create or duplicate
   */
  onSubmit(savedSearchMode: string): void {
    if (savedSearchMode === this.savedSearchMode.DUPLICATE || savedSearchMode === this.savedSearchMode.CREATE) {
      this.createSavedSearch();
    } else {
      this.updateSavedSearch();
    }
  }

  /**
   * Checks if we have to show duplicate or Save button option.
   * @returns Boolean value
   */
  showDuplicateButton(): boolean {
    const search = this.selectedSavedSearch;
    return search && search.createdByUserName !== this.username && search.createdByUserId !== this.userId;
  }

  private deleteSavedSearch(savedSearch: SavedSearchPojo) {
    this.confirmModal.updateConfig({
      nzOkLoading: true,
      nzCancelLoading: true,
      nzClosable: false
    });
    this.savedSearchControllerService.deleteSavedSearch(this.projectId, savedSearch.id).subscribe(() => {
      const index = this.savedSearchList.findIndex(item => item.id === savedSearch.id);
      this.savedSearchList.splice(index, 1);
      this.selectedSavedSearchName = '';
      this.messageService.success(this.translateService.instant('savedSearchModal.deleteSuccess', { name: savedSearch.name }) as string);
      this.onSavedSearchSelection();
    }, (error: HttpErrorResponse) => {
      this.errorNotificationMsg(error, 'savedSearchModal.deleteFailed', savedSearch.name);
    });
  }

  private setTitle(selectedOption: string): string {
    if (SavedSearchMode.CREATE === selectedOption) {
      return this.translateService.instant('savedSearchModal.saveSearch');
    } else if (SavedSearchMode.RENAME === selectedOption) {
      return this.translateService.instant('savedSearchModal.RenameSearchHeader');
    } else {
      return this.translateService.instant('savedSearchModal.DuplicateSavedSearchHeader');
    }
  }

  private savedSearchString(isSavedSearch: { [key: string]: any }): string {
    let filterSearchString = '';
    Object.keys(isSavedSearch).forEach((element: string) => {
      if (element !== 'columns') {
        filterSearchString = filterSearchString.concat(element + '=' + isSavedSearch[element] + '&');
      }
    });
    const columnString = isSavedSearch['columns'] && isSavedSearch['columns'].map((x: string) => 'columns=' + x).join('&');
    return filterSearchString + columnString;
  }

  private openModal(title: any) {
    if (this.checkText === SavedSearchMode.RENAME) {
      this.savedSearchForm.controls['name'].setValue(this.selectedSavedSearchName);
    } else if (this.checkText === SavedSearchMode.DUPLICATE) {
      const selectedSavedSearch = this.savedSearchList.find(savedSearch => savedSearch.name === this.selectedSavedSearchName);
      this.savedSearchForm.controls['name'].setValue(this.selectedSavedSearchName + ' ' + this.translateService.instant('savedSearchModal.copy'));
      this.savedSearchForm.controls['shareWith'].setValue(selectedSavedSearch.scope);
    }
    this.confirmModal = this.modalService.create({
      nzTitle: title,
      nzClosable: true,
      nzKeyboard: true,
      nzMaskClosable: false,
      nzContent: this.saveSearchTemplate,
      nzWidth: 800,
      nzAutofocus: null,
      nzFooter: this.nzFooter
    });
    this.confirmModal.afterClose.subscribe(modalResult => {
      if (modalResult && modalResult !== 'cancel') {
        this.savedSearchList = [];
        this.selectedSavedSearchName = modalResult.name;
        this.savedSearchControllerService.findByUsage(this.projectId, this.usage).subscribe((result: [{ [key: string]: any }]) => {
          result.forEach((element: { [key: string]: any }) => {
            this.savedSearchList.push(element);
            this.savedSearchList.sort((a, b) => (a.name.localeCompare(b.name.toLowerCase())));
          });
          if (modalResult.name) {
            this.createSavedSearchTitle = this.translateService.instant('savedSearchModal.savedButtonTitle');
            this.onSavedSearchSelection();
          }
        });
      }
      this.savedSearchDetails = {
        savedSearchName: this.selectedSavedSearchName,
        initialTableConfig: this.initialTableConfig,
        route: this.route
      };
      this.savedSearchForm.controls['name'].reset();
    });
  }

  private checkDuplicateList(control: UntypedFormControl): ValidationErrors | null {
    const titleIndex = this.savedSearchList.findIndex(existingTitle => existingTitle.name.toLowerCase() === control.value.toLowerCase());
    return (titleIndex !== -1) ? { 'duplicate': true } : null;
  }

  private updateSavedSearch(checkSavedSearch?: string): void {
    const selectedSavedSearch = this.savedSearchList.find(savedSearch => savedSearch.name === this.selectedSavedSearchName);
    if (this.checkText === this.savedSearchMode.RENAME) {
      selectedSavedSearch.name = this.savedSearchForm.get('name').value;
    } else {
      const shareWith = this.savedSearchForm.get('shareWith').value;
      selectedSavedSearch.name = checkSavedSearch;
      selectedSavedSearch.scope = shareWith;
      selectedSavedSearch.projectId = shareWith === 'GLOBAL' ? 0 : this.projectId;
    }
    const name = this.savedSearchForm.get('name').value ? this.savedSearchForm.get('name').value : this.selectedSavedSearchName;
    this.savedSearchControllerService.updateSavedSearch(this.projectId, selectedSavedSearch.id, selectedSavedSearch).subscribe(details => {
      this.setSavedSearchParameter(details);
      this.createNewSavedSearch = false;
      this.createSavedSearchTitle = this.createNewSavedSearch ? this.translateService.instant('savedSearchModal.buttonTitle') : 'Saved';
      const scopeTranslateKey = this.savemModalContent(selectedSavedSearch.scope);
      this.closeModalBox(name as string, this.scopeSelected ? scopeTranslateKey : 'savedSearchModal.successSaved', details);
    }, (error: HttpErrorResponse) => {
      selectedSavedSearch.name = this.selectedSavedSearchName;
      this.errorNotificationMsg(error, 'savedSearchModal.failedSaved', this.savedSearchForm.get('name').value as string);
    });
  }

  private errorNotificationMsg(error: HttpErrorResponse, translateKey: string, name: string) {
    log.error(error);
    const errorMsg = this.translateService.instant(translateKey, { name }) as string;
    this.notification.error(errorMsg, this.translateService.instant('contactSupport') as string, { nzDuration: 0 });
  }

  private closeModalBox(name: string, translateKey: string, details: SavedSearchPojo) {
    this.messageService.create('success', `${this.translateService.instant(translateKey, { name })}`);
    this.confirmModal?.close(details);
  }

  private createSavedSearch(): void {
    const shareWith = this.savedSearchForm.get('shareWith').value;
    const newSavedSearch: SavedSearchPojo = {
      name: this.savedSearchForm.get('name').value,
      projectId: shareWith === 'GLOBAL' ? 0 : this.projectId,
      usage: this.usage,
      savedSearch: this.savedSearchString(this.savedSearchText),
      scope: shareWith
    };
    this.savedSearchControllerService.createSavedSearch(this.projectId, newSavedSearch).subscribe(details => {
      this.closeModalBox(this.savedSearchForm.get('name').value as string, 'savedSearchModal.successSaved', details);
    }, (error: HttpErrorResponse) => {
      this.errorNotificationMsg(error, 'savedSearchModal.failed', this.savedSearchForm.get('name').value as string);
    });
  }

  private deleteConfirmModalContent(scope: SavedSearchPojo.ScopeEnum): string {
    switch (scope) {
      case SavedSearchPojo.ScopeEnum.PROJECT:
      case SavedSearchPojo.ScopeEnum.CLIENT: {
        return this.translateService.instant('savedSearchModal.deleteSavedSearchContent', { scope: scope.toLowerCase() });
      }
      case SavedSearchPojo.ScopeEnum.GLOBAL: {
        return this.translateService.instant('savedSearchModal.deleteSavedSearchAllUsersContent');
      }
      default: {
        return;
      }
    }
  }

  private savemModalContent(scope: SavedSearchPojo.ScopeEnum): string {
    switch (scope) {
      case SavedSearchPojo.ScopeEnum.INDIVIDUAL: {
        return 'savedSearchModal.savedWithIndividual';
      }
      case SavedSearchPojo.ScopeEnum.PROJECT: {
        return 'savedSearchModal.savedWithProject';
      }
      case SavedSearchPojo.ScopeEnum.CLIENT: {
        return 'savedSearchModal.savedWithClient';
      }
      case SavedSearchPojo.ScopeEnum.GLOBAL: {
        return 'savedSearchModal.savedWithGlobal';
      }
      default: {
        return;
      }
    }
  }

  /**
   * Method saves the selected saved search name to the session storage based on the current page title
   * @param pageTitle the current page
   * @param selectedSavedSearchName selected saved search
   */
  private setSavedSearchSessionStorage(pageTitle: string, selectedSavedSearchName: string): void {
    switch (pageTitle) {
      case 'Modules':
        sessionStorage.setItem('modulesSavedSearch', JSON.stringify(selectedSavedSearchName));
        break;
      case 'Annotations':
        sessionStorage.setItem('annotationsSavedSearch', JSON.stringify(selectedSavedSearchName));
        break;
      case 'Data Dictionary':
        sessionStorage.setItem('dataDictionariesSavedSearch', JSON.stringify(selectedSavedSearchName));
        break;
    }
  }
}
