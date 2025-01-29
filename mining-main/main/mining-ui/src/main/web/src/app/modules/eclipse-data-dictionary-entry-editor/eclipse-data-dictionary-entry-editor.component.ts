import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Data, Params } from '@angular/router';
import { IdentityAccessManagementService } from '@app/core/authentication/identity-access-management.service';
import { FormResponse, FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { TranslateService } from '@ngx-translate/core';
import { combineLatest } from 'rxjs';
import { DataDictionaryControllerService, DataDictionaryPojo, DataFieldFormat, ModulePojo } from '@innowake/mining-api-angular-client';

declare const java__callback__onSave: () => Promise<string>;
declare const java__callback__onDelete: () => Promise<string>;
declare const java__callback__onCancel: () => Promise<string>;

@Component({
  selector: 'app-eclipse-data-dictionary-entry-editor',
  templateUrl: './eclipse-data-dictionary-entry-editor.component.html'
})
export class EclipseDataDictionaryEntryEditorComponent implements OnInit {

  moduleData: ModulePojo;
  dataDictionaryEntry: DataDictionaryPojo;
  message: string;
  isNewEntry: boolean;
  moduleTitle: string;

  constructor(
    private route: ActivatedRoute,
    private ddControllerService: DataDictionaryControllerService,
    private authService: IdentityAccessManagementService,
    private translateService: TranslateService
  ) { }

  ngOnInit(): void {
    combineLatest([
      this.route.params,
      this.route.queryParams,
      this.route.data
    ]).subscribe(([routeParams, queryParams, routeData]: [Params, Params, Data]) => {
      this.moduleData = routeData.module;
      this.moduleTitle = this.translateService.instant('sharedDataDictionaryEditor.headerTitle', { moduleName: this.moduleData.name });
      const projectId: number = routeParams.projectId.split('-')[1];
      if (routeParams?.dataDictionaryEntryId) {
        this.isNewEntry = false;
        this.ddControllerService.findAllDataDictionaryEntries(projectId, this.moduleData.id)
          .subscribe((response: DataDictionaryPojo[]) => {
            if (response.length) {
              this.dataDictionaryEntry = response.find(record => record.id === +routeParams.dataDictionaryEntryId);
            }
          });
      } else if (queryParams.length && queryParams.offset) {
        this.isNewEntry = true;
        const offset: number = queryParams.offset;
        const length: number = queryParams.length;
        this.ddControllerService.getFormatIfSelectionIsValid(projectId, this.moduleData.id, offset)
          .subscribe((response: DataFieldFormat) => {
            if (response !== null) {
              this.dataDictionaryEntry = {
                dataElementName: response['fieldName'],
                length: response.byteLength,
                format: response.languageType,
                location: { offset, length },
                isBusiness: false,
                fieldTransformation: '',
                sourceInput: '',
                targetOutput: '',
                scopes: {},
                usage: response.usage,
                definedLocation: response.definedLocation as DataDictionaryPojo.DefinedLocationEnum
              };
            }
          });
      }
    });
  }

  /**
   * Calls the java callback methods as per the form result emitted.
   * @param response event emitted by the Shared Data dictionary editor
   */
  handleFormResult(response: FormResponse<DataDictionaryPojo>): void {
    switch (response.result) {
      case FormResult.Saved:
        void java__callback__onSave();
        break;
      case FormResult.Canceled:
        void java__callback__onCancel();
        break;
      case FormResult.Deleted:
        void java__callback__onDelete();
        break;
    }
  }
}
