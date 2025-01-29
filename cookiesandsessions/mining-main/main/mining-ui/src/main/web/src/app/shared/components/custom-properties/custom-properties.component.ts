import { Component, Input, OnInit, TemplateRef, ViewChild } from '@angular/core';
import { Logger } from '@app/core';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { FieldTypeEnum, MiningTableConfig, MiningTableRow, ViewMode } from '@app/shared/components/mining-table/mining-table-config.interface';
import { TranslateService } from '@ngx-translate/core';
import { forkJoin } from 'rxjs';
import { PropertiesMetaModel } from '@app/shared/interfaces/custom-property-input.interface';
import { sortArrayBasedOnKeyAndDirection } from '@app/core/utils/sort.util';
import { MiningTableOptionSelected } from '../mining-table/mining-table-option-selected.interface';
import { FormResponse } from '@app/shared/interfaces/annotation-editor.interface';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { map } from 'rxjs/operators';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { AnnotationPojo, CustomPropertyMetadata, DataPointControllerService, MetamodelControllerService,
  MiningDataPointDefinitionWithPath, ProjectRole } from '@innowake/mining-api-angular-client';

const logger = new Logger('CustomPropertiesComponent');

@Component({
  selector: 'mn-custom-properties',
  templateUrl: './custom-properties.component.html'
})

export class CustomPropertiesComponent implements OnInit {

  @ViewChild('tooltipDisabledBtn') tooltipDisabledBtn: TemplateRef<void>;
  @Input() projectId: number;
  @Input() currentClient: ClientProjectRelationship;
  @Input() customPropertyClass: string;
  newRecord = false;
  tableConfig: MiningTableConfig = {
    columnMap: {
      position: {
        field: 'position',
        header: this.translateService.instant('customPropertyTable.position'),
        fieldType: FieldTypeEnum.NUMBER,
        widthColumn: '10%',
        sortOrder: 'ascend',
        options: []
      },
      title: {
        field: 'title',
        header: this.translateService.instant('customPropertyTable.title')
      },
      type: {
        field: 'type',
        header: this.translateService.instant('customPropertyTable.type')
      },
      customCategory: {
        field: 'customCategory',
        header: this.translateService.instant('customPropertyTable.customCatgory')
      },
      defaultValues: {
        field: 'defaultValues',
        header: this.translateService.instant('customPropertyTable.defaultValues'),
        displayAsCallback: this.defaultValuesDisplayAs,
        options: [
          {
            value: 'defaultValues',
            icon: 'info-circle',
            title: 'Project-wide Available for Selection',
            styleClass: 'ant-helper-secondary-text',
            disableItem: (): boolean => false
          }
        ]
      },
      mandatory: {
        field: 'mandatory',
        header: this.translateService.instant('customPropertyTable.mandatory')
      },
    },
    paginator: true,
    rows: 20,
    showTotalCount: true
  };
  totalRecords = 0;
  propertiesMetaModel: PropertiesMetaModel[] = [];
  customCategoryList = ['Add new'];
  selectedCustomProperty: PropertiesMetaModel;
  routerToLink: string;
  customPropertyClasses = {
    'Annotation': 'Annotation',
    'Module': 'Module',
    'DataDictionaryEntry': 'Data Dictionary'
  };
  toolTipEditor: string;
  isClientAdmin = false;

  constructor(
    public dataPointControllerService: DataPointControllerService,
    private graphQlControllerService: GraphQlControllerService,
    private metamodelService: MetamodelControllerService,
    private authorizationService: KeycloakAuthorizationService,
    private translateService: TranslateService
  ) { }

  ngOnInit(): void {
    this.tableConfig.columnMap.position.options.push({
      value: 'position',
      icon: 'info-circle',
      title: this.translateService.instant('customPropertyTable.editorToolTip', { editorName: this.customPropertyClasses[this.customPropertyClass] }),
      styleClass: 'ant-helper-secondary-text',
      disableItem: () => false
    });
    this.toolTipEditor = this.customPropertyClasses[this.customPropertyClass];
    if (this.customPropertyClass === 'DataDictionaryEntry') {
      this.routerToLink = RouteBuilder.buildProjectRoute(this.projectId, 'data-dictionary');
    } else {
      this.routerToLink = RouteBuilder.buildProjectRoute(this.projectId, this.customPropertyClass.toLocaleLowerCase() + 's');
    }
    this.tableConfig.loading = true;
    if (this.authorizationService.hasUserRole(this.currentClient, ProjectRole.UserRoleEnum.EDITOR)) {
      this.tableConfig.actions = [[{ value: 'editCustomProperties', icon: 'edit', prop: { type: 'text', size: 'small' }, disableItem: () => false }]];
    } else {
      this.tableConfig.actions = [];
    }
    this.isClientAdmin = this.authorizationService.isClientAdmin(this.currentClient.getClientId());
    this.fetchCustomProperties();
  }

  /**
   * the method tigger hide and show of the form.
   * @param value is the emmited value from the child.
   */
  setDrawerVisibilty(value: boolean): void {
    this.newRecord = value;
  }

  /**
   * the method allows to update the table.
   * @param value is the emmited value from the child.
   */
  updateTable(value: FormResponse<AnnotationPojo>): void {
    if (value) {
      this.fetchCustomProperties();
    }
  }

  /**
   * Opens the Custom property Editor on click of Add/edit options.
   * @param rowData user selected row for editing
   */
  openCustomPropertyEditor(rowData?: MiningTableOptionSelected): void {
    this.newRecord = true;
    this.selectedCustomProperty = rowData?.data;
  }

  private getDataPointsCategories(type: string, usage: string) {
    this.dataPointControllerService.getDataPointsForType(this.projectId, type, [usage])
      .subscribe((dataPoints: MiningDataPointDefinitionWithPath[]) => {
        dataPoints.forEach(def => {
          /* fix deserialization issue: JSON string[] is deserialized as string[] but not as Set<string> */
          if (new Set(def.providedBy).has('innowake.mining.server.graphql.config.CustomPropertyDataPointSource') && def.usageAttributes[usage]?.category  &&
          ! this.customCategoryList.includes(def.usageAttributes[usage]?.category)) {
              this.customCategoryList.push(def.usageAttributes[usage]?.category);
          }
        });
      });
  }

  private fetchCustomProperties() {
    this.graphQlControllerService.graphQl({
      query: 'query ($projectId: EntityId!) { project(projectId: $projectId) { customPropertyClasses } }',
      variables: { projectId: this.projectId }
    }).subscribe(
      (response: { [key: string]: any }) => {
        if (response.data) {
          const customPropertyClasses: string[] = response.data.project.customPropertyClasses[this.customPropertyClass];
          forkJoin(customPropertyClasses?.map(className => this.metamodelService.findMetaModel(className)))
            .pipe(map(response => response.map((metaModels, i) => (
              metaModels.map(metaModel => ({
                ...metaModel,
                className: customPropertyClasses[i]
              }))
            )))).subscribe(res => {
              if (res && res.length) {
                const properties: PropertiesMetaModel[] = [];
                res.forEach((metaModels: CustomPropertyMetadata[]) => {
                  this.totalRecords = metaModels.length;
                  const sortedMetaModels: CustomPropertyMetadata[] = sortArrayBasedOnKeyAndDirection(metaModels, 'position', 'ASC', 'number');
                  sortedMetaModels.forEach((metaModel: CustomPropertyMetadata) => {
                    const customPropertyName = `${this.customPropertyClass}CustomProperties${this.projectId}`;
                    const prop: PropertiesMetaModel = {
                      id: properties.length,
                      position: metaModel.customViewIndex !== 0 ? metaModel.customViewIndex : this.translateService.instant('notAvailable'),
                      title: metaModel.label,
                      name: metaModel.name,
                      type: this.checkDataType(metaModel.fieldType, metaModel.dataType),
                      tags: metaModel.autoCompletionKey,
                      customCategory: metaModel.customCategory,
                      defaultValues: this.translateService.instant('notAvailable'),
                      mandatory: metaModel.mandatory
                    };
                    if (metaModel['className'] !== customPropertyName) {
                      prop.disabledActions = ['editCustomProperties'];
                      prop.buttonToolTip = this.tooltipDisabledBtn;
                    }
                    properties.push(prop);
                  });
                });
                if (properties.length) {
                  this.assignTagValues(properties);
                } else {
                  this.propertiesMetaModel = properties;
                  this.tableConfig.loading = false;
                }
              }
            }, () => {
              this.tableConfig.loading = false;
              logger.error('Error while loading custom annotation properties from server');
            });
        }
      },
      (error: any) => {
        this.tableConfig.loading = false;
        logger.error('Error while loading custom annotation properties from server', error);
      }
    );
    if (this.customPropertyClass === 'Annotation') {
      this.getDataPointsCategories(TypeName.PAGEANNOTATION, Usages.ANNOTATIONTABLE);
    } else if (this.customPropertyClass === 'DataDictionaryEntry') {
      this.getDataPointsCategories(TypeName.PAGEDATADICTIONARY, Usages.DATADICTIONARYTABLE);
    } else if (this.customPropertyClass === 'Module') {
      this.getDataPointsCategories(TypeName.PAGEMODULE, Usages.MODULETABLE);
    }
  }

  private assignTagValues(properties: PropertiesMetaModel[]): void {
    this.graphQlControllerService.graphQl({
      query: 'query ($projectId: EntityId!) { project(projectId: $projectId) { autoCompletionMap } }',
      variables: { projectId: this.projectId }
    }, this.projectId).subscribe(
      (response: { [key: string]: any }) => {
        const autoCompletions = response.data.project?.autoCompletionMap;
        if (autoCompletions) {
          Object.keys(autoCompletions as object).forEach(key => {
            let indexOfKey = properties.findIndex(x => x.tags === key);
            if (indexOfKey !== -1) {
              const autoCompletionValue = autoCompletions[key].filter((e: any) => e);
              properties[indexOfKey].defaultValues = autoCompletions[key].length > 0 && autoCompletionValue.length > 0 ?
                autoCompletions[key] : this.translateService.instant('notAvailable');
            }

            indexOfKey = properties.findIndex(x => x.defaultValueKey === key);
            if (indexOfKey !== -1) {
              const defaultValue = autoCompletions[key];
              properties[indexOfKey].defaultValues = autoCompletions[key].length > 0 && defaultValue.length > 0 ?
                autoCompletions[key][0] : this.translateService.instant('notAvailable');
            }
          });
        }
        this.propertiesMetaModel = properties;
        this.tableConfig.loading = false;
      },
      (error: any) => {
        this.tableConfig.loading = false;
        logger.error('Error while loading auto-completion values from server', error);
      }
    );
  }

  private checkDataType(fieldType: string, dataType: string): string {
    if (fieldType === CustomPropertyMetadata.FieldTypeEnum.TAG) {
      return this.translateService.instant('customPropertyDataTypes.selectTag');
    }
    if (fieldType === CustomPropertyMetadata.FieldTypeEnum.URL) {
      return this.translateService.instant('customPropertyDataTypes.url');
    }

    switch (dataType) {
      case CustomPropertyMetadata.DataTypeEnum.STRING:
        return this.translateService.instant('customPropertyDataTypes.inputString');
      case CustomPropertyMetadata.DataTypeEnum.EMBEDDEDLIST:
        return this.translateService.instant('customPropertyDataTypes.stringRepeater');
      default:
        return dataType;
    }
  }

  private defaultValuesDisplayAs(row: MiningTableRow): ViewMode {
    if (row.type === CustomPropertyMetadata.FieldTypeEnum.URL) {
      return ViewMode.EXTERNALLINK;
    }
    return ViewMode.TAG;
  }
}
