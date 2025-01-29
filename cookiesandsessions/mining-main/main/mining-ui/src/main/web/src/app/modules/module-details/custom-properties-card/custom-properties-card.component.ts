import { Component, Input, OnChanges, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { CustomPropertiesService } from '@app/core/services/custom-properties/custom-properties.service';
import { CustomPropertyDetails, CustomPropertyInput } from '@app/shared/interfaces/custom-property-input.interface';
import { Logger } from '@app/core';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { TranslateService } from '@ngx-translate/core';
import { CustomPropertyFieldListComponent } from
  '@app/shared/components/custom-property-editor/custom-property-field-list/custom-property-field-list.component';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { CustomPropertyFormComponent } from '@app/shared/components/custom-property-form/custom-property-form.component';
import { CustomPropertyMetadata, ModulePojo, ProjectPojoCustomProperties } from '@innowake/mining-api-angular-client';
import { ViewMode } from '@app/shared/components/mining-table/mining-table-config.interface';

const logger = new Logger('CustomPropertiesCardComponent');

@Component({
  selector: 'custom-properties-card',
  templateUrl: './custom-properties-card.component.html'
})
export class CustomPropertiesCardComponent implements OnInit, OnChanges, OnDestroy {

  @ViewChild(CustomPropertyFieldListComponent) customPropertyList: CustomPropertyFieldListComponent;
  @Input() customProperties: { [key: string]: ProjectPojoCustomProperties[]; } = {};
  @Input() currentClient: ClientProjectRelationship;
  @Input() selectedModule: ModulePojo;
  moduleCustomProperties: CustomPropertyInput[] = [];
  customPropertiesDetails: CustomPropertyDetails[] = [];
  private modalInstance: NzModalRef;

  constructor(
    private customPropertiesService: CustomPropertiesService,
    public authorizationService: KeycloakAuthorizationService,
    private translateService: TranslateService,
    private modalService: NzModalService
    ) {
  }

  ngOnInit(): void {
    this.customPropertiesService.getCustomPropertiesMetadataForClass('Module', this.currentClient.getProjectId()).subscribe((res) => {
      if (res) {
        this.moduleCustomProperties.push(...res);
        this.setCustomPropertiesDetails();
      }
    }, (error: any) => {
      logger.error('Error while loading custom module properties from server', error);
    });
  };

  ngOnChanges(): void {
    if (this.moduleCustomProperties.length) {
      this.setCustomPropertiesDetails();
    }
  }

  ngOnDestroy(): void {
    this.modalInstance?.destroy();
  }

  /**
   * Opens a modal window to edit custom property details.
   */
  editCustomProperties(): void {
    this.modalInstance = this.modalService.create<CustomPropertyFormComponent>({
      nzTitle: this.translateService.instant('editCustomProperties'),
      nzClosable: true,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzContent: CustomPropertyFormComponent,
    });
    const instance = this.modalInstance.getContentComponent();
    instance.customProperties = this.customProperties;
    instance.currentClient = this.currentClient;
    instance.selectedModule = this.selectedModule;
    this.modalInstance.afterClose.subscribe((result: string) => {
      if ( ! result || result === 'cancel') {
        return;
      }
      this.setCustomPropertiesDetails();
    });
  }

  getDisplayAs(fieldType: CustomPropertyMetadata.FieldTypeEnum): ViewMode {
    if (fieldType === CustomPropertyMetadata.FieldTypeEnum.URL) {
      return ViewMode.EXTERNALLINK;
    } else if (fieldType === CustomPropertyMetadata.FieldTypeEnum.TAG) {
      return ViewMode.TAG;
    }
  }

  renderAsLink(value: CustomPropertyMetadata.FieldTypeEnum): boolean {
    return value === CustomPropertyMetadata.FieldTypeEnum.URL;
  }

  private setCustomPropertiesDetails() {
    this.customPropertiesDetails = [];
    this.moduleCustomProperties.forEach(moduleCpDetails => {
      const customProperty = this.customProperties[moduleCpDetails.customPropertyClassName]?.[moduleCpDetails.name];
      if (customProperty) {
        this.customPropertiesDetails.push({
          value: customProperty,
          label: moduleCpDetails.label,
          fieldType: moduleCpDetails.fieldType,
          customViewIndex: moduleCpDetails.customViewIndex
        });
      }
    });
    this.customPropertiesDetails = this.customPropertiesDetails.length ? this.customPropertiesDetails.filter(x => (x.value !== null && x.value !== '')) : [];
    // We need to convert the string containing arrays for display only because forms are handling the string value
    this.customPropertiesDetails.forEach(detail => {
      if (detail.value?.indexOf('[') > -1) {
        detail.value = JSON.parse(detail.value as string);
      }
    });
  }

}
