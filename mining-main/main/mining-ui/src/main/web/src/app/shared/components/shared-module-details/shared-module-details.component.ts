import { HttpErrorResponse } from '@angular/common/http';
import { Component, EventEmitter, Inject, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Logger } from '@app/core';
import { CustomPropertiesService } from '@app/core/services/custom-properties/custom-properties.service';
import { DeepLinkService } from '@app/core/services/deep-link.service';
import { ScrollDirection, ScrollEventService } from '@app/core/services/scroll-event/scroll-event.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { getTaxomomyTreeNode } from '@app/core/utils/taxonomy.utils';
import { WindowToken } from '@app/core/utils/window';
import { ModuleComplexityDetails } from '@app/modules/module-details/models/module-complexity-details';
import { CustomPropertyDetails, CustomPropertyInput } from '@app/shared/interfaces/custom-property-input.interface';
import { ModuleDetailPanelState } from './dependency-graph-panel-state.interface';
import { TaxonomyList } from '@app/shared/interfaces/taxonomy-list.interface';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';
import { ModuleDetailsDPSidePanelService } from '../../../core/services/module-details-and-dp-panel-state.service';
import { calculateModuleComplexity } from './shared-module-details.util';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { AssignTaxonomiesComponent } from '../assign-taxonomies/assign-taxonomies.component';
import { AssignTaxonomiesModalService } from '../assign-taxonomies/assign-taxonomies-modal.service';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { CustomPropertyFormComponent } from '../custom-property-form/custom-property-form.component';
import { EclipseService } from '@app/core/authentication/eclipse.service';
import {
  AnnotationPojo,
  CustomPropertyMetadata,
  EntityId,
  ModuleControllerService,
  ModulePojo,
  ProjectPojoCustomProperties,
  ProjectRole,
  TaxonomyAssignmentsGetResponse,
  TaxonomyControllerService,
  TaxonomyPojo
} from '@innowake/mining-api-angular-client';
import { DataLineageNode } from '@app/modules/module-data-lineage/models/data-lineage.node';
const log = new Logger('AnnotationEditModal');

@Component({
  selector: 'app-shared-module-details',
  templateUrl: './shared-module-details.component.html',
})

export class SharedModuleDetailsComponent implements OnInit, OnDestroy {

  @Input() moduleDetails: ModulePojo;
  @Input() projectId: number;
  @Input() selectedItem: any;
  @Input() isDPGraphFromCallChain = false;
  @Input() scrollPosition: ScrollDirection;
  @Input() fromDL: boolean;
  @Input() highlightedNodes: DataLineageNode[];
  @Output() hideShowTab: EventEmitter<boolean> = new EventEmitter();
  @Output() invokeHighlight: EventEmitter<string> = new EventEmitter<string>();
  @Output() closeModal: EventEmitter<any> = new EventEmitter();
  showHide = '';
  modulePath: string;
  extraTpl: number;
  scrollEventSubscription: Subscription;
  showBarElement: any = [];
  moduleComplexity: ModuleComplexityDetails;
  currentClient: ClientProjectRelationship;
  taxonomyList: TaxonomyList[] = [];
  taxonomyTotalNumber: number;
  annotationLink: string;
  moduleTitle: string;
  isEditor: boolean;
  taxonomyResponse: TaxonomyAssignmentsGetResponse;
  customPropertiesDetails: CustomPropertyDetails[] = [];
  customProperties: ProjectPojoCustomProperties = {};
  moduleCustomProperties: CustomPropertyInput[] = [];
  moduleId: EntityId;
  showDropDown = {
    characteristics: false,
    metrics: false,
    description: false,
    taxonomy: false,
    annotations: false,
    customProperty: false
  };
  maintainSidePanelState: ModuleDetailPanelState = this.showDropDown;
  numberPerAnnoationType  =  {};
  isEclipseLinkAvailable = this.deeplinkService.featureIsActive();

  icons = {
    name: this.translateService.instant('iconToolTip.moduleDetails'),
    routePath: 'details/overview',
    icon: 'graph-context-menu:icon-OpenModuleDetails'
  };
  isEclipseView = false;
  private modalInstance: NzModalRef;
  private assignTaxonomiesSubscription: Subscription;
  private clientProjectSubscription: Subscription;
  private combinedSubscription: Subscription[] = [];

  constructor(private scrollEventService: ScrollEventService, private moduleControllerService: ModuleControllerService,
    private taxonomyControllerService: TaxonomyControllerService,
    @Inject(WindowToken) private $window: Window,
    private translateService: TranslateService,
    public formateNumber: NumberFormatter,
    private customPropertiesService: CustomPropertiesService,
    private modalService: NzModalService,
    private authorizationService: KeycloakAuthorizationService,
    private relationshipService: ClientProjectRelationshipService,
    private assignTaxonomiesModalService: AssignTaxonomiesModalService,
    private deeplinkService: DeepLinkService,
    private moduleDetailsDPSidePanelService: ModuleDetailsDPSidePanelService,
    private messageService: NzMessageService,
    private eclipseService: EclipseService,
    ) { }

  ngOnInit(): void {
    this.isEclipseView = this.eclipseService.isEclipseView;
    this.scrollEventSubscription = this.scrollEventService.getScrollObservable().subscribe((scrollPosition) => {
      if (scrollPosition === ScrollDirection.DOWN) {
        this.showHide = 'shared-side-viewer__drawer-editor-header-' + scrollPosition;
      } else {
        this.showHide = 'custom-property-editor__' + scrollPosition;
      }
    });

    if (this.isDPGraphFromCallChain) {
      this.showHide = this.showHide + ' graph-global-style--call-chain';
    }

    this.assignTaxonomiesModalService.getloadTaxonomySubject().subscribe((response: boolean) => {
      if (response) {
      this.getAssignedTaxonomyByModule();
      }
    });
    this.assignTaxonomiesSubscription = this.assignTaxonomiesModalService.closeModal.subscribe((resp: boolean) => {
      if (resp) {
        this.modalInstance.close();
      }
    });
    this.combinedSubscription.push(this.assignTaxonomiesSubscription, this.scrollEventSubscription);
  }

  /**
   * Method to get Data for shared module to create instance in DP component
   */
  getDataForSharedModule(): void {
    if (this.scrollPosition === ScrollDirection.DOWN) {
      this.showHide = 'shared-side-viewer__drawer-editor-header-' + this.scrollPosition;
    }
    if (this.selectedItem) {
      this.invokeHighlight.emit('highlight');
    }
    if (this.moduleId !== this.moduleDetails.id) {
      this.showBarElement = [];
      this.taxonomyTotalNumber = 0;
      this.showDropDown = this.moduleDetailsDPSidePanelService.getPanelState('nodePanelState') as ModuleDetailPanelState;
      this.moduleTitle = this.moduleDetails.name;
      this.annotationLink = RouteBuilder.buildDetailsRoute(this.projectId, this.moduleDetails?.linkHash, 'annotations');
      this.moduleId = this.moduleDetails.id;
      this.customProperties = this.moduleDetails.customProperties;
      this.clientProjectSubscription = this.relationshipService.getClientProjectObservable().subscribe(currentClient => {
        this.currentClient = currentClient;
        this.isEditor = this.authorizationService.hasUserRole(currentClient, ProjectRole.UserRoleEnum.EDITOR);
      });
      this.moduleControllerService.findModuleById(this.projectId, this.moduleId).subscribe((response: ModulePojo) => {
        this.moduleComplexity = calculateModuleComplexity(response);
        if (this.moduleComplexity === undefined) {
          this.moduleComplexity = { description: 'notAvailable' };
        }
        this.moduleComplexity.description = this.translateService.instant(this.moduleComplexity.description);
        this.modulePath = response.path;
        this.customProperties = response.customProperties;
        if (response) {
          const characteristics = {
            characteristics: [{
              name: this.translateService.instant('baseData'),
              type: response.type ? response.type : this.translateService.instant('notAvailable'),
              technology: response.technology ? response.technology : this.translateService.instant('notAvailable')
            }],
            description: [{
              name: this.translateService.instant('description'),
              description: (response.description !== null && response.description !== '') ? response.description : this.translateService.instant('notAvailable')
            }],
            metrics: [{
              name: this.translateService.instant('metrics.menuItem'),
              codeLines: response.sourceMetrics?.codeLines,
              commentLines: response.sourceMetrics?.commentLines,
            }],
            taxonomy: [{
              name: this.translateService.instant('taxonomies'),
            }],
            annotation: [{
              name: this.translateService.instant('annotationReporting.label'),
            }],
            customProperty: [{
              name: this.translateService.instant('customProperties')
            }]
          };
          this.showBarElement.push(characteristics);
        }
        this.getAssignedTaxonomyByModule();
        this.getCustomPropertiesMetadataForClass();
        this.findModuleAnnotation();
      }, (err: HttpErrorResponse) => {
        log.error(err);
      }
      );
    }
    this.combinedSubscription.push(this.clientProjectSubscription);
  }

  ngOnDestroy(): void {
    this.combinedSubscription.forEach(subscription => subscription?.unsubscribe());
    this.modalInstance?.destroy();
    this.moduleDetailsDPSidePanelService.setPanelState({
      characteristics: false,
      metrics: false,
      description: false,
      taxonomy: false,
      annotations: false,
      customProperty: false
    }, 'nodePanelState');
  }

  openInEclipse(): void {
    const module: ModulePojo = {
      projectId: this.projectId,
      path: this.modulePath
    };
    this.deeplinkService.showModuleInEclipse(module);
  }

  /**
   * Opens Modal for editing module description.
   * @event the mouse event.
   */
  openDescriptionModal(event: MouseEvent): void {
    event.preventDefault();
    const updateModuleDesc: () => ModuleDetailsDPSidePanelService = this.updateModuleDesc.bind(this);
    this.moduleDetailsDPSidePanelService.editModuleDescription(this.moduleDetails, updateModuleDesc);
  }

  /**
   * Opens modal for assigning taxonomy.
   */
  editTaxonomyDetails(): void {
    this.modalInstance?.destroy();
    this.modalInstance = this.getModalInstance(
      AssignTaxonomiesComponent,
      this.translateService.instant('assignTaxonomies.modalTitle',
      { moduleName: this.moduleDetails.name }) as string
    );
    const instance = this.modalInstance.getContentComponent();
    instance.taxonomyResponse = this.taxonomyResponse;
    instance.moduleIdArray = [this.moduleId];
    instance.projectId = this.projectId;
  }

  /**
   * Get the annotation details
   */
  findModuleAnnotation(): void {
    this.numberPerAnnoationType = {};
    this.moduleControllerService.findAnnotationsForModule(this.projectId, this.moduleId).subscribe((response: AnnotationPojo[]) => {
      response.forEach(element => {
        if ( ! this.numberPerAnnoationType[element.type]) {
          this.numberPerAnnoationType[element.type] = 1;
        } else {
          this.numberPerAnnoationType[element.type] = this.numberPerAnnoationType[element.type] + 1;
        }
      });
    }, (error: HttpErrorResponse) => {
      log.error(error);
    });
  }

  /**
   * Get the taxonomy details
   */
  getAssignedTaxonomyByModule(): void {
    this.taxonomyControllerService.getAssignedTaxonomyByModule(this.projectId, { 'modules': { 'ids': [this.moduleId] } })
      .subscribe((taxonomyAssignmentResponse: TaxonomyAssignmentsGetResponse) => {
        if (taxonomyAssignmentResponse?.taxonomies?.length) {
          const taxonomyData: TaxonomyPojo[] = [];
          this.taxonomyTotalNumber = 0;
          taxonomyAssignmentResponse.taxonomies.forEach((response) => {
            if (response.state === 'ALL' || response.state === 'SOME') {
              taxonomyData.push(response.taxonomy);
            }
          });
          this.taxonomyList = getTaxomomyTreeNode(taxonomyData);
          this.taxonomyList.forEach(element => {
            element.type.forEach(element => {
              this.taxonomyTotalNumber += element.taxonomies?.length;
            });
          });
          this.taxonomyResponse = taxonomyAssignmentResponse;
        }
      });
  }

  /**
   * Opens a modal window to edit custom property details.
   */
  editCustomProperties(): void {
    this.modalInstance?.destroy();
    this.modalInstance = this.getModalInstance(
      CustomPropertyFormComponent,
      this.translateService.instant('editCustomProperties') as string
    );
    const instance = this.modalInstance.getContentComponent();
    instance.customProperties = this.customProperties;
    instance.currentClient = this.currentClient;
    instance.selectedModule = this.moduleDetails;
    instance.className = 'Module';
    this.modalInstance.afterClose.subscribe((result: string) => {
      if ( ! result || result === 'cancel') {
        return;
      }
      this.setCustomPropertiesDetails();
      });
  }

  /**
   * close the form
   */
  closeEditDetails(): void {
    this.hideShowTab.emit(false);
  }

  /**
   * Captures the event emitted by the contextual Toolbar component
   * and redirects to the code viewer or module details in new tab.
   * @param path route to be navigated to.
   */
  openInNewBrowserTab(path: string): void {
    openInNewTab(this.projectId, this.moduleDetails.id, path, this.$window);
  }

  /**
   * Keeps the drops down open or close
   * @param value is the type of dropdown
   */
  toggelDropDown(value: string): void {
    if ( ! this.maintainSidePanelState[value]) {
      this.maintainSidePanelState[value] = true;
    } else {
      this.maintainSidePanelState[value] = false;
    }
    this.moduleDetailsDPSidePanelService.setPanelState(this.maintainSidePanelState, 'nodePanelState');
  }

  private updateModuleDesc(updatedParams: {[key: string]: any}): void {
    const oldDescription = this.moduleDetails.description;
    this.moduleDetails.description = updatedParams.Description;
    this.moduleControllerService.updateModule(this.projectId, this.moduleId, this.moduleDetails).subscribe(() => {
      this.messageService.success(`${this.translateService.instant('updateSuccessful')}`);
      this.showBarElement[0].description[0].description = updatedParams.Description !== '' ?
      updatedParams.Description : this.translateService.instant('notAvailable');
      this.moduleDetailsDPSidePanelService.descriptionModal.destroy();
    }, error => {
      this.moduleDetails.description = oldDescription;
      this.messageService.error(`${this.translateService.instant('updateError')}`);
      log.error(error.message);
    });
  }

  /**
   * This calls the custom property data
   */
  private getCustomPropertiesMetadataForClass(): void {
    this.moduleCustomProperties = [];
    this.customPropertiesService.getCustomPropertiesMetadataForClass('Module', this.projectId).subscribe((res) => {
      if (res) {
        this.moduleCustomProperties.push(...res);
        this.setCustomPropertiesDetails();
      }
    });
  }

  private setCustomPropertiesDetails() {
    this.customPropertiesDetails = [];
    this.moduleCustomProperties.forEach(moduleCpDetails => {
      const moduleCustomProperties = this.customProperties[moduleCpDetails.customPropertyClassName];
      const customProperty = moduleCustomProperties ?
        Object.entries(moduleCustomProperties as CustomPropertyMetadata).find(([key]) => moduleCpDetails.name === key) : null;
      if (customProperty) {
        this.customPropertiesDetails.push({
          ...customProperty,
          label: moduleCpDetails.label,
          fieldType: moduleCpDetails.fieldType,
          value: customProperty[1].toString(),
          customViewIndex: moduleCpDetails.customViewIndex
        });
      }
    });
    this.customPropertiesDetails = this.customPropertiesDetails.length ? this.customPropertiesDetails.filter(x => (x.value !== null && x.value !== '')) : [];
    // We need to convert the string containing arrays for display only because forms are handling the string value
    this.customPropertiesDetails.forEach(detail => {
      if (detail.value?.indexOf('[') > -1) {
        try {
          detail.value = JSON.parse(detail.value as string);
        } catch (error) {
          log.error('Error parsing JSON:' + error);
        }
      }
    });
  }

  /**
   * Creates the modal service instance.
   * @param modalData data sent to the modal.
   * @param component component for the opened modal.
   * @param title title for the modal.
   * @returns reference of the modal.
   */
  private getModalInstance(component: any, title: string): NzModalRef {
    return this.modalService.create({
      nzTitle: title,
      nzClosable: true,
      nzMaskClosable: false,
      nzKeyboard: true,
      nzAutofocus: null,
      nzContent: component
    });
  }
}
