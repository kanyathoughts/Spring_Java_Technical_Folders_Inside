import { Component, EventEmitter, Inject, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { ModuleControllerService, ModulePojo, ModuleRelationshipPojo, ReferenceControllerService } from '@innowake/mining-api-angular-client';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { WindowToken } from '@app/core/utils/window';
import { DepedendencyGraphEdgeMetaData, InputForArtificialEdge, RelationshipTableData } from './graph-edge-meta-data.interface';
import { EllipsisPipe } from '@app/shared/pipes/ellipsis-pipe';
import { ModuleDetailsDPSidePanelService } from '@app/core/services/module-details-and-dp-panel-state.service';
import { EdgeLabelPanelState } from '@app/shared/components/shared-module-details/dependency-graph-panel-state.interface';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { Subscription, forkJoin } from 'rxjs';

@Component({
  selector: 'app-graph-edge-meta-data',
  templateUrl: './graph-edge-meta-data.component.html'
})
export class GraphEdgeMetaDataComponent implements OnDestroy, OnInit {
  @Input() inputForEdgeMetadata: DepedendencyGraphEdgeMetaData[];
  @Input() inputForArtificialEdgeMetadata: InputForArtificialEdge[];
  @Input() isDPGraphFromCallChain = false;
  @Input() projectId: number;
  @Input() isArtificialEdge: boolean;
  @Input() relationShipType: string;
  @Input() fromId: number;
  @Input() toId: number;
  @Input() edgeFileAccessType: string;
  @Output() closeEdgeSidePanel: EventEmitter<boolean> = new EventEmitter();
  maintainSidePanelState = {
    module: false,
    relationship: false,
    reference: false
  };
  relationshipData: RelationshipTableData[];
  metaDataLoaded: boolean;
  isGroupedNode: boolean;
  private jobManagerSubscription: Subscription;
  private referenceControllerSubscription: Subscription;
  private combinedSubscription: Subscription[] = [];

  constructor(
    private referenceControllerService: ReferenceControllerService,
    private moduleDetailsDPSidePanelService: ModuleDetailsDPSidePanelService,
    private labelMappingService: LabelMappingService,
    private jobManagerService: JobManagerService,
    private moduleControllerService: ModuleControllerService,
    @Inject(WindowToken) private $window: Window
  ) { }

  ngOnInit(): void {
    if (this.isArtificialEdge) {
      this.metaDataLoaded = true;
      this.jobManagerService.subscribeToJobNotification();
      this.jobManagerSubscription = this.jobManagerService.jobResult.subscribe((result: any) => {
        if (result && result['modules'] && result['references']) {
          this.inputForEdgeMetadata = this.getEdgeMetaDataProperties(result['modules'] as ModulePojo[]);
          const moduleKeyValuePairs = {};
          result['modules'].forEach((obj: ModulePojo) => {
            moduleKeyValuePairs[obj.uid] = obj;
          });
          const references: {[key: string]: RelationshipTableData;} = {};
          result['references'].forEach((referenceItem: ModuleRelationshipPojo) => {
            const key = referenceItem.srcModule + referenceItem.relationship + referenceItem.dstModule;
            if ( ! references[key]) {
              let property = {};
              if (referenceItem['properties'] && Object.keys(referenceItem['properties'] as object).length) {
                const relationshipProperties = {};
                relationshipProperties[`${this.labelMappingService.mapLabel(LabelType.RELATIONSHIP,
                  referenceItem.relationship as string)} ${1}`] = referenceItem['properties'];
                property = relationshipProperties;
              }

              const referenceObj: RelationshipTableData = {
                fromNode: {
                  name: moduleKeyValuePairs[referenceItem.srcModule].name,
                  id: referenceItem.srcModule,
                  moduleShortName: new EllipsisPipe().transform(moduleKeyValuePairs[referenceItem.srcModule].name as string, 8)
                },
                toNode: {
                  name: moduleKeyValuePairs[referenceItem.dstModule].name,
                  id: referenceItem.dstModule,
                  moduleShortName: new EllipsisPipe().transform(moduleKeyValuePairs[referenceItem.dstModule].name as string, 8)
                },
                reference: this.labelMappingService.mapLabel(LabelType.RELATIONSHIP, referenceItem?.relationship as string),
                properties: property
              };
              references[key] = referenceObj;
            } else {
              const referenceObj = references[key];
              const keys = Object.keys(referenceObj.properties);
              if (referenceItem['properties'] && Object.keys(referenceItem['properties'] as object).length) {
                referenceObj.properties[`${this.labelMappingService.mapLabel(LabelType.RELATIONSHIP,
                  referenceItem.relationship as string)} ${keys.length + 1}`] = referenceItem['properties'];
              }
            }
          });
          this.relationshipData = Object.values(references);
          this.metaDataLoaded = false;
        }
      });
    }
    this.combinedSubscription.push(this.jobManagerSubscription);
  }

  /**
   * Method to get data for Edge to create isntance in DP component
   */
  getDataForEdgeModule(): void {
    this.isGroupedNode = this.inputForEdgeMetadata
    .some((inputForEdgeMetadata: DepedendencyGraphEdgeMetaData) =>
    ! inputForEdgeMetadata.technology && ! inputForEdgeMetadata.type);
    this.maintainSidePanelState = this.moduleDetailsDPSidePanelService.getPanelState('edgePanelState') as EdgeLabelPanelState;
    if ( ! this.isGroupedNode) {
      if ( ! this.isArtificialEdge) {
        this.metaDataLoaded = false;
        this.referenceControllerSubscription = this.referenceControllerService.findAllByFromAndToModuleIds(this.projectId,
          this.fromId, this.toId, this.relationShipType as ModuleRelationshipPojo.RelationshipEnum).subscribe((response: ModuleRelationshipPojo[]) => {
            if (response && response.length > 0) {
              const moduleRelationship = response;
              const relationshipProperties = {};
              moduleRelationship.forEach((item, index: number) => {
                if (Object.keys(item['properties']).length) {
                  const label = this.labelMappingService.mapLabel(LabelType.RELATIONSHIP, this.getLabelKey(item));
                  relationshipProperties[`${label} ${index + 1}`] = item['properties'];
                }
              });
              const srcModuleReq = this.moduleControllerService.findModuleById(this.projectId, moduleRelationship[0]?.srcModule);
              const dstModuleReq = this.moduleControllerService.findModuleById(this.projectId, moduleRelationship[0]?.dstModule);
              forkJoin([srcModuleReq, dstModuleReq]).subscribe(([srcModule, dstModule]) => {
                const moduleForName = new EllipsisPipe().transform(srcModule.name, 8);
                const moduleToName = new EllipsisPipe().transform(dstModule.name, 8);
                this.relationshipData = [{
                  reference: this.labelMappingService.mapLabel(LabelType.RELATIONSHIP, this.getLabelKey(moduleRelationship[0])),
                  fromNode: { name: srcModule.name, id: srcModule.id, moduleShortName: moduleForName },
                  toNode: { name: dstModule.name, id: dstModule.id, moduleShortName: moduleToName },
                  properties: relationshipProperties,
                }];
              });
            }
          });
        this.combinedSubscription.push(this.referenceControllerSubscription);
      } else {
        this.metaDataLoaded = true;
        const requestObj = this.buildRequestObject(this.inputForArtificialEdgeMetadata);
        this.jobManagerService.jobExtension('ARTIFICIAL_EDGE', this.projectId, requestObj);
      }
    }
  }

  /**
   * Method to open module details in new tab
   */
  buildRouteForModule(moduleId: number): void {
    openInNewTab(this.projectId, moduleId, 'details/overview', this.$window);
  }

  /**
   * method to toggle section of edge side panel
   * @param event:click event
   * @param value: which section is clicked
   */
  toggleDropDown(event: Event, value: string): void {
    event.stopPropagation();
    this.maintainSidePanelState[value] = ! this.maintainSidePanelState[value];
  }

  /**
   * Method to close the side panel
   */
  closePanel(): void {
   this.closeEdgeSidePanel.emit();
  }

  ngOnDestroy(): void {
    this.moduleDetailsDPSidePanelService.setPanelState({ module: false, relationship: false, reference: false }, 'edgePanelState');
    this.combinedSubscription.forEach(subscription => subscription?.unsubscribe());
  }

  private buildRequestObject(inputForArtificialEdgeMetadata: InputForArtificialEdge[]): {[key: string]: string[]} {
    const requestObject = {};
    requestObject['direction'] = ['OUT', 'IN'];
    requestObject['considerConditionalDependencies'] = [false];
    requestObject['dataAccessBased'] = [false];
    requestObject['compressed'] = ['false'];
    requestObject['ignoredTaxonomy'] = [];
    requestObject['exportFormat'] = ['DEPENDENCY_GRAPH'];
    requestObject['depth'] = inputForArtificialEdgeMetadata[0]['depth'];
    requestObject['startModuleId'] = inputForArtificialEdgeMetadata[0]['startModule'];
    requestObject['endModuleId'] = inputForArtificialEdgeMetadata[0]['endModule'];
    return requestObject;
  }

  private getEdgeMetaDataProperties(modules: ModulePojo[]): DepedendencyGraphEdgeMetaData[] {
    return modules.map((moduleItem: ModulePojo) => ({
      type: moduleItem.type,
      technology: moduleItem.technology,
      module: moduleItem.name,
      moduleId: moduleItem.id,
      shortName: new EllipsisPipe().transform(moduleItem.name, 8)
    }));
  }

  private getLabelKey(details: ModuleRelationshipPojo): string {
    return details.relationship === 'ACCESSES' && ! this.isDPGraphFromCallChain ? this.edgeFileAccessType : details.relationship;
  }
}
