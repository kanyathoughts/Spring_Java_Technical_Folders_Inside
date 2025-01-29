import { ChangeDetectorRef, Component, Inject, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BulkAction, Column, DEFAULT_NUMBER_OF_ROWS } from '@app/shared/components/mining-table/mining-table-config.interface';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { BaseReportingPageComponent } from '@app/shared/components/base-customizable-table/base-reporting-page.component';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalService } from 'ng-zorro-antd/modal';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { SharedAnnotationEditorService } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.service';
import { WindowToken } from '@app/core/utils/window';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { DataPointControllerService, MiningDataPointDefinitionWithPath,
SavedSearchControllerService, TaxonomyAssignmentsGetResponse,
TaxonomyControllerService } from '@innowake/mining-api-angular-client';
import { CustomizableTableCoreComponent } from '@app/shared/components/customizable-table-core/customizable-table-core.component';
import { ReachabilityService } from '../utils/reachability.service';
import { AssignTaxonomiesComponent } from '@app/shared/components/assign-taxonomies/assign-taxonomies.component';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { StatesType } from '@app/shared/components/mining-table/mining-table-action.interface';

const upperBoundName = 'upperBoundModuleName';
const upperBoundTechnology = 'upperBoundModuleTechnology';
const upperBoundType = 'upperBoundModuleType';
const upperBoundPath = 'content.upperBoundModuleName.id';

@Component({
  selector: 'app-block-details',
  templateUrl: '../../../shared/components/base-customizable-table/base-reporting-page.component.html'
})
export class BlockDetailsComponent extends BaseReportingPageComponent implements OnInit {

  @ViewChild(CustomizableTableCoreComponent) customTable: CustomizableTableCoreComponent;
  graphQlType = 'reachabilityData';
  usage: Usages = Usages.REACHABILITYTABLE;
  pageType = TypeName.PAGEREACHABILITY;
  blockId: string[];
  moduleIdArray: number[];
  tableActions: BulkAction[] = [];
  taxonomyResponse: TaxonomyAssignmentsGetResponse;

  constructor(
    public clientProjectRelationshipService: ClientProjectRelationshipService,
    public router: Router,
    public graphQlControllerService: GraphQlControllerService,
    public messageService: NzMessageService,
    public userCustomizableTable: CustomizableTableColumnService,
    public route: ActivatedRoute,
    public dataPointControllerService: DataPointControllerService,
    public savedSearchControllerService: SavedSearchControllerService,
    public translateService: TranslateService,
    public parametersService: CustomizableTableParametersService,
    public modalService: NzModalService,
    public notification: NzNotificationService,
    public cd: ChangeDetectorRef,
    public sharedAnnotationEditorService: SharedAnnotationEditorService,
    public reachabilityService: ReachabilityService,
    private taxonomyControllerService: TaxonomyControllerService,
    private nzDrawerService: NzDrawerService,
    @Inject(WindowToken) private $window: Window,
  ) {
    super(messageService, clientProjectRelationshipService, userCustomizableTable, graphQlControllerService, route,
      dataPointControllerService, savedSearchControllerService, translateService, parametersService, router, modalService,
      notification, cd, sharedAnnotationEditorService);
  }

  ngOnInit(): void {
    this.tableConfig.bulkSelectionDataPoints = [{ name: 'id', path: upperBoundPath }];
    this.tableConfig.bulkSelectPoints = [
    {name : 'lowerBoundModuleName', path : 'content.lowerBoundModuleName.id'},
    {name : 'upperBoundModuleName', path : upperBoundPath},
    {name : 'accessModulesName', path : 'content.accessModulesName.id'},
    {name : 'intermediateModuleName', path : 'content.intermediateModuleName.id'}
    ];
    this.allowSelectDeselectRows = true; // for enabling the check boxes
    this.tableConfig.showHeaderExtraButton= true; // added this property to show the extra button in table header
    this.tableConfig.occupiedSpace = 265; // To have custom table height since RB table also has breadcrumb
    this.columnForRowSelection = 'upperBoundModuleName'; // setting this property for row id for core table
    this.projectId = this.clientProjectRelationshipService.currentClient.getProjectId();
    this.breadcrumbItems = [this.translateService.instant('reachability.reachabilityBlocks')];
    const selectedBlockIds: string[] = JSON.parse(localStorage.getItem(`${this.projectId}-reachabilityIds`));
    const reachabilityDetails = JSON.parse(localStorage.getItem(`${this.projectId}-reachabilityDetails`));
    this.blockId = selectedBlockIds.map((id: string) => JSON.stringify(id));
    this.tableActions.push({label: this.translateService.instant('reachability.seeAllNodesInModulesTable'),action: () => this.fetchAssociatedModulesForRB()},
    { label: this.translateService.instant('assignTaxonomies.buttonLabel'), action: (data: Set<number>) => this.getAssignmentData(data),
      isBulkSelection: true
    }
    );
    if (this.blockId.length > 1) {
        this.tableActions.push({
        label: this.translateService.instant('reachability.switchColumn'),
        icon: 'swap',
        action: (data: Column[]) => this.changeColumnOrder(data)
      });
      this.pageTitle = this.translateService.instant('reachability.tableViewTitle', {count: this.blockId.length});
      this.breadcrumbItems.push(this.pageTitle);
    } else {
      this.getReachabilityBlock(reachabilityDetails);
    }
    if (reachabilityDetails) {
      this.showReachabilityAlert = (reachabilityDetails?.outdated || reachabilityDetails?.deleted || reachabilityDetails?.mergeParent);
    }
    this.reloadSubscription = this.parametersService.reloadTableDataFlag$.subscribe(value => {
      if (value) {
        this.refreshCoreTable = true;
        this.parametersService.setReloadTableDataValue(false);
      }
    });
    this.initialized = true;
    this.setUrlParameters();
    this.tableConfig.tableActions = this.tableActions.reverse();
    this.tableConfig.selectAllCallback = () => this.getAllReachabilityIds();
    this.additionalGraphQLParams = {
      functionalBlocks: this.blockId,
      includeIntermediateModulesPerAccessModule: false
    };
    this.allowSelectDeselectRows = true;
  }

  /**
   * Method to update the Outdated RB.
   */
  updateOutdatedRb(): void {
    this.reachabilityService.updateOutdatedBlock = true;
    void this.router.navigate(['/project-' + this.projectId + '/reachability/overview']);
  }

  private fetchAssociatedModulesForRB(): void {
    this.moduleIdArray = [];
    const requestQuery = {
      'query': `{
        functionalBlocks(projectId: ${this.projectId}, filterObject: {content_uid: {in: [${this.blockId}]}}) {
          content {
            resolvedModuleParts {
              module {
                id
              }
            }
          }
        }
      }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((result: any) => {
      if (result) {
        this.moduleIdArray = result.data.functionalBlocks?.content.flatMap((block: any) => block.resolvedModuleParts?.map((part: any) =>
          part.module.id));
        const randomValue = new Uint8Array(5);
        crypto.getRandomValues(randomValue);
        const uniqueIdsKeys = `reachabilityModules-${Array.from(randomValue).map(byte => byte.toString(36)).join('')}`;
        const uniqueIdsValue = { blockName: this.pageTitle, moduleIds: this.moduleIdArray };
        localStorage.setItem(uniqueIdsKeys, JSON.stringify(uniqueIdsValue));
        const path = 'modules?preFilter=' + uniqueIdsKeys;
        openInNewTab(this.projectId, null , path, this.$window);
      }
    });
  }

  private getReachabilityBlock(reachabilityDetails: any): void {
    this.breadcrumbItems.push(reachabilityDetails.pageTitle as string);
    this.pageTitle = reachabilityDetails.pageTitle;
    if (reachabilityDetails.mergeParent) {
      this.tableActions.push({
        label: this.translateService.instant('reachability.switchColumn'),
        icon: 'swap',
        action: (data: Column[]) => this.changeColumnOrder(data)
      });
    }
  }

  private changeColumnOrder(columns: Column[]): void {
    const swapColumns = this.customTable?.initialDataPoints.map(row => ({ ...row }));
    const upperBound = swapColumns.find((obj: MiningDataPointDefinitionWithPath) => obj.name === upperBoundName);
    const lowerBound = swapColumns.find((obj: MiningDataPointDefinitionWithPath) => obj.name === `lower${upperBoundName.substr(5)}`);
    if (columns[0].header.includes('Upper')) {
      this.switchColumns = { id: lowerBound.id, column: lowerBound.displayName };
      this.columnForRowSelection = 'lowerBoundModuleName';
    } else {
      this.switchColumns = { id: upperBound.id, column: upperBound.displayName };
      this.columnForRowSelection = 'upperBoundModuleName';
    }
    this.customTable?.initialize(this.pageType);
    this.customTable.switchedDataPoints = this.swapDefaultColumnIndex(swapColumns);
  }

  /**
   * Method to swap defaultColumnIndex values between two objects(Upper/Lower bound).
   * @param columns column data to reorder.
   * @returns swapped/reordered column data.
   */
  private swapDefaultColumnIndex(column: MiningDataPointDefinitionWithPath[]): MiningDataPointDefinitionWithPath[] {
    const propertiesToSwap = [upperBoundName, upperBoundTechnology, upperBoundType];
    propertiesToSwap.forEach(propertyName => {
      const upperBoundProperty = column.find((obj: MiningDataPointDefinitionWithPath) => obj.name === propertyName);
      const lowerBoundProperty = column.find((obj: MiningDataPointDefinitionWithPath) => obj.name === `lower${propertyName.substr(5)}`);
      if (upperBoundProperty && lowerBoundProperty) {
        const property = upperBoundProperty.usageAttributes[this.usage].defaultColumnIndex;
        upperBoundProperty.usageAttributes[this.usage].defaultColumnIndex = lowerBoundProperty.usageAttributes[this.usage].defaultColumnIndex;
        lowerBoundProperty.usageAttributes[this.usage].defaultColumnIndex = property;
      }
    });
    return column;
  }

  private getAssignmentData(ids: Set<number>): void {
    this.moduleIdArray = [];
    ids.forEach((element: number) => {
      if (element) {
        if (element.toString().includes('_')) {
          element = +element.toString().split('_')[2];
        }
        if (element && this.moduleIdArray.indexOf(element) === -1) {
          this.moduleIdArray.push(element);
        }
      }
    });
    this.tableConfig.loading = true;
    this.taxonomyControllerService.getAssignedTaxonomyByModule(this.projectId, { 'modules': { 'ids': this.moduleIdArray } })
      .subscribe((taxonomyAssignmentResponse: TaxonomyAssignmentsGetResponse) => {
        this.tableConfig.loading = false;
        this.taxonomyResponse = taxonomyAssignmentResponse;
        this.assignTaxonomies(ids);
      }, () => {
        this.tableConfig.loading = false;
      });
  }

  private assignTaxonomies(ids: Set<number>): void {
    this.refreshCoreTable = false;
    this.nzDrawerService.create({
      nzTitle: this.translateService.instant('assignTaxonomies.modalTitle', { moduleName: (ids ? this.moduleIdArray.length : 0) + ' Selected Modules' }),
      nzContent: AssignTaxonomiesComponent,
      nzContentParams: {
        taxonomyResponse: this.taxonomyResponse,
        moduleIdArray: this.moduleIdArray,
        projectId: this.projectId,
        parentComponent: false,
        nzModalFooter: false,
        hintText: this.translateService.instant('reachability.assignTaxonomyReachabilityHeaderText')
      },
      nzWidth: '35vw',
      nzPlacement: 'right',
      nzClosable: true,
      nzMaskClosable: false
    }).afterClose.subscribe(() => {
      this.refreshCoreTable = true;
    });
  }

  private getAllReachabilityIds(): void {
    const requestQuery: { [key: string]: any } = {
      'query': `{ reachabilityData(
        projectId: ${this.projectId}
        functionalBlocks: [${this.blockId}]
        sortObject: ${this.parametersService.currentTableParameters.sort}
        includeIntermediateModulesPerAccessModule: ${this.additionalGraphQLParams.includeIntermediateModulesPerAccessModule}
      ) {
        content {
          intermediateModulesData {
            id
          }
          upperBoundModules {
            id
          }
          lowerBoundModules {
            id
          }
          accessModules {
            id
          }
        }
        totalElements
        size
      }
    }`
    };
    this.graphQlControllerService.graphQl(requestQuery).subscribe((response: { [key: string]: any }) => {
      // logic is written the break the entire response in to record per page so that indexing could match
      let slicedResponseBasedOnRecords: { [key: string]: any } = [];
      let counter = 0;
      if (response && response.data.reachabilityData && response.data.reachabilityData.content.length) {
        const content = response.data.reachabilityData.content;
        const totalRecord = Math.ceil(content.length / DEFAULT_NUMBER_OF_ROWS);
        while (counter < totalRecord) {
          counter++;
          slicedResponseBasedOnRecords = content.splice(0, DEFAULT_NUMBER_OF_ROWS);
          this.getModuleIdsBasedOnColumnSelection(slicedResponseBasedOnRecords, counter);
        }
        this.customTable.selectedState = StatesType.ALL;
        this.customTable.checkCurrentPageRecord();
        this.customTable.refreshCheckedStatus();
      }
    });
  }

  private getModuleIdsBasedOnColumnSelection(slicedData: { [key: string]: any }, currentPage: number) {
    slicedData.forEach((contentItem: { [key: string]: any }, index: number) => {
      let defaultId: string = contentItem.upperBoundModules.id;
      if (this.columnForRowSelection === 'lowerBoundModuleName') {
        defaultId = contentItem?.lowerBoundModules?.id;
      }
      const uniqueIdPerRow = currentPage * (DEFAULT_NUMBER_OF_ROWS) + '_' + index + '_' + defaultId;
      this.customTable.setOfSelectedId.add((uniqueIdPerRow) as unknown as number);
      this.customTable.selectedReachabilityRow.push(uniqueIdPerRow);
      if (this.columnForRowSelection === 'lowerBoundModuleName') {
        if (contentItem.upperBoundModules && this.isModuleIncluded(upperBoundPath)) {
          this.customTable.setOfSelectedId.add(contentItem.upperBoundModules.id as number);
        }
      } else {
        if (contentItem.lowerBoundModules && this.isModuleIncluded('content.lowerBoundModuleName.id')) {
          this.customTable.setOfSelectedId.add(contentItem.lowerBoundModules.id as number);
        }
      }
      if (contentItem.intermediateModulesData && this.isModuleIncluded('content.intermediateModuleName.id')) {
        this.setSelectIdsForModule(contentItem.intermediateModulesData as { [key: string]: any });
      }
      if (contentItem.accessModules && this.isModuleIncluded('content.accessModulesName.id')) {
        this.setSelectIdsForModule(contentItem.accessModules as { [key: string]: any });
      }
    });
  }

  private isModuleIncluded(key: string): number {
    return this.customTable.tableConfig.bulkSelectionDataPoints
      .filter((item: MiningDataPointDefinitionWithPath) => item.path === key).length;
  }

  private setSelectIdsForModule(contentItem: { [key: string]: any }) {
    contentItem.forEach((item: { [key: string]: any }) => {
      this.customTable.setOfSelectedId.add(item?.id as number);
    });
  }
}
