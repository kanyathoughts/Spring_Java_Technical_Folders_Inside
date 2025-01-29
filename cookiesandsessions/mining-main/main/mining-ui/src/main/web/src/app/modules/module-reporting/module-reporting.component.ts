import { Component, OnDestroy, OnInit, Inject, ChangeDetectorRef, AfterContentChecked } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { NzMessageService } from 'ng-zorro-antd/message';
import { BadgeCountUpdateOperation } from '@app/modules/module-details/models/module-complexity-details';
import { ModuleBadgeUpdateService } from '@app/core/services/module-details/module-badge-update.service';
import { ActivatedRoute, Router } from '@angular/router';
import { BaseReportingPageComponent } from '@app/shared/components/base-customizable-table/base-reporting-page.component';
import { BulkAction, MiningTableRow } from '@app/shared/components/mining-table/mining-table-config.interface';
import { AssignTaxonomiesComponent } from '@app/shared/components/assign-taxonomies/assign-taxonomies.component';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { NzModalService } from 'ng-zorro-antd/modal';
import { LinkType } from '@app/shared/components/mining-table/mining-table-action.interface';
import { MiningTableOptionSelected } from '@app/shared/components/mining-table/mining-table-option-selected.interface';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { WindowToken } from '@app/core/utils/window';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { Observable } from 'rxjs';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { handleCodeViewerButton } from '@app/shared/components/code-viewer-button/code-viewer-button.component';
import { HttpErrorResponse } from '@angular/common/http';
import { Logger } from '@app/core';
import { IdentifyAffectedModulesComponent } from '../../shared/taxonomy-propagation/identify-modules/identify-affected-modules-by-propagation.component';
import { buttonLabels } from '@app/core/services/dna-analysis.service';
import { CandidateIdentificationControllerService, ControlFlowControllerService, DataPointControllerService,
  JobControllerService,
  ModuleControllerService,
  ProjectRole,
  SavedSearchControllerService,
  TaxonomyAssignmentsGetResponse,
  TaxonomyControllerService} from '@innowake/mining-api-angular-client';
import { last } from 'rxjs/internal/operators/last';
import { SharedAnnotationEditorService } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.service';
import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { CfgSupportedTypeService } from '@app/core/services/cfg-supported-type.service';
const log = new Logger('ModuleReporting');

@Component({
  selector: 'app-mining-module',
  templateUrl: '../../shared/components/base-customizable-table/base-reporting-page.component.html'
})
export class ModuleReportingComponent extends BaseReportingPageComponent implements OnInit, OnDestroy, AfterContentChecked {

  pageTitle = this.translateService.instant('modules');

  selectedFilter = this.translateService.instant('modules');

  graphQlType = 'modules';

  pageType = TypeName.PAGEMODULE;

  usage = Usages.MODULETABLE;
  errorMessage = this.translateService.instant('errorFetchingDetails', { tableType: 'Module' });
  taxonomyResponse: TaxonomyAssignmentsGetResponse;
  moduleIdArray: number[] = [];

  constructor(
    public clientProjectRelationship: ClientProjectRelationshipService,
    public userCustomizableTable: CustomizableTableColumnService,
    public dataPointControllerService: DataPointControllerService,
    public messageService: NzMessageService,
    public route: ActivatedRoute,
    public graphQlControllerService: GraphQlControllerService,
    public savedSearchControllerService: SavedSearchControllerService,
    public translateService: TranslateService,
    public parametersService: CustomizableTableParametersService,
    public router: Router,
    public modalService: NzModalService,
    public notification: NzNotificationService,
    public cd: ChangeDetectorRef,
    private moduleService: ModuleControllerService,
    private moduleBadgeUpdateService: ModuleBadgeUpdateService,
    private taxonomyControllerService: TaxonomyControllerService,
    private nzDrawerService: NzDrawerService,
    private candidateIdentificationService: CandidateIdentificationControllerService,
    private controlFlowService: ControlFlowControllerService,
    private jobManagerService: JobManagerService,
    private jobController: JobControllerService,
    private authorizationService: KeycloakAuthorizationService,
    private sharedAnnotationService: SharedAnnotationEditorService,
    private featureToggleService: FeatureToggleService,
    private cfgSupportedTypesService: CfgSupportedTypeService,
    @Inject(WindowToken) private $window: Window
  ) {
    super(messageService, clientProjectRelationship, userCustomizableTable, graphQlControllerService, route,
      dataPointControllerService, savedSearchControllerService, translateService, parametersService, router, modalService,
      notification, cd, sharedAnnotationService);
  }

  ngOnInit(): void {
    this.featureToggleService.isActive('generativeAiTranslations').subscribe((isActive) => {
      this.clientProjectSubscription = this.initialize().subscribe((currentClient) => {
        this.internalDataPoints = [
          { name: 'id', path: 'content.id'},
          { name: 'requiresReview', path: 'content.requiresReview'},
          { name: 'inCodebase', path: 'content.inCodebase'},
          { name: 'identification', path: 'content.identification'},
          { name: 'storage', path: 'content.storage' },
          { name: 'linkHash ', path: 'content.linkHash ' },
        ];
        this.hasRequiresReview(currentClient);
        const bulkActions: BulkAction[] = [
          {
            label: this.translateService.instant('bulkActionButtonLbls.sendToCallChainLabel'),
            tooltip: this.translateService.instant('bulkActionButtonLbls.sendToCallChainTooltipText'),
            action: (data: Set<number>) => this.identifyWebBasedAnalyses(data, 'globalCallChain')
          },
          {
            label: this.translateService.instant('bulkActionButtonLbls.calculateControlFolwlbl'),
            action: (data: Set<number>) => this.identifyWebBasedAnalyses(data, 'calculateControlFlowGrapgh')
          }
        ];
        const tableConfig: any = {};
        if (this.authorizationService.hasUserRole(currentClient, ProjectRole.UserRoleEnum.MANAGER)) {
          if (isActive) {
            bulkActions.push({
              label: this.translateService.instant('bulkActionButtonLbls.generate'),
              tooltip: this.translateService.instant('genAI.contentWarning'),
              class: 'gen-Ai__generateBtn',
              icon: 'mining-icons:gen-ai-stars',
              subActions: [
                {
                  label: this.translateService.instant('bulkActionButtonLbls.explainAnnotations'),
                  tooltip: this.translateService.instant('bulkActionButtonLbls.explainAnnotationsTooltipText'),
                  action: (data: Set<number>) => this.identifyWebBasedAnalyses(data, 'explainAnnotations')
                },
                {
                  label: this.translateService.instant('bulkActionButtonLbls.generateModuleDescriptions'),
                  tooltip: this.translateService.instant('bulkActionButtonLbls.generateModuleDescriptionsTooltipText'),
                  action: (data: Set<number>) => this.identifyWebBasedAnalyses(data, 'generateModuleDescriptions')
                }
              ]
            });
          }
          bulkActions.push(
            {
              label: this.translateService.instant('bulkActionButtonLbls.identify'),
              subActions: [{
                label: this.translateService.instant('bulkActionButtonLbls.identifyCandidatesbuttonLabel'),
                tooltip: this.translateService.instant('bulkActionButtonLbls.identifyCandidatesbuttonTooltipText'),
                action: (data: Set<number>) => this.identifyWebBasedAnalyses(data, 'identifyCandidates')
              },
              {
                label: this.translateService.instant('bulkActionButtonLbls.identifyDeadCodebuttonLabel'),
                tooltip: this.translateService.instant('bulkActionButtonLbls.identifyDeadCodeButtonTooltipText'),
                action: (data: Set<number>) => this.identifyWebBasedAnalyses(data, 'identifyDeadCode')
              },

              {
                label: this.translateService.instant('bulkActionButtonLbls.identifyTechnicalTaxonomies'),
                action: (data: Set<number>) => this.identifyWebBasedAnalyses(data, 'identifyTachnicalTaxonomies'),
              },
              {
                label: this.translateService.instant('bulkActionButtonLbls.identifyModuleDescriptions'),
                action: (data: Set<number>) => this.identifyWebBasedAnalyses(data, 'idetifyModuleDescriptions')
              }
            ]
            }
          );
        }
        if (this.authorizationService.hasUserRole(currentClient, ProjectRole.UserRoleEnum.EDITOR)) {
          bulkActions.push({
            label: this.translateService.instant('assignTaxonomies.buttonLabel'),
            subActions: [{
              label: this.translateService.instant('moduleReporting.assignTaxonomiesToModules'),
              action: (data: Set<number>) => this.getAssignmentData(data)
            },
            {
              label: this.translateService.instant('taxonomyPropagation.pageTitle'),
              action: (data: Set<number>) => this.taxonomyPropagation(data)
            }]
          });
        }
        tableConfig.bulkActions = bulkActions.reverse();
        this.tableConfig = tableConfig;
      });
    });
    const isPreFilterFromQuery: string = this.route.snapshot.queryParams['preFilter'] || '';
    Object.keys(localStorage).forEach((key) => {
      if ((key.includes('call-chain') || key.includes('reachabilityModules')) &&
        isPreFilterFromQuery !== key && isPreFilterFromQuery !== '') {
        localStorage.removeItem(key);
      }
    });
    this.rowActions = [[
      {
        type: LinkType.DROPDOWN,
        icon: 'more',
        options: [
          {
            label: this.translateService.instant('iconToolTip.codeViewer'), value: buttonLabels.codeViewer,
            optionToolTip: (data: MiningTableRow) => this.translateService.instant(this.disableTableAction(buttonLabels.codeViewer, data).toolTip),
            disableItem: (data: MiningTableRow) => this.disableTableAction(buttonLabels.codeViewer, data).disableButton
          },
          {
            label: this.translateService.instant('iconToolTip.controlFlow'), value: buttonLabels.controlFlow,
            optionToolTip: (data: MiningTableRow) => this.translateService.instant(this.disableTableAction(buttonLabels.controlFlow, data).toolTip),
            disableItem: (data: MiningTableRow) => this.disableTableAction(buttonLabels.controlFlow, data).disableButton
          },
          { label: this.translateService.instant('iconToolTip.depGraph'), value: 'dependencies', disableItem: () => false },
        ]
      },
    ]];
    this.loadCustomizableTable();
    this.allowSelectDeselectRows = true;
  }

  ngAfterContentChecked(): void {
    this.cd.detectChanges();
  }

  /**
   * handles selected Option of screen navigations.
   * @param rowData gives the details of screen
   */
  handleSelectedOption(rowData: MiningTableOptionSelected): void {
    const moduleId: number = rowData?.data?.id;
    openInNewTab(this.projectId, moduleId, rowData.optionValue, this.$window);
  }

  /**
   * Gets the taxonomy assignment data through TaxonomyControllerService.
   * @param ids gives the set of module id
   */
  getAssignmentData(ids: Set<number>): void {
    this.moduleIdArray = [];
    ids.forEach((element: number) => {
      this.moduleIdArray.push(element);
    });
    this.tableConfig.loading = true;
    this.taxonomyControllerService.getAssignedTaxonomyByModule(this.projectId, { 'modules': { 'ids': this.moduleIdArray } })
      .subscribe((taxonomyAssignmentResponse: TaxonomyAssignmentsGetResponse) => {
        this.tableConfig.loading = false;
        this.taxonomyResponse = taxonomyAssignmentResponse;
        this.assignTaxonomies(ids);
      }, (error: HttpErrorResponse) => {
        this.tableConfig.loading = false;
        log.error(error);
      });
  }

  /**
   * open the drawer of the TaxonomyPropagationComponent
   * @param ids gives the set of module id
   */
  taxonomyPropagation(ids: Set<number>): void {
    const moduleIds = Array.from(ids);
    this.nzDrawerService.create({
      nzTitle: this.translateService.instant('taxonomyPropagation.pageTitle'),
      nzMaskClosable: false,
      nzWrapClassName: 'vertical-center-modal',
      nzContent: IdentifyAffectedModulesComponent,
      nzContentParams: {
        projectId: this.projectId,
        moduleIds
      },
      nzWidth: '36vw',
      nzPlacement: 'right',
      nzClosable: true,
    });
  }

  /**
   * open the drawer of the AssignTaxonomiesComponent
   * @param ids gives the set of module id
   */
  assignTaxonomies(ids: Set<number>): void {
    this.nzDrawerService.create({
      nzTitle: this.translateService.instant('assignTaxonomies.modalTitle', { moduleName: (ids ? ids.size : 0) + ' Selected Modules' }),
      nzContent: AssignTaxonomiesComponent,
      nzContentParams: {
        taxonomyResponse: this.taxonomyResponse,
        moduleIdArray: this.moduleIdArray,
        projectId: this.projectId,
        parentComponent: false,
        nzModalFooter: false
      },
      nzWidth: '35vw',
      nzPlacement: 'right',
      nzClosable: true,
      nzMaskClosable: false
    }).afterClose.subscribe((element: any) => {
      if (element.result === FormResult.Saved) {
        this.refreshCoreTable = true;
      }
    });
  }

  /**
   * identify web based analyses based on selected job type
   * @param ids gives the set of module id
   * @param jobType gives the type of job that user wants perform
   */
  identifyWebBasedAnalyses(ids: Set<number>, jobType: string): void {
    this.moduleIdArray = [];
    ids.forEach((element: number) => {
      this.moduleIdArray.push(element);
    });
    let identifyRequest: Observable<string[]>;
    switch (jobType) {
      case 'identifyCandidates':
        identifyRequest = this.candidateIdentificationService.identifyAllCandidates(this.projectId, { 'ids': this.moduleIdArray });
        break;
      case 'identifyDeadCode':
        identifyRequest = this.candidateIdentificationService.identifyDeadCode(this.projectId, { 'ids': this.moduleIdArray });
        break;
      case 'identifyTachnicalTaxonomies':
        identifyRequest = this.taxonomyControllerService.identifyTechnicalTaxonomies(this.projectId, { 'ids': this.moduleIdArray });
        break;
      case 'idetifyModuleDescriptions':
        identifyRequest = this.moduleService.identifyModuleDescriptions(this.projectId, { 'ids': this.moduleIdArray });
        break;
      case 'calculateControlFlowGrapgh':
        identifyRequest = this.controlFlowService.calculateControlFlowGraphs(this.projectId, { 'ids': this.moduleIdArray });
        break;
      case 'explainAnnotations':
        this.showAnnotationsModal();
        break;
      case 'generateModuleDescriptions':
        this.showModulesModal();
        break;
      case 'globalCallChain':
        Object.keys(localStorage).forEach((key) => {
          if (key.includes('reporting_moduleIds')) {
            localStorage.removeItem(key);
          }
        });
        localStorage.setItem(`${this.projectId}-reporting_moduleIds`, JSON.stringify(this.moduleIdArray));
        openInNewTab(this.projectId, null, 'reachability/call-chain', this.$window);
        break;
    }
    identifyRequest?.subscribe((response: string[]) => {
      const jobProgressId = response.toString();
      this.jobManagerService.register({ jobId: jobProgressId, foreground: true, cancellable: true });
    });
  }

  showAnnotationsModal(): void {
    this.annotationConfirmVisible = true;
  }

  showModulesModal(): void {
    this.moduleConfirmVisible = true;
  }

  handleAnnotationsModalOk(): void {
    this.jobController.submitJobExtensionV2(this.projectId, 'generate-annotation-descriptions-from-module',
      { 'ids': this.moduleIdArray, 'overwrite': this.explainAnnotationsOverwrite })
      .subscribe((response: string[]) => {
        this.getJobStatus(response.toString(), false);
      }, (error: HttpErrorResponse) => {
        log.error(error.message);
      });
    this.annotationConfirmVisible = false;
  }

  handleModulesModalOk(): void {
    this.jobController.submitJobExtensionV2(this.projectId, 'generate-module-descriptions',
      { 'ids': this.moduleIdArray, 'overwrite': this.generateModuleDescriptionsOverwrite })
      .subscribe((response: string[]) => {
        this.getJobStatus(response.toString(), false);
      }, (error: HttpErrorResponse) => {
        log.error(error.message);
      });
    this.moduleConfirmVisible = false;
  }

  /**
   * Updates the requires review status on deleting the warning.
   */
  handleAlertOnConfirm(): void {
    this.showWarning = false;
    this.moduleService.clearRequiresReview(this.projectId).subscribe(() => {
      this.moduleBadgeUpdateService.updateModuleToBeReviewed(BadgeCountUpdateOperation.DELETE_ALL_WARNINGS);
    });
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.destroy();
  }

  private hasRequiresReview(curentClient: ClientProjectRelationship): void {
    this.moduleService.countRequiresReview(curentClient.getProjectId()).subscribe((count: number) => {
      if (count) {
        this.showWarning = true;
        this.alertText = this.translateService.instant('noOfModifiedAlert', { count });
        this.alertPopconfirmTitle = this.translateService.instant('popConfirmDeleteWithValue', { count });
        this.alertOkText = this.translateService.instant('btnLabel.deleteAll');
        this.alertCancelText = this.translateService.instant('btnLabel.cancel');
        this.alertButtonText = this.translateService.instant('deleteWarning');
      }
    });
  }

  private getJobStatus(jobId: string, autoDownload: boolean): void {
    const remoteJob = {
      jobId: jobId as unknown as string,
      autoDownloadResult: autoDownload,
      foreground: true,
      cancellable: true
    };
    this.jobManagerService.register(remoteJob).status$.pipe(last()).subscribe(() => {
    });
  }

  private disableTableAction(butttonLabel: string, data: MiningTableRow): { disableButton: boolean; toolTip: string } {
    switch (butttonLabel) {
      case buttonLabels.codeViewer:
        data['storage'] = data.storage;
        data['sourceCodeAvailable'] = data.inCodebase;
        data['identification'] = data.identification;
        return handleCodeViewerButton(data);
      case buttonLabels.controlFlow:
        const actuallySupported = this.cfgSupportedTypesService.checkIfSupported(data.technology as string, data.type as string);
        return { disableButton: ! (actuallySupported && data.inCodebase), toolTip: this.translateService.instant('cfgNotAvailable') };
    }
  }

}
