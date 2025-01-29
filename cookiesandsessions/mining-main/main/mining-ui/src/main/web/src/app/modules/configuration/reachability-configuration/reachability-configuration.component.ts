import { Component, DestroyRef, input, OnInit } from '@angular/core';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { ProjectControllerService, ProjectRole, ReachabilityAnalysisConfig } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';

@Component({
  selector: 'reachability-configuration',
  templateUrl: './reachability-configuration.component.html'
})
export class ReachabilityConfigurationComponent implements OnInit {
  projectId = input.required<number>();
  clientProjectData = input.required<ClientProjectRelationship>();
  listModuleTypeOptions: Array<{ value: string, label: string }> = [];
  listOfSelectedUpperBoundType: ReachabilityAnalysisConfig.UpperBoundModuleTypesEnum[] = [];
  listOfSelectedLowerBoundType: ReachabilityAnalysisConfig.LowerBoundModuleTypesEnum[] = [];
  isEdit = false;
  enableActions = false;
  isManager = false;
  requiredUpper = false;
  requiredLower = false;
  sameValue = false;

  constructor(private labelMappingService: LabelMappingService,
    private projectController: ProjectControllerService,
    private messageService: NzMessageService,
    private translateService: TranslateService,
    private authorizationService: KeycloakAuthorizationService,
    private destroyRef: DestroyRef
  ) { }

  ngOnInit(): void {
    const projectControllerSubscription = this.projectController.getReachabilityAnalysisConfiguration(this.projectId())
      .subscribe((resp: ReachabilityAnalysisConfig) => {
      this.setModuleTypeOptions();
      if (resp) {
        resp?.upperBoundModuleTypes.forEach(type => {
          this.listOfSelectedUpperBoundType.push(type);
        });
        resp?.lowerBoundModuleTypes.forEach(type => {
          this.listOfSelectedLowerBoundType.push(type);
        });
      } else {
        this.setDefaultType();
      }
    });
    this.isManager = this.authorizationService.hasUserRole(this.clientProjectData(), ProjectRole.UserRoleEnum.MANAGER);
    this.isEdit = localStorage.getItem('reachabilityConfigEdit') === 'true';
    localStorage.removeItem('reachabilityConfigEdit');
    this.destroyRef.onDestroy(() => {
      projectControllerSubscription?.unsubscribe();
      localStorage.removeItem('reachabilityConfigEdit');
    });
  }

  /**
   * To enable the action buttons on click of Edit.
   */
  onEdit(): void {
    this.isEdit = false;
  }

  /**
   * To Validate the selected values and enable the action buttons.
   * @param event selected values from the dropdown.
   */
  checkActionsToEnable(event: string[]): void {
    this.requiredUpper = this.listOfSelectedUpperBoundType.length === 0;
    this.requiredLower = this.listOfSelectedLowerBoundType.length === 0;
    this.sameValue = this.listOfSelectedUpperBoundType.some(value => this.listOfSelectedLowerBoundType.includes(value));
    if (this.requiredUpper || this.requiredLower || this.sameValue) {
      this.enableActions = false;
    } else {
      this.enableActions = event?.length > 0;
    }
  }

  /**
   * To reset the selected values to default.
   */
  onReset(): void {
    this.listOfSelectedLowerBoundType = [];
    this.listOfSelectedUpperBoundType = [];
    this.setDefaultType();
  }

  /**
   * Method To save the configuration.
   */
  saveConfiguartion(): void {
    const reachabilityAnalysisConfig: ReachabilityAnalysisConfig = {
      upperBoundModuleTypes: this.listOfSelectedUpperBoundType,
      lowerBoundModuleTypes: this.listOfSelectedLowerBoundType
    };
    this.projectController.saveReachabilityAnalysisConfiguration(this.projectId(), reachabilityAnalysisConfig).subscribe(() => {
      this.messageService.success(`${this.translateService.instant('reachability.saveConfiguration')}`);
      this.enableActions = false;
    });
  }

  private setDefaultType(): void {
    this.projectController.getReachabilityAnalysisConfiguration(this.projectId(), true).subscribe((resp: ReachabilityAnalysisConfig) => {
      this.setModuleTypeOptions();
      if (resp) {
        resp?.upperBoundModuleTypes.forEach(type => {
          this.listOfSelectedUpperBoundType.push(type);
        });
        resp?.lowerBoundModuleTypes.forEach(type => {
          this.listOfSelectedLowerBoundType.push(type);
        });
      }
    });
  }

  private setModuleTypeOptions(): void {
    Object.keys(this.labelMappingService.labelMappings[LabelType.MODULE_TYPE]).forEach(type => {
      const technologyTypeLabel = this.labelMappingService.labelMappings[LabelType.MODULE_TYPE][type];
      this.listModuleTypeOptions.push({
        value: type,
        label: technologyTypeLabel
      });
    });
  }
}
