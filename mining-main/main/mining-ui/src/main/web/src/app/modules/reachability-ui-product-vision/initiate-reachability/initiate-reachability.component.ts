import { Component, Input, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { ProjectControllerService, ReachabilityAnalysisConfig, ReachabilityAnalysisRequest
} from '@innowake/mining-api-angular-client';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';

@Component({
  selector: 'initiate-rechability',
  templateUrl: './initiate-reachability.component.html'
})
export class InitiateReachabilityComponent implements OnInit {
  @Input() projectId: number;
  switchUpperLowerBoundary = 'upper';
  upperBoundTypeTaxonomy = 'type';
  listModuleTypeOptions: Array<{ value: string, label: string }> = [];
  listOfUpperBound: ReachabilityAnalysisConfig.UpperBoundModuleTypesEnum[] = [];
  listOfLowerBound: ReachabilityAnalysisConfig.LowerBoundModuleTypesEnum[] = [];
  taxonomyIds: string[] = [];

  constructor(
    private modal: NzModalRef,
    private projectController: ProjectControllerService,
    private router: Router,
    private labelMappingService: LabelMappingService) { }

  ngOnInit(): void {
    this.setModuleTypeOptions();
  }

  /**
   * Handles the cancel action for the modal.
   * Closes the modal and destroys it.
   */
  handleCancel(): void {
    this.modal?.destroy();
  }

  /**
   * Send user entered details on saving the modal.
   */
  sendToParent(): void {
    const reachabilityAnalysisRequest: ReachabilityAnalysisRequest = {
      analysisType: this.switchUpperLowerBoundary === 'upper' ? ReachabilityAnalysisRequest.AnalysisTypeEnum.TOP_DOWN :
      ReachabilityAnalysisRequest.AnalysisTypeEnum.BOTTOM_UP,
      ...(this.taxonomyIds.length) && {moduleTaxonomies: new Set(this.taxonomyIds)}
    };
    this.modal.close(reachabilityAnalysisRequest);
  }

  /**
   * Method to get taxonomy selection
   * @param event user selected taxonomy details.
   */
  getTaxonomySelection(event: TaxonomyFilterSelected[]): void {
    this.taxonomyIds.length = 0;
    event.forEach((taxonomyId: TaxonomyFilterSelected) => {
      this.taxonomyIds.push(...taxonomyId.toString().split('_')[1].split(','));
    });
  }

  /**
   * Redirects to reachability cofniguration page.
   */
  redirectToConfig(): void {
    this.modal.close();
    localStorage.setItem('reachabilityConfigEdit', 'true');
    void this.router.navigate(['/project-' + this.projectId + '/configuration/reachability']);
  }

  private setModuleTypeOptions(): void {
    this.projectController.getReachabilityAnalysisConfiguration(this.projectId).subscribe((resp: ReachabilityAnalysisConfig) => {
      if (resp) {
        resp?.upperBoundModuleTypes.forEach(type => {
          const technologyTypeLabel = this.labelMappingService.mapLabel(LabelType.MODULE_TYPE, type);
          this.listOfUpperBound.push(technologyTypeLabel as ReachabilityAnalysisConfig.UpperBoundModuleTypesEnum);
        });
        resp?.lowerBoundModuleTypes.forEach(type => {
          const technologyTypeLabel = this.labelMappingService.mapLabel(LabelType.MODULE_TYPE, type);
          this.listOfLowerBound.push(technologyTypeLabel as ReachabilityAnalysisConfig.LowerBoundModuleTypesEnum);
        });
      }
    });
  }
}
