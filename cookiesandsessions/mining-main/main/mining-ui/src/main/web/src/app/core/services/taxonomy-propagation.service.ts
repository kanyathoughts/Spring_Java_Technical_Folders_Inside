import { Injectable } from '@angular/core';
import { CustomizableTableColumnService } from './user-customizable-table/customizable-table-column.service';
import { last,switchMap } from 'rxjs/operators';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { TranslateService } from '@ngx-translate/core';
import { TaxonomyPropagationComponent } from '@app/shared/taxonomy-propagation/taxonomy-propagation/taxonomy-propagation.component';
import { JobManagerService } from './job-manager/job-manager.service';
import { ClientProjectRelationshipService } from './client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { JobControllerService, JobInformation } from '@innowake/mining-api-angular-client';

@Injectable({
  providedIn: 'root'
})
export class TaxonomyPropagationService {
  modulePropagationModal: NzModalRef;
  currentProjectId: number;
  selectedTaxonomyName: string;
  selectedTaxonomies: Record<string, unknown>;

  private clientProjectSubscription: Subscription;

  constructor(private jobController: JobControllerService,
    private translateService: TranslateService,
    private userCustomizableTable: CustomizableTableColumnService,
    private router: Router,
    private modalService: NzModalService,
    private jobService: JobManagerService,
    private clientProjectRelationshipService: ClientProjectRelationshipService) { }

  /**
   * Invokes job to identify the affected modules
   * @param requestObject contains details of taxonomy propagation.
   * @param currentProjectId is the project id.
   * @param selectedTaxonomies contains the name of selected taxonomies list with comma as separator.
   */
  startTaxonomyPropagationJob(requestObject: Record<string, unknown>, triggeredJobProjectId: number, selectedTaxonomies: string): void {
    this.selectedTaxonomies = requestObject;
    this.selectedTaxonomyName = selectedTaxonomies;
    let validationJobId: string;
    this.clientProjectSubscription = this.clientProjectRelationshipService.getClientProjectObservable()
    .subscribe((clientProjectRelationship: ClientProjectRelationship) => {
      this.currentProjectId = clientProjectRelationship?.getProjectId() ? clientProjectRelationship?.getProjectId(): triggeredJobProjectId;
    });
    this.jobController.submitJobExtension(triggeredJobProjectId, 'taxonomy-propagation-module-identification', requestObject as { [key: string]: string[]; })
      .pipe(switchMap((jobId: string[]) => {
        validationJobId = jobId as unknown as string;
        return this.jobService.register({
          jobId: jobId as unknown as string
        }).status$;
      }), last(),
        switchMap((jobRes: JobInformation.StatusEnum) => jobRes === 'SUCCESS' ? this.jobController.getJobResult(validationJobId as unknown as string): null)
        ).subscribe((jobResult) => {
          if (this.router.url.includes('modules') && this.currentProjectId === triggeredJobProjectId) {
            this.modulePropagationModal = this.modalService.create<TaxonomyPropagationComponent>({
              nzTitle: this.translateService.instant('taxonomyPropagation.taxonomyPropagationModalTitle'),
              nzKeyboard: true,
              nzClosable: true,
              nzCentered: true,
              nzWidth: '80%',
              nzMaskClosable: false,
              nzWrapClassName: 'vertical-center-modal',
              nzContent: TaxonomyPropagationComponent
            });
            const instance = this.modulePropagationModal.getContentComponent();
            instance.affectedModules = jobResult;
            instance.startModules = requestObject.moduleIds as number[];
            instance.usage = Usages.MODULETABLE;
            instance.graphQlType = 'modules';
            instance.projectId = triggeredJobProjectId;
            instance.pageType = TypeName.PAGEMODULE;
            instance.moduleIdForPreFiltering = jobResult?.object[1]?.map((modules: Record<string, unknown>) => modules.moduleId);
            instance.internalDataPoints = [this.userCustomizableTable.dataPointsList.find(y => y.name === 'id')];
            instance.allowSelectDeselectRows = true;
            instance.selectedTaxonomyNames = this.translateService.instant('taxonomyPropagation.selectedTaxonomiesInfo',
              { selectedTaxonomiesString: selectedTaxonomies });
          }
        });
  }
}
