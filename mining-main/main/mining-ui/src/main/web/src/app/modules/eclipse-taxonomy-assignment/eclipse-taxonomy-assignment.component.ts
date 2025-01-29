import { Component, OnDestroy, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { Subscription } from 'rxjs';
import { TaxonomyList } from '@app/shared/interfaces/taxonomy-list.interface';
import { getTaxomomyTreeNode } from '@app/core/utils/taxonomy.utils';
import { FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TaxonomyAssignmentsGetResponse, TaxonomyControllerService, TaxonomyPojo } from '@innowake/mining-api-angular-client';

declare const java__callback__retrieveIdentifiers: () => Promise<string[]>;
declare const java__callback__onSave: () => Promise<string>;
declare const java__callback__onCancel: () => Promise<string>;

@Component({
  selector: 'app-eclipse-taxonomy-assignment',
  templateUrl: './eclipse-taxonomy-assignment.component.html',
})
export class EclipseTaxonomyAssignmentComponent implements OnInit, OnDestroy {
  taxonomyListLength: number;
  moduleIdArray: number[];
  modulePathPatterns: string[];
  taxonomyList: TaxonomyList[] = [];
  taxonomyResponse: TaxonomyAssignmentsGetResponse;
  projectId: number;
  categories: string[];
  showEmptyState = false;
  taxonomyAssignmentTitle: string;
  private clientProjectSubscription: Subscription;

  constructor(
    private translateService: TranslateService,
    private taxonomyControllerService: TaxonomyControllerService,
    private clientProjectRelationshipService: ClientProjectRelationshipService,
    private messageService: NzMessageService) {
  }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationshipService.getClientProjectObservable()
      // eslint-disable-next-line @typescript-eslint/no-misused-promises
      .subscribe(async (clientProjectRelationship: ClientProjectRelationship) => {
        this.projectId = clientProjectRelationship.getProjectId();
        this.moduleIdArray = [];
        const isMethodDefined = typeof java__callback__retrieveIdentifiers;
        if (isMethodDefined.toLowerCase() !== 'undefined') {
          try {
            this.modulePathPatterns = await java__callback__retrieveIdentifiers();
            this.getAssignmentData();
          } catch (err) {
            this.showEmptyState = true;
          }
        } else {
          this.showEmptyState = true;
        }
      });
  }

  /**
   * Gets the taxonomy assignment data through TaxonomyControllerService.
   */
  getAssignmentData(): void {
    this.taxonomyAssignmentTitle = '';
    this.taxonomyControllerService.getAssignedTaxonomyByModule
      (this.projectId, { 'modules': { 'ids': this.moduleIdArray,'pathPatterns':this.modulePathPatterns } })
      .subscribe((taxonomyAssignmentResponse: TaxonomyAssignmentsGetResponse) => {
        const taxonomyData: TaxonomyPojo[] = [];
        const moduleCount = taxonomyAssignmentResponse.moduleCount;
        taxonomyAssignmentResponse.taxonomies.forEach((response) => {
          if (response.state === 'ALL' || response.state === 'SOME') {
            taxonomyData.push(response.taxonomy);
          }
        });
        this.taxonomyResponse = taxonomyAssignmentResponse;
        this.taxonomyList = getTaxomomyTreeNode(taxonomyData);
        if (this.taxonomyResponse.taxonomies.length > 0) {
          if (moduleCount > 1) {
            this.taxonomyAssignmentTitle = this.translateService.instant('assignTaxonomies.modalTitle', { moduleName:  `${moduleCount} Selected Modules`});
          } else {
            const title = this.modulePathPatterns[0].split('/');
            this.taxonomyAssignmentTitle = this.translateService.instant('assignTaxonomies.modalTitle', { moduleName:  `${title[title.length - 1]}`});
          }
          this.showEmptyState = false;
        } else {
          this.showEmptyState = true;
        }
      });
  }

  /**
   * Triggered the switch case on the basis of result.
   * @param result is the result return by Form (Save or Cancel).
   */
  async handleFormResult(result: FormResult): Promise<void> {
    switch (result) {
      case FormResult.Saved:
        const successContent: string = this.translateService.instant('assignTaxonomies.successMessage');
        this.messageService.success(successContent);
        await java__callback__onSave();
        break;
      case FormResult.Canceled:
        await java__callback__onCancel();
        break;
    }
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
  }
}
