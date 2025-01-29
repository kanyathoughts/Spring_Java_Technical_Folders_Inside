import { EventEmitter, OnChanges, OnDestroy, OnInit, Output, SimpleChanges } from '@angular/core';
import { Component, Input } from '@angular/core';
import { getTaxomomyTreeNode } from '@app/core/utils/taxonomy.utils';
import { TaxonomyList } from '@app/shared/interfaces/taxonomy-list.interface';
import { AssignTaxonomiesModalService } from '../assign-taxonomies/assign-taxonomies-modal.service';
import { Subscription } from 'rxjs';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { AssignTaxonomiesComponent } from '../assign-taxonomies/assign-taxonomies.component';
import { TranslateService } from '@ngx-translate/core';
import { TaxonomyAssignmentsGetResponse, TaxonomyPojo } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'module-overview-taxonomy',
  templateUrl: './module-overview-taxonomy.component.html'
})

export class ModuleOverViewTaxonomyComponent implements OnDestroy, OnChanges, OnInit {

  @Input() moduleIdArray: number[];
  @Input() moduleName: string;
  @Input() projectId: number;
  @Input() showModalfromModuleDetails: boolean;
  @Input() taxonomyResponse: TaxonomyAssignmentsGetResponse;
  @Input() isEditor: boolean;
  @Output() closeModal: EventEmitter<any> = new EventEmitter();
  @Output() taxonomyListLength: EventEmitter<any> = new EventEmitter();
  taxonomyList: TaxonomyList[] = [];
  categories: string[];
  private modalInstance: NzModalRef<AssignTaxonomiesComponent, any>;
  private assignTaxonomiesSubscription: Subscription;

  constructor(
    private assignTaxonomiesModalService: AssignTaxonomiesModalService,
    private translateService: TranslateService,
    private modalService: NzModalService) { }

  ngOnInit(): void {
    this.assignTaxonomiesModalService.getloadTaxonomySubject().subscribe((response: boolean) => {
      if (response) {
        const taxonomyData: TaxonomyPojo[] = [];
        this.taxonomyResponse.taxonomies = this.assignTaxonomiesModalService.getupdatedTaxonomyData();
        this.taxonomyResponse.taxonomies.forEach((response) => {
          if (response.state === 'ALL' || response.state === 'SOME') {
            taxonomyData.push(response.taxonomy);
          }
        });
        this.getTaxonomyData(taxonomyData);
      } else {
        this.closeModal.emit(response);
      }
    });

    this.assignTaxonomiesSubscription = this.assignTaxonomiesModalService.closeModal.subscribe((resp: boolean) => {
      if (resp) {
        this.modalInstance.close();
      }
    });
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.showModalfromModuleDetails?.currentValue) {
      this.assignTaxonomies();
    }
  }

  /**
   * method to open modal for taxonomy assignments
   */
  assignTaxonomies(): void {
    this.modalInstance = this.modalService.create<AssignTaxonomiesComponent>(
      {
        nzTitle: this.translateService.instant('assignTaxonomies.modalTitle', { moduleName: this.moduleName }),
        nzContent: AssignTaxonomiesComponent,
        nzMaskClosable: false,
        nzOnCancel: () => {
          this.handleCancel();
        }
      }
    );
    const contentInstance = this.modalInstance.getContentComponent();
    contentInstance.taxonomyResponse = this.taxonomyResponse;
    contentInstance.moduleIdArray = this.moduleIdArray;
    contentInstance.projectId = this.projectId;
  }

  /**
   * Handles cancel operation without saving.
   */
  handleCancel(): void {
    this.closeModal.emit(false);
  }

  /**
   * Gets the necessary taxonomy data through ModuleControllerService.
   * @param taxonomyData data to create taxonomyList and display in taxonomy card.
   */
  getTaxonomyData(taxonomyData: TaxonomyPojo[]): void {
    this.taxonomyList = getTaxomomyTreeNode(taxonomyData);
    this.taxonomyListLength.emit(this.taxonomyList.length);
  }

  ngOnDestroy(): void {
    this.assignTaxonomiesSubscription.unsubscribe();
  }
}
