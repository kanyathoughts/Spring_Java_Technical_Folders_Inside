import { Component, Input, OnInit, Optional } from '@angular/core';
import { UntypedFormBuilder, UntypedFormControl, UntypedFormGroup, Validators } from '@angular/forms';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { TranslateService } from '@ngx-translate/core';
import { NzDrawerRef } from 'ng-zorro-antd/drawer';
import { MiningTableAction } from '../../components/mining-table/mining-table-action.interface';
import { MiningTableRow } from '../../components/mining-table/mining-table-config.interface';
import { TaxonomyPropagationService } from '@app/core/services/taxonomy-propagation.service';
import { TaxonomyAssignmentsGetResponse } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'identify-affected-modules-by-propagation',
  templateUrl: './identify-affected-modules-by-propagation.component.html',
})

export class IdentifyAffectedModulesComponent implements OnInit {
  @Input() projectId: number;
  @Input() moduleIds: number[];
  @Input() modulesList: MiningTableRow[];
  taxonomyPropagation: UntypedFormGroup;
  onLoading: boolean;
  listOfReferences: Array<{ value: string, label: string, toolTip: string }> = [];
  listOfReadsWritesAccesses: Array<{ value: string, label: string }>;
  taxonomyResponse: TaxonomyAssignmentsGetResponse;
  taxonomyId: string[] = [];
  childVisible = false;
  selectedIncomingReferences: string[] = [];
  selectedOutgoingReferences: string[] = [];
  selectedTaxonomyNames = '';

  constructor(
    private formBuilder: UntypedFormBuilder,
    private translateService: TranslateService,
    private taxonomyPropagationService: TaxonomyPropagationService,
    @Optional() private drawerRef: NzDrawerRef<MiningTableAction>
  ) { }

  ngOnInit(): void {
    this.taxonomyPropagation = this.formBuilder.group({
      incomingReference: new UntypedFormControl([], Validators.required),
      outgoingReference: new UntypedFormControl([], Validators.required),
      readsWritesAccesses: new UntypedFormControl([])
    });
    const edgeRefernces =  this.translateService.instant('edgeReferences');
    const refernces = ['Calls', 'Includes', 'Accesses', 'References', 'Contains'];
    const readsWritesAccessTypes = ['Read', 'Write', 'Delete', 'Update', 'Store'];
    this.listOfReferences = refernces.map(item => ({
      value: item,
      label: edgeRefernces[item.toLowerCase()]?.label,
      toolTip: edgeRefernces[item.toLowerCase()]?.toolTip
    }));
    this.listOfReadsWritesAccesses = readsWritesAccessTypes.map(item => ({
      value: item,
      label: item
    }));
  }

  /**
   * method to get names of selected taxonomy filters
   * @param  event event emitted by taxonomy filter component with details
   */
  getSelectedTaxonomyDetails(event: TaxonomyFilterSelected[]): void {
    this.taxonomyId = [];
    this.selectedTaxonomyNames = '';
    const taxonomyTypeName: string[] = [];
    event.forEach((taxonomyDetail: TaxonomyFilterSelected) => {
      this.taxonomyId.push(...taxonomyDetail.taxonomyId.toString().split(','));
      const selectedTaxonomy = taxonomyDetail.taxonomyTitle.split(':');
      if (selectedTaxonomy.length === 1) {
        this.selectedTaxonomyNames = selectedTaxonomy[0];
      } else {
        taxonomyTypeName.push(selectedTaxonomy[1]);
        this.selectedTaxonomyNames = taxonomyTypeName.join(', ');
      }
    });
  }

  /*
     * method to start the propagation.
     */
  startPropagation(): void {
    this.onLoading = true;
    const requestObject = {
      'taxonomyIds': Array.from(new Set(this.taxonomyId)),
      'incomingReferences': this.taxonomyPropagation.get('incomingReference').value,
      'outgoingReferences': this.taxonomyPropagation.get('outgoingReference').value,
      'readsWritesAccesses': this.taxonomyPropagation.get('readsWritesAccesses').value,
      'moduleIds': this.moduleIds
    };
    this.taxonomyPropagationService.startTaxonomyPropagationJob(requestObject, this.projectId, this.selectedTaxonomyNames);
    this.onLoading = false;
    this.handleCancel();
  }

  /*
   * method to Close the parent drawer window.
   */
  handleCancel(): void {
    this.drawerRef?.close();
  }

  /*
   * method to handle incoming and outgoing references changes and handle form accordingly
   */
  handleIncomingAndOutgoingReferenceChange(): void {
    if ( ! this.selectedIncomingReferences.includes('Accesses') && ! this.selectedOutgoingReferences.includes('Accesses')) {
      this.taxonomyPropagation.get('readsWritesAccesses').reset();
    }
  }

  /*
   * method to open or close child drawer window.
   */
  setChildDrawerVisibilty(isVisible: boolean): void {
    this.childVisible = isVisible;
  }
}
