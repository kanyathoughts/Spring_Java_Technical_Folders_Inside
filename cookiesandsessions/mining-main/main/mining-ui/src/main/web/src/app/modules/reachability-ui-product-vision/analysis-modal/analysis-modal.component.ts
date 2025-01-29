import { Component, EventEmitter, Input, Output, ViewChild } from '@angular/core';
import { FilterFormComponent } from './filter-form/filter-form.component';
import { getBasePath } from '@app/core/utils/base-path.utils';

@Component({
  selector: 'analysis-modal',
  templateUrl: './analysis-modal.component.html',
  styleUrls: ['./analysis-modal.component.less']
})
export class AnalysisModalComponent {

  @Input() isVisible: boolean;
  @Output() submitEvent = new EventEmitter<FormResult>();

  @ViewChild('upper') upper: FilterFormComponent;
  @ViewChild('lower') lower: FilterFormComponent;
  @ViewChild('chainCriteria') chainCriteria: FilterFormComponent;

  pathImgAnalysis = getBasePath() + '/assets/reachability-img/impact-analysis.png';
  pathImgDecomposition = getBasePath() + '/assets/reachability-img/decomposition.png';

  relationshipList: string[] = ['Calls', 'Reads', 'Writes', 'Contains'];

  current = 0;
  maxStepIndex = 2;

  radioValue = 'impact_analysis';

  selectedRelationships: string[] = [];

  conditionalDependencies = false;
  controlM = false;

  handleNext(): void {
    if (this.current < this.maxStepIndex) {
      this.current++;
    }
  }

  handleBack(): void {
    if (this.current !== 0) {
      this.current--;
    }
  }

  handleOk(): void {
    this.isVisible = false;
    const res = new FormResult(this.selectedRelationships, this.radioValue, this.upper.values, this.lower.values, this.chainCriteria.values,
      this.conditionalDependencies, this.controlM);
    this.submitEvent.next(res);
  }

  showModal(): void {
    this.isVisible = true;
  }

  handleCancel(): void {
    this.isVisible = false;
  }

  onIndexChange(index: number): void {
    this.current = index;
  }
}

export class Values {
  selectedNames: string[] = [];
  selectedTypes: string[] = [];
  selectedTechnologies: string[] = [];
  selectedTaxonomies: string[] = [];
}

export class FormResult {
  constructor(public selectedRelationShips: string[],
    public approach: string,
    public upper: Values,
    public lower: Values,
    public chainCriteria: Values,
    public conditionalDependencies: boolean,
    public controlM: boolean) {
  }
}
