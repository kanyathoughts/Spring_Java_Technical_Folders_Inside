import { Component, Input, OnInit } from '@angular/core';
import { Values } from '../analysis-modal.component';

@Component({
  selector: 'filter-form',
  templateUrl: './filter-form.component.html',
  styleUrls: ['./filter-form.component.less']
})
export class FilterFormComponent implements OnInit {

  @Input() maxTagCount = 6;

  typeList: string[] = ['Table', 'File', 'Program', 'Job'];
  technologyList: string[] = ['COBOL', 'JCL', 'JAVA', 'SQL'];
  nameList: string[] = ['MMRS7101', 'MMRS71C1', 'MMRS71Z1', 'MMRS710A', 'MMRS710B', 'MMRS710C', 'MMRS710D'];

  enabled = new Map<string, boolean>([
    ['Type', true],
    ['Technology', false],
    ['Name', false],
    ['Taxonomy', false]
  ]);

  buttonEnabled = true;

  values = new Values();

  constructor() { }

  ngOnInit(): void {
    this.setAllEnabled();
  }

  addFilter(a: string): void {
    this.enabled.set(a, true);
    this.setAllEnabled();
  }

  disableSelector(selector: string): void {
    this.enabled.set(selector, false);
    this.setAllEnabled();
  }

  setAllEnabled(): void {
    for (const [, value] of this.enabled) {
      if ( ! value) {
        this.buttonEnabled = true;
        return;
      }
    }
    this.buttonEnabled = false;
  }

  handleTaxonomyId(event: string[]): void {
    this.values.selectedTaxonomies = event;
  }

}
