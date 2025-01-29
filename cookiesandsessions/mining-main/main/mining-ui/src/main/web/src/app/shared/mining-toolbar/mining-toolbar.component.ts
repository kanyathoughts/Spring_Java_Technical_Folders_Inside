import { Component, ViewEncapsulation, Input } from '@angular/core';

export const JustifyContentValues = ['flex-start', 'flex-end', 'center', 'space-between'];

@Component({
  selector: 'mn-toolbar',
  templateUrl: './mining-toolbar.component.html',
  encapsulation: ViewEncapsulation.None,
})
export class MiningToolbarComponent {

  /*
   * Gives the values to defines the alignment inside the toolbar
   * This attribute is useless when the toolbar contains groups
   */
  @Input() justifyContent: string;

  /**
   * Whether the toolbar is sticky in code viewer
   */
  @Input() stickyToolbar: boolean;
  /*
   * Whether the toolbar is displayed in the eclipse view
   */
  @Input() eclipseView: boolean;

  constructor() { }

  /*
   * Set the css attribute in the DOM according to the input and the expected values
   */
  public setJustifyContentStyle(): {'justify-content': string} {
    if (JustifyContentValues.includes(this.justifyContent)) {
      return {
        'justify-content': this.justifyContent
      };
    } else {
      return {
        'justify-content': 'flex-start'
      };
    }
  }

}
