import { Directive, HostBinding, Input } from '@angular/core';
import { JustifyContentValues } from '../mining-toolbar.component';


@Directive({
  selector: 'mn-toolbar-group'
})
export class MiningToolbarGroupDirective {
  /*
   * Gives the values to defines the alignment inside the toolbar-group
   * Must be an expected value as defined in the MiningToolbar component
   */
  @Input() justifyContent: string;

  @HostBinding('class') class = 'mining-toolbar-group';

  @HostBinding('style.justifyContent') get justifyContentStyle(): string {
    if (JustifyContentValues.includes(this.justifyContent)) {
      return this.justifyContent;
    }
  }

}
