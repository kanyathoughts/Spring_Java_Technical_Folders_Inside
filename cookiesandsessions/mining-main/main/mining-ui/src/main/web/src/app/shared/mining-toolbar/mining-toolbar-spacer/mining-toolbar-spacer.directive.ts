import { Directive, Input, HostBinding } from '@angular/core';

@Directive({
  selector: 'mn-toolbar-spacer'
})
export class MiningToolbarSpacerDirective {
  /*
   * Gives the size (in em) of the spacer element
   * Default value is 1
   */
  @Input() size = 1;

  @HostBinding('class') get classBuilder(): string {
    if (isNaN(this.size)) {
      this.size = 1;
    }
    return 'mining-toolbar-spacer-' + this.size;
  }

}
