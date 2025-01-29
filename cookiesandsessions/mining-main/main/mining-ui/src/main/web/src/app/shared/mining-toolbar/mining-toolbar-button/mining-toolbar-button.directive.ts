import { Directive, TemplateRef, Input } from '@angular/core';

@Directive({
  selector: '[mn-toolbar-button-dir]'
})
export class MiningToolbarButtonDirective {
  @Input() alwaysHidden = false;
  constructor(public templateRef: TemplateRef<unknown>) { }

}
