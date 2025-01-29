import { Component, Input } from '@angular/core';

@Component({
  selector: 'mn-default-card-cover',
  templateUrl: './default-card-cover.component.html'
})
/**
 * This component provide a default background with title for cards for selection pages
 */
export class DefaultCardCoverComponent {

  @Input() title: string;

  constructor() {}

}
