import { Component, Input } from '@angular/core';

@Component({
  selector: 'mn-loader',
  templateUrl: './loader.component.html'
})
export class LoaderComponent {
  @Input() loadingMessage= '';
}
