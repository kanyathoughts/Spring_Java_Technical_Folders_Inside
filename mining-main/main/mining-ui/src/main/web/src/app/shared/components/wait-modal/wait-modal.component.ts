import { Component } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

@Component({
  selector: 'app-wait-modal',
  templateUrl: './wait-modal.component.html'
})
export class WaitModalComponent {

  constructor(
    protected translateService: TranslateService) {
  }

}
