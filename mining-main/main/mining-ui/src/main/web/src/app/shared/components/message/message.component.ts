import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-message',
  templateUrl: './message.component.html'
})
export class MessageComponent {
  @Input() message = 'Pass the message';
  @Input() severity: 'info' | 'success' | 'warning' | 'error' = 'info';
  @Input() hrefLink = 'Href link';
  @Input() linkName = 'Link Name';

}
