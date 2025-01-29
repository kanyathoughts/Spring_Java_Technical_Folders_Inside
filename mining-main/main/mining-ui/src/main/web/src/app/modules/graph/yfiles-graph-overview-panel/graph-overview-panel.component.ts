import { Component } from '@angular/core';

export const EXPAND_ICON_PATH = 'assets/icon-expand.svg';
export const COLLAPSE_ICON_PATH = 'assets/icon-collapse.svg';

@Component({
  selector: 'graph-overview-panel',
  templateUrl: './graph-overview-panel.component.html'
})
export class GraphOverviewPanelComponent {

  showOverview = true;
  iconPath = 'assets/icon-collapse.svg';

  toggleOverview(): void {
    this.showOverview = ! this.showOverview;
    this.iconPath =  this.showOverview ? COLLAPSE_ICON_PATH : EXPAND_ICON_PATH;
  }
}
