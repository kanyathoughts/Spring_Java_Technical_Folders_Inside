import { Component, Input } from '@angular/core';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { LocalShowcaseMode } from '@app/core/utils/local-showcase.util';

@Component ({
    selector: 'mn-view-configuration',
    templateUrl: './view-configuration.component.html'
  })
export class ViewConfigurationComponent {

    @Input () clientProjectData: ClientProjectRelationship;
    projectId: number;

    localShowcaseMode = new LocalShowcaseMode();

    constructor() {}

    get isToggleOn(): boolean {
      return this.localShowcaseMode.isInLocalShowcaseMode;
    }

    localShowcaseModeChanged(event: boolean): void {
      this.localShowcaseMode.isInLocalShowcaseMode = event;
    }
}
