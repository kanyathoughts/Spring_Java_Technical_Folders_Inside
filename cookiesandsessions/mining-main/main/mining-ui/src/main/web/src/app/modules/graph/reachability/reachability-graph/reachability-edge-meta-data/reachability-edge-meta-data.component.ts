import { Component, EventEmitter, Input, Output } from '@angular/core';

@Component({
    selector: 'reachability-edge-meta-data',
    templateUrl: './reachability-edge-meta-data.component.html'
})
export class ReachabilityEdgeMetaDataComponent {
    @Input() sharedModuleDetails: any[] = [];
    @Output() closeReachabilitySidePanel: EventEmitter<boolean> = new EventEmitter();

    closePanel(): void {
        this.closeReachabilitySidePanel.emit();
    }
}
