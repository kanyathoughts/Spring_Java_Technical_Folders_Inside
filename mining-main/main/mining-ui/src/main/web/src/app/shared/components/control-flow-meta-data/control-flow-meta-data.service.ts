import { Injectable } from '@angular/core';

@Injectable({
    providedIn: 'root',
})

export class ControlFLowMetaDataService {
    maintainStateForSidePanel = {};

    /**
     * Sets the state(open/close) of collapsible section in cfg side panel.
     * @param selectedState user selected state.
     */
    setPanelCollapseState(selectedState: Record<string, unknown>): void {
        this.maintainStateForSidePanel = selectedState;
    }

    /**
     * Method to get the current state of collapsible section in cfg side panel.
     * @returns current state of collapsible section.
     */
    getPanelCollapseState(): any {
        return this.maintainStateForSidePanel;
    }
}
