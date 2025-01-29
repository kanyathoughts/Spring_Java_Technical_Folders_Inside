import { Injectable } from '@angular/core';
import { CfgMetaDataPanelState } from '@app/shared/components/control-flow-meta-data/cfg-meta-data.interface';

@Injectable({
    providedIn: 'root',
})

export class CfgMetaDataService {
    maintainState: CfgMetaDataPanelState = {
        inputFiles: false,
        outputFiles: false
    };

    setState(selectedState: CfgMetaDataPanelState): void {
        this.maintainState = selectedState;
    }

    getState(): CfgMetaDataPanelState {
        return this.maintainState;
    }
}
