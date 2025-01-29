import { Injectable } from '@angular/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { TranslateService } from '@ngx-translate/core';

@Injectable({
    providedIn: 'root'
})
export class ModuleMetricsDetailsService {
    constructor(private numberFormatter: NumberFormatter,
        private translateService: TranslateService) { }

    /**
     * Method to get the Source Metrics Details.,
     * @param value is the value of the LOC/CommentLOC/PLOC.
     */
    getSourceMetricsDetails(value: number): string {
        if (value === undefined || value === null) {
            return this.translateService.instant('notAvailable');
        } else {
            return this.numberFormatter.transform(value);
        }
    }
}
