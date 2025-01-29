import { Injectable } from '@angular/core';
import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { Observable, BehaviorSubject } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class MetricsFilterService {

  metricsTaxonomyFilter: BehaviorSubject<any> = new BehaviorSubject<TaxonomyFilterSelected[]>([{taxonomyId: 0, taxonomyTitle: ''}]);

  /**
   * Set the taxonomy filter value.
   * @param filterDetail value which is selected.
   */
  setMetricsTaxonomyFilter(filterDetail: TaxonomyFilterSelected[]): void {
    this.metricsTaxonomyFilter.next(filterDetail);
  }

  /**
   * Get the taxonomy filter value.
   * @returns selected value.
   */
  getMetricsTaxonomyFilter(): Observable<TaxonomyFilterSelected[]> {
    return this.metricsTaxonomyFilter;
  }
}
