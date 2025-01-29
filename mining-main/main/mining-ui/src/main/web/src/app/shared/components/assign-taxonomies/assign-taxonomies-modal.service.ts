import { Injectable } from '@angular/core';
import { TaxonomyGetAssignment } from '@innowake/mining-api-angular-client/model/taxonomyGetAssignment';
import { Subject } from 'rxjs';

@Injectable({
  providedIn: 'root',
})
export class AssignTaxonomiesModalService {
  loadUpdatedTaxonomyData = new Subject<boolean>();
  closeModal: Subject<boolean> = new Subject<boolean>();
  updatedTaxonomyData: TaxonomyGetAssignment[];

  /**
   * Triggers load subject.
   * @param flag checks if data needs to be loaded.
   */
  triggerLoadSubject(flag: boolean): void {
    this.loadUpdatedTaxonomyData.next(flag);
  }

  /**
   * Method to get the load taxonomy subject.
   * @returns Subject string.
   */
  getloadTaxonomySubject(): Subject<boolean> {
    return this.loadUpdatedTaxonomyData;
  }

  /**
   * Gets the updated Taxonomy data.
   *
   * @returns TaxonomyGetAssignment[] with updated taxonomy data.
   */
  getupdatedTaxonomyData(): TaxonomyGetAssignment[] {
    return this.updatedTaxonomyData;
  }

  /**
   * Sets updated Taxonomy data.
   * @param taxonomyData updated data after assignment.
   */
  setUpdatedTaxonomyData(taxonomyData: TaxonomyGetAssignment[]): void {
    this.updatedTaxonomyData = taxonomyData;
  }
}
