import { Injectable, EventEmitter } from '@angular/core';
import { Subject } from 'rxjs';


@Injectable({
  providedIn: 'root',
})
export class TaxonomyModalService {
  cancelWarningSubject = new Subject<string>();
  validationResponse: object;
  validationStart = false;
  projectId: number;
  taxonomyImportLink = 'http://appmod-documentation.deloitte.com/innowake-documentation/trunk/mining/mining-manual/working-with-mining/#taxonomy-batch-import';
  taxonomyModalEventEmitter = new EventEmitter<string>();

  /**
   * @param  operation is  type of operation performed by user
   */
  triggerCancelWarningSubject(operation: string): void {
    this.cancelWarningSubject.next(operation);
  }

  /**
   * method to set the API response for validation
   * @param  responseAfterValidation  setting up the response from api
   */
  setValidationResponse(responseAfterValidation: object): void {
    this.validationResponse = responseAfterValidation;
  }

  /**
   * method to get the response after validation
   * @returns TaxonomyImportValidationResult type of response after validation
   */
  getValidationResponse(): any {
    return this.validationResponse;
  }

  /**
   * method to download log for warning and error case
   */
  downloadLog(): void {
    const downloadText = JSON.stringify(this.validationResponse, null, '\t');
    const link = document.createElement('a');
    const mimeType = 'text/plain';
    link.setAttribute('download', 'log');
    link.setAttribute('href', 'data:' + mimeType + ';charset=utf-8,' + encodeURIComponent(downloadText));
    link.click();
  }

  /**
   * method to setValidation flag
   * @param  isValidationStart  boolean flag to set  for validation
   */
  setStartValidation(isValidationStart: boolean): void {
    this.validationStart = isValidationStart;
  }

  /**
   * method to check if validation process started
   * @returns type is boolean depending upon the validation process
   */
  isImportValidationStarted(): boolean {
    return this.validationStart;
  }

  /**
   * * method to set the project id
   */
  setProjectId(projectId: number): void {
    this.projectId = projectId;
  }

  /**
   * method to get the project id
   * @returns project id which has been set
   */
  getProjectId(): number {
    return this.projectId;
  }
}
