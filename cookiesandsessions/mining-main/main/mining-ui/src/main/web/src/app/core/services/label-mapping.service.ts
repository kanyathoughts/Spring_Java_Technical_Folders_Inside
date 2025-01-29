import { HttpClient, HttpErrorResponse } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { getBasePath } from '../utils/base-path.utils';
import { Logger } from './logger.service';
const log = new Logger('LabelMappingService');

@Injectable({
  providedIn: 'root'
})
export class LabelMappingService {

  http: HttpClient;
  labelMappings: { [key: string]: { [key: string]: string } };

  constructor(
    private httpService: HttpClient
  ) {
    this.http = this.httpService.disableApiPrefix().skipErrorHandler();
  }

  /**
   * Initialise the service by getting the label map
   */
  async init(): Promise<void> {
    const fetchLabelMapping = new Promise((resolve) => {
      this.http.get<{ [key: string]: { [key: string]: string; }; }>(`${getBasePath()}/api/v1/label-mappings`).subscribe(data => {
        this.labelMappings = data;
        resolve(data);
      }, (error: HttpErrorResponse) => {
        log.error(error.message);
      });
    });
    await fetchLabelMapping;
  }

  /**
   * Get label based on label type.
   * @param labelType the type of label which could be of type project, technology or type
   * @param label the label key to get the mapped label
   * @param labels contains set of label mappings
   * @returns mapped label value.
   */
  public mapLabel(labelType: string, label: string): string {
    const mapping = this.labelMappings[labelType];
    const mapped: string = mapping ? mapping[label]: label;
    return mapped == null ? label : mapped;
  }
}
