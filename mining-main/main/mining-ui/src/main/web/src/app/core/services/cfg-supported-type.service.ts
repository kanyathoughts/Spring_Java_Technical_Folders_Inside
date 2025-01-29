import { HttpClient, HttpErrorResponse } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Logger } from './logger.service';
import { getBasePath } from '../utils/base-path.utils';
const log = new Logger('LabelMappingService');

@Injectable({
  providedIn: 'root'
})
export class CfgSupportedTypeService {

  supportedTypes: Array<{[key: string]: string;}>;

  httpClient: HttpClient;

  constructor(
    private httpService: HttpClient
  ) {
    this.httpClient = this.httpService.skipErrorHandler().disableApiPrefix();
  }

  /**
   * Initialise the service by getting the supported types.
   */
  async init(): Promise<void> {
    const fetchSupportedTypes = new Promise((resolve) => {
      const url = getBasePath() + '/api/v1/control-flow-support';
      this.httpClient.get<Array<{[key: string]: string;}>>(url).subscribe(data => {
        this.supportedTypes = Array.from(data);
        resolve(data);
      }, (error: HttpErrorResponse) => {
        log.error(error.message);
      });
    });
    await fetchSupportedTypes;
  }

  /**
   * Check if supported for cfg based on moduleType
   * @param type the type of label which could be of type project, technology or type
   * @returns true/false.
   */
  public checkIfSupported(technology: string, type: string): boolean {
    const supportedTypes = this.supportedTypes.map(node => node['technology'].toLowerCase() + ' ' + node['type'].toLowerCase());
    return supportedTypes.includes(technology.toLowerCase() + ' ' + type.toLowerCase());
  }
}
