import { Injectable } from '@angular/core';
import { Logger } from '../services/logger.service';

declare const java__callback__authenticate_eclipse_view: () => Promise<string>;
const log = new Logger('EclipseService');

@Injectable({
  providedIn: 'root'
})
export class EclipseService {

  isEclipseView: boolean;
  constructor() {
    const isMethodDefined = typeof java__callback__authenticate_eclipse_view;
    if (isMethodDefined.toLowerCase() !== 'undefined') {
      this.isEclipseView = true;
    } else {
      this.isEclipseView = false;
    }
  }

  /**
   * Gets the token for the authenticated user from eclipse which might be authenticated via keyClaok or OAuth.
   * Eclipse will internally refresh token if access token is expired. Eclipse will also open login page in system's
   * default browser for the user to login if session is expired.
   *
   * @return Access Token.
   */
  async getAccessToken(): Promise<string> {
    if ( ! this.isEclipseView) {
      return null;
    }
    try {
      return await java__callback__authenticate_eclipse_view();
    } catch (err) {
      log.error('Error while fetching token from eclipse: ' + err.message);
      return null;
    }
  }
}
