import { Injectable } from '@angular/core';
import { OauthToken } from '../authentication/oauthtoken';

@Injectable({
  providedIn: 'root'
})
export class OauthtokenService {

  constructor() {}

  /**
   * Sets the oAuth token to the local storage
   * @param oauthToken oAuthtoken for the logged in user
   */
  setOauthToken(oauthToken: OauthToken): void {
    localStorage.setItem('oauthToken', JSON.stringify(oauthToken));
  }

  /**
   * returns the username of the logged-in user from the localstorage
   */
  getUsername(): string {
    const oauthData = (JSON.parse(localStorage.getItem('oauthToken')) as OauthToken);
    return oauthData ? oauthData.username : '';
  }

  /**
   * returns the access-token of the logged-in user from the current url or from local storage
   */
  getAccessToken(): string {
    return !!localStorage.getItem('oauthToken')
      ? (JSON.parse(localStorage.getItem('oauthToken')) as OauthToken).access_token
      : null;
  }
}
