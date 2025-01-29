import { Injectable } from '@angular/core';
import { OauthToken } from '../authentication/oauthtoken';
import { OauthtokenService } from './oauthtoken.service';
import { LegacyAuthControllerService, LegacyToken } from '@innowake/mining-api-angular-client';

export interface LoginContext {
  username: string;
  password: string;
  remember?: boolean;
}

/**
 * Provides a base for authentication workflow.
 * The login/logout methods should be replaced with proper implementation.
 */
@Injectable()
export class AuthenticationService {
  constructor(private oauthTokenService: OauthtokenService, private legacyAuth: LegacyAuthControllerService) {}

  /**
   * Authenticates the user.
   * @param context The login parameters.
   */
  login(context: LoginContext, sucessCallBack: () => void, errorCallBack: (e: any) => void): void {

    this.legacyAuth.login({
      username: context.username,
      password: context.password
    }).subscribe(
      (data: LegacyToken) => {
        this.oauthTokenService.setOauthToken(({
          username: data.username,
          access_token: data.access_token,
          token_type: data.token_type
        } as OauthToken));
        sucessCallBack();
      },
      error => {
        errorCallBack(error);
      }
    );
  }
}
