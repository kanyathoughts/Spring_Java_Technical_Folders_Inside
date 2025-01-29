import { NgModule, Optional, SkipSelf } from '@angular/core';
import { CommonModule } from '@angular/common';
import { HttpClient, HttpClientModule } from '@angular/common/http';
import { RouteReuseStrategy, RouterModule } from '@angular/router';
import { TranslateModule } from '@ngx-translate/core';
import { RouteReusableStrategy } from './utils/route-reusable-strategy';
import { AuthenticationService } from './authentication/authentication.service';
import { AuthenticationGuard } from './authentication/authentication.guard';
import { I18nService } from './services/i18n/i18n.service';
import { HttpService } from './http/http.service';
import { HttpCacheService } from './http/http-cache.service';
import { ApiPrefixInterceptor } from './http/api-prefix.interceptor';
import { ErrorHandlerInterceptor } from './http/error-handler.interceptor';
import { CacheInterceptor } from './http/cache.interceptor';
import { NzAlertModule } from 'ng-zorro-antd/alert';
import { NzMessageService } from 'ng-zorro-antd/message';
import { KeycloakAuthorizationService } from './authorization/keycloak-authorization.service';
import { authorizationFactory } from './authorization/authorization.factory';
import { KeycloakService } from './authentication/keycloak.service';
import FileSaveSupport  from './utils/file-save-support.utils';
import { GraphQlControllerService } from './services/graphql.service';
import { CancelRequestOnNavigationInterceptor } from './http/cancel-request-on-navigation.interceptor';
import { NavigationGuard } from './pendingChanges/navigationGuard';
import { SetSerializationInterceptor } from './http/set-serialization.interceptor';

@NgModule({
  imports: [CommonModule, HttpClientModule, TranslateModule, RouterModule, NzAlertModule],
  providers: [
    AuthenticationService,
    AuthenticationGuard,
    NavigationGuard,
    I18nService,
    HttpCacheService,
    ApiPrefixInterceptor,
    ErrorHandlerInterceptor,
    CacheInterceptor,
    CancelRequestOnNavigationInterceptor,
    SetSerializationInterceptor,
    FileSaveSupport,
    GraphQlControllerService,
    {
      provide: HttpClient,
      useClass: HttpService
    },
    {
      provide: RouteReuseStrategy,
      useClass: RouteReusableStrategy
    },
    NzMessageService,
    {
      provide: KeycloakAuthorizationService,
      useFactory: authorizationFactory,
      deps: [KeycloakService]
    }
  ]
})
export class CoreModule {
  constructor(@Optional() @SkipSelf() parentModule: CoreModule) {
    // Import guard
    if (parentModule) {
      throw new Error(`${parentModule} has already been loaded. Import Core module in the AppModule only.`);
    }
  }
}
