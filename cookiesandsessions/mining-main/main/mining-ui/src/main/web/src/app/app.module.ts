import { BrowserModule } from '@angular/platform-browser';
import { NgModule, APP_INITIALIZER } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpClient, HttpClientModule } from '@angular/common/http';
import { TranslateModule } from '@ngx-translate/core';
import { CoreModule } from '@app/core';
import { ShellModule } from './shell/shell.module';
import { AppComponent } from './app.component';
import { AppRoutingModule } from './app-routing.module';
import { ApiModule } from '@innowake/mining-api-angular-client';
import { BASE_PATH } from '@innowake/mining-api-angular-client';
import { WindowToken, windowFunction } from './core/utils/window';
import { EllipsisPipe } from './shared/pipes/ellipsis-pipe';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { KeycloakService } from './core/authentication/keycloak.service';
import { getBasePath } from './core/utils/base-path.utils';
import { EclipseService } from './core/authentication/eclipse.service';
import { NumberFormatter } from './shared/pipes/number-formatter';
import { initializeLabelMapping } from './core/initializer/label-mapping.initializer';
import { LabelMappingService } from './core/services/label-mapping.service';
import { initializeKeycloak } from './core/initializer/keycloak.initializer';
import { CfgSupportedTypeService } from './core/services/cfg-supported-type.service';
import { APOLLO_OPTIONS, ApolloModule } from 'apollo-angular';
import { HttpLink } from 'apollo-angular/http';
import { InMemoryCache } from '@apollo/client/core';
import { initializeCfgSupportedTypes } from './core/initializer/cfg-supported-types.initializer';

@NgModule({
  imports: [
    BrowserModule,
    ApolloModule,
    FormsModule,
    HttpClientModule,
    TranslateModule.forRoot(),
    CoreModule,
    ShellModule,
    ApiModule,
    BrowserAnimationsModule,
    AppRoutingModule // must be imported as the last module as it contains the fallback route
  ],
  declarations: [AppComponent, EllipsisPipe],
  providers: [
    {
      provide: BASE_PATH,
      useFactory: getBasePath
    },
    {
      provide: WindowToken,
      useFactory: windowFunction
    },
    {
      provide: APP_INITIALIZER,
      useFactory: initializeKeycloak,
      multi: true,
      deps: [KeycloakService, EclipseService, HttpClient],
    },
    {
      provide: APP_INITIALIZER,
      useFactory: initializeLabelMapping,
      multi: true,
      deps: [LabelMappingService],
    },
    {
      provide: APP_INITIALIZER,
      useFactory: initializeCfgSupportedTypes,
      multi: true,
      deps: [CfgSupportedTypeService],
    },
    {
      provide: APOLLO_OPTIONS,
      useFactory: (httpLink: HttpLink) => (
         {
          cache: new InMemoryCache(),
          link: httpLink.create({
            uri: getBasePath() + '/api/v2/graphql',
            withCredentials: true,
          })
        }
      ),
      deps: [HttpLink],
    },
    NumberFormatter
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
