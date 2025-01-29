import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router, NavigationEnd, ActivatedRoute, Data, Params } from '@angular/router';
import { Title } from '@angular/platform-browser';
import { TranslateService } from '@ngx-translate/core';
import { merge, Subscription } from 'rxjs';
import { filter, map, switchMap } from 'rxjs/operators';

import { environment } from '@env/environment';
import { Logger, I18nService, untilDestroyed } from '@app/core';
import { ClientProjectRelationshipService } from './core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from './shared/models/client-project-relationship.model';
import { LicenseExpiryService } from './core/license/license-expiry.service';

const log = new Logger('App');

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html'
})
export class AppComponent implements OnInit, OnDestroy {
  private clientProjectSubscription: Subscription;
  constructor(
    private router: Router,
    private activatedRoute: ActivatedRoute,
    private titleService: Title,
    private translateService: TranslateService,
    private i18nService: I18nService,
    private clientProjectRelationship: ClientProjectRelationshipService,
    private licenseExpiryService: LicenseExpiryService
    ) { }

  ngOnInit(): void {
    // Setup logger
    if (environment.production) {
      Logger.enableProductionMode();
    }

    log.debug('init');

    // Setup translations
    this.i18nService.init(environment.defaultLanguage, environment.supportedLanguages);

    const onNavigationEnd = this.router.events.pipe(filter(event => event instanceof NavigationEnd));

    // Change page title on navigation or language change, based on route data
    merge(this.translateService.onLangChange, onNavigationEnd)
      .pipe(
        map(() => {
          let route = this.activatedRoute;
          while (route.firstChild) {
            route = route.firstChild;
          }
          return route;
        }),
        filter(route => route.outlet === 'primary'),
        switchMap(route => (route.data.pipe(
            map((data: Data) => ({ params: route.snapshot.params, data })),
          ))
        ),
        untilDestroyed(this)
      ).subscribe((routeDetails: {params: Params, data: Data}) => {
        if (routeDetails.data.title) {
          this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe(currentClientProject => {
            this.setTitleParameters(currentClientProject, routeDetails.data, routeDetails.params);
          });
        }
      });
    // check license expiry information
    this.licenseExpiryService.checkExpiry();
  }

  ngOnDestroy(): void {
    this.i18nService.destroy();
    this.clientProjectSubscription?.unsubscribe();
  }

  /**
   * Sets the titleParameters based on route to translate the title of current HTML document on navigation.
   */
  private setTitleParameters(currentClientProject: ClientProjectRelationship, event: Data, params: Params): void {
    const titleParameters: any = {};
    if (currentClientProject) {
      titleParameters.projectName = currentClientProject.getProjectName();
      titleParameters.clientName = currentClientProject.getClientName();
    }
    /* Checking for the event.module (moduleResolver for path containing module-id param),
      to add moduleName param to the titleParams. */
    if (event.module) {
      titleParameters.moduleName = event.module.name;
    }
    /* Checking for the blockId in the routing parameters,
      to add blockId param to the titleParams. */
    const blockId = params['blockId'];
    if(blockId){
      titleParameters.blockId = blockId;
    }
    this.titleService.setTitle(this.translateService.instant(event.title as string, titleParameters as object) as string);
  }
}
