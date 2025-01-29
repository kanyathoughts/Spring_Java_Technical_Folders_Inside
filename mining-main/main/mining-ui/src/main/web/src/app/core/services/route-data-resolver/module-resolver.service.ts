import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Router, RouterStateSnapshot } from '@angular/router';
import { EclipseService } from '@app/core/authentication/eclipse.service';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { EMPTY, Observable, of } from 'rxjs';
import { catchError, tap } from 'rxjs/operators';
import { Logger } from '../logger.service';
import { ModuleControllerService, ModulePojo } from '@innowake/mining-api-angular-client';

const logger = new Logger();

@Injectable({
  providedIn: 'root',
})
export class ModuleResolverService  {

  previousModule: ModulePojo;

  constructor(
    private router: Router,
    private moduleService: ModuleControllerService,
    private messageService: NzMessageService,
    private translateService: TranslateService,
    private eclipseService: EclipseService
  ) {  }

  resolve(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<ModulePojo> {
    const paramData = route.paramMap.get('moduleHash');
    const params = paramData.split('-');
    const id = Number(params[1]);
    const redirectionId = params.slice(1).join('-');
    let moduleObservable: Observable<ModulePojo>;
    if (params.length !== 2 && params.length !== 6) {
      void this.router.navigate(['/clients']);
      this.messageService.error(`${this.translateService.instant('dataResolverError', { dataType: params[0] })}`);
    }
    if (params[0] === 'module') {
      const projectId = Number(route.paramMap.get('projectId').split('-')[1]);
      const includeSources = this.checkIncludeSource(route);
      if ( this.canReusePreviousModule(route, projectId, id, params)) {
        moduleObservable = of(this.previousModule);
      } else {
        moduleObservable = Number(params[1]) || this.isValidUuid(redirectionId) ?
          this.moduleService.findModuleById(projectId, redirectionId, includeSources) :
          this.moduleService.findModuleByLinkHash(projectId, params[1], includeSources);
      }
      return moduleObservable.pipe(
        tap((module: ModulePojo) => {
          this.previousModule = module;
          if (Number(params[1]) || this.isValidUuid(redirectionId)) {
            const newRoute = state.url.replace('module-' + redirectionId + '', 'module-' + module.linkHash);
            void this.router.navigateByUrl(newRoute);
          }
        }),
        catchError(error => {
          logger.error(error);
          if (this.eclipseService.isEclipseView) {
            void this.router.navigate(['/error']);
          } else {
            void this.router.navigate(['/clients']);
          }
          this.messageService.error(`${this.translateService.instant('dataResolverError', { dataType: params[0] })}`);
          return EMPTY;
        })
      );
    }
  }

  private isValidUuid(uuid: string): boolean {
    const uuidRegex = /^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[1-5][0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$/;
    return uuidRegex.test(uuid);
  }

  private checkIncludeSource(route: ActivatedRouteSnapshot): boolean {
    while (route) {
      if (route.data.includeSource) {
        return true;
      }
      route = route.firstChild;
    }
    return false;
  }

  private canReusePreviousModule(route: ActivatedRouteSnapshot, projectId: number, id: number, params: string[]): boolean {
    return (! route.data?.forceModuleRefresh &&
      this.previousModule && this.previousModule.projectId === projectId
      && (this.previousModule.id === id || this.previousModule.linkHash === params[1]));
  }
}
