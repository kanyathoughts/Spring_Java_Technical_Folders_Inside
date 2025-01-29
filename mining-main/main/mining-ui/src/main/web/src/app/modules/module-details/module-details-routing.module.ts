import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ModuleResolverService } from '@app/core/services/route-data-resolver/module-resolver.service';
import { ModuleDetailsComponent } from './module-details.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';
import { NavigationGuard } from '@app/core/pendingChanges/navigationGuard';

const routes: Routes = [
    {
      path: '',
      resolve: {
        module: ModuleResolverService
      },
      data:{ includeSource: true, reuse: false, forceModuleRefresh: true },
      children: [
        {
          path: '',
          pathMatch: 'full',
          redirectTo: 'overview'
        },
        {
          path: 'overview',
          component: ModuleDetailsComponent,
          data: { title: 'module.overviewPageTitle', role: ProjectRole.UserRoleEnum.VIEWER },
        },
        {
          path: 'annotations',
          component: ModuleDetailsComponent,
          canDeactivate: [NavigationGuard],
          data: { title: 'module.annotationsPageTitle', role: ProjectRole.UserRoleEnum.VIEWER },
        },
        {
          path: 'data-dictionary',
          component: ModuleDetailsComponent,
          canDeactivate: [NavigationGuard],
          data: { title: 'module.dataDictionaryPageTitle', role: ProjectRole.UserRoleEnum.VIEWER },
        },
        {
          path: 'dependencies',
          component: ModuleDetailsComponent,
          data: { title: 'module.dependenciesPageTitle', role: ProjectRole.UserRoleEnum.VIEWER },
        },
        {
          path: 'call-chain',
          component: ModuleDetailsComponent,
          data: { title: 'module.callChainPageTitle', role: ProjectRole.UserRoleEnum.VIEWER },
        },
        {
          path: 'data-schema',
          component: ModuleDetailsComponent,
          data: { title: 'module.schemaInfoPageTitle', role: ProjectRole.UserRoleEnum.VIEWER },
        },
        {
          path: 'errors',
          component: ModuleDetailsComponent,
          data: { title: 'module.errorsPageTitle', role: ProjectRole.UserRoleEnum.VIEWER },
        },
      ]
    }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class ModuleDetailsRoutingModule {}
