import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ConfigurationComponent } from './configuration.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const routes: Routes = [
    {
      path: '',
      data:{includeSource: true},
      children: [
        {
          path: '',
          pathMatch: 'full',
          redirectTo: 'annotations'
        },
        {
          path: 'annotations',
          component: ConfigurationComponent,
          data: { title: 'configuration.annotationsPageTitle', role: ProjectRole.UserRoleEnum.VIEWER },
        },
        {
          path: 'modules',
          component: ConfigurationComponent,
          data: { title: 'configuration.modulePageTitle', role: ProjectRole.UserRoleEnum.VIEWER },
        },
        {
          path: 'taxonomies',
          component: ConfigurationComponent,
          data: { title: 'configuration.taxonomiesPageTitle', role: ProjectRole.UserRoleEnum.VIEWER },
        },
        {
          path: 'data-dictionary',
          component: ConfigurationComponent,
          data: { title: 'configuration.dataDictionaryPageTitle', role: ProjectRole.UserRoleEnum.VIEWER }
        },
        {
          path: 'schema',
          component: ConfigurationComponent,
          data: { title: 'configuration.schemaPageTitle', role: ProjectRole.UserRoleEnum.MANAGER },
        },
        {
          path: 'metadata',
          component: ConfigurationComponent,
          data: { title: 'configuration.metadataPageTitle', role: ProjectRole.UserRoleEnum.VIEWER }
        },
        {
          path: 'scheduler-data',
          component: ConfigurationComponent,
          data: { title: 'configuration.schedulerDataPageTitle', role: ProjectRole.UserRoleEnum.MANAGER }
        },
        {
          path: 'reachability',
          component: ConfigurationComponent,
          data: { title: 'configuration.reachabilityPageTitle', role: ProjectRole.UserRoleEnum.VIEWER }
        },
        {
          path: 'view',
          component: ConfigurationComponent,
          data : { title: 'configuration.viewPageTitle', role: ProjectRole.UserRoleEnum.VIEWER }
        }
      ]
    },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})

export class ConfigurationRoutingModule { }
