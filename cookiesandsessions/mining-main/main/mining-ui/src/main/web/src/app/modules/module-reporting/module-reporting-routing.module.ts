import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ModuleReportingComponent } from './module-reporting.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'modulesPageTitle';
const routes: Routes = [
    {
      path: '',
      component: ModuleReportingComponent,
      data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
    }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class MiningModuleRoutingModule {}
