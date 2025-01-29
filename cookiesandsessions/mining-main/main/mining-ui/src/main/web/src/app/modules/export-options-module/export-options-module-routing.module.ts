import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ExportOptionsModuleComponent } from './export-options-module.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'exportOptionsPageTitle';

const routes: Routes = [
    {
      path: '',
      component: ExportOptionsModuleComponent,
      data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
    }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class ExportOptionsModuleRoutingModule { }
