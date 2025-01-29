import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { DataLineageComponent } from './data-lineage.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'dataLineage.pageTitle';
const routes: Routes = [
    {
      path: '',
      component: DataLineageComponent,
      data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
    }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})

export class DataLineageReportingModule { }
