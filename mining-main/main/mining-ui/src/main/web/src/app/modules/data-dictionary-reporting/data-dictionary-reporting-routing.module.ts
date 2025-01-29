import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { DataDictionaryReportingComponent } from './data-dictionary-reporting.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'dataDictionaryReporting.pageTitle';
const routes: Routes = [
    {
      path: '',
      component: DataDictionaryReportingComponent,
      data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
    }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})

export class DataDictionaryReportingRoutingModule { }
