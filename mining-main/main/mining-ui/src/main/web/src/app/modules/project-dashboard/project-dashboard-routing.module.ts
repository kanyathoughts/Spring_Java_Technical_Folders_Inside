import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ProjectDashboardComponent } from './project-dashboard.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'dashboardPageTitle';
const routes: Routes = [
  { path: '', component: ProjectDashboardComponent, data: { title, role: ProjectRole.UserRoleEnum.VIEWER } }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class ProjectDashboardRoutingModule {}
