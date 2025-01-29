import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { AnnotationReportingComponent } from './annotation-reporting.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';
import { NavigationGuard } from '@app/core/pendingChanges/navigationGuard';

const title = 'annotationReporting.pageTitle';
const routes: Routes = [
  {
    path: '',
    component: AnnotationReportingComponent,
    canDeactivate: [NavigationGuard],
    data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class AnnotationReportingRoutingModule { }
