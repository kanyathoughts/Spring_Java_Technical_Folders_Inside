import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ClientProjectAuthorizationGuard } from '@app/core/authorization/client-project-authorization.guard';
import { EclipseTaxonomyAssignmentComponent } from './eclipse-taxonomy-assignment.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'annotationReporting.pageTitle';
const routes: Routes = [
    {
      path: '',
      children: [
        {
          path: '',
          component: EclipseTaxonomyAssignmentComponent,
          canActivate: [ClientProjectAuthorizationGuard ],
          data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
        }
      ]
    }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class EclipseTaxonomyAssignmentRoutingModule { }
