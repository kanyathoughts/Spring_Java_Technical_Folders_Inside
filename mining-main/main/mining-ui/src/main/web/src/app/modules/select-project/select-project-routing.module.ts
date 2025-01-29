import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ClientProjectAuthorizationGuard } from '@app/core/authorization/client-project-authorization.guard';
import { ProjectsOverviewComponent } from './projects-overview/projects-overview.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'selectProject.pageTitle';
const routes: Routes = [
{
        path: '',
        component: ProjectsOverviewComponent,
        canActivateChild: [ClientProjectAuthorizationGuard ],
        data: { title, clientAuthorization: ProjectRole.UserRoleEnum.VIEWER }
    }
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule],
    providers: []
  })
  export class SelectProjectRoutingModule {}
