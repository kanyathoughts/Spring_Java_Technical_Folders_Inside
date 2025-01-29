import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AuthorizationGuard } from '@app/core/authorization/authorization.guard';
import { ManageClientAndProjectsComponent } from './manage-client-and-projects.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';


const routes: Routes = [
{
        path: '',
        component: ManageClientAndProjectsComponent,
        canActivate: [AuthorizationGuard],
        data: { title: 'clientsAndProjects', clientAuthorization: ProjectRole.UserRoleEnum.ADMIN }
    }
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule],
    providers: []
})
export class ManageClientAndProjectsRoutingModule { }
