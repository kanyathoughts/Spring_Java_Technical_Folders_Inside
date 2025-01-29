import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ErrorComponent } from './error.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'error';
const routes: Routes = [
{
        path: '',
        component: ErrorComponent,
        data: { title, role: ProjectRole.UserRoleEnum.VIEWER },
    }, {
        path:':lincenseId',
        component: ErrorComponent,
        data: { title, role: ProjectRole.UserRoleEnum.VIEWER },
    }
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule],
    providers: []
  })
  export class ErrorRoutingModule {}
