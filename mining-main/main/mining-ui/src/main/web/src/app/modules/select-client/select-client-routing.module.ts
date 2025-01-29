import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ClientsOverviewComponent } from './clients-overview/clients-overview.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'selectClient.pageTitle';
const routes: Routes = [
  {
        path: '',
        component: ClientsOverviewComponent,
        data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
    }
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule],
    providers: []
  })
  export class SelectClientRoutingModule {}
