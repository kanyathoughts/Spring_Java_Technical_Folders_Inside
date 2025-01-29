import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { DependencyExplorerComponent } from './dependency-explorer/dependency-explorer.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const routes: Routes = [
    {
        path: '',
        component: DependencyExplorerComponent,
        data: { title: 'dependencyGraph.pageTitle', role: ProjectRole.UserRoleEnum.VIEWER }
    }
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule],
    providers: []
  })
  export class DependencyGraphRoutingModule {}
