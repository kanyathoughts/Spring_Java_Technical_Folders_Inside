import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { MiningCodeViewerComponent } from './mining-code-viewer.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const routes: Routes = [
    {
      path: '',
      component: MiningCodeViewerComponent,
      data: { title: 'codeViewer.pageTitle', role: ProjectRole.UserRoleEnum.VIEWER, includeSource: true }
    },
    {
      path: 'assembled',
      component: MiningCodeViewerComponent,
      data: { title: 'codeViewer.pageTitle', role: ProjectRole.UserRoleEnum.VIEWER, includeSource: true, assembled: true }
    },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class MiningCodeViewerRoutingModule { }
