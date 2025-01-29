import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ExtensionHostComponent } from './extension-host.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'externalPageTitle';
const routes: Routes = [
  { path: '', component: ExtensionHostComponent, data: { title, role: ProjectRole.UserRoleEnum.VIEWER } }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class ExtensionHostRoutingModule {}
