import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { TableExtensionHostComponent } from './table-extension-host.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'customTableTitle';
const routes: Routes = [
  { path: '', component: TableExtensionHostComponent, data: { title, role: ProjectRole.UserRoleEnum.VIEWER } }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class TableExtensionHostRoutingModule {}
