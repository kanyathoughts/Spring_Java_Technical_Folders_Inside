import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { FunctionalBlockUiProductVisionComponent } from './functional-block-ui-product-vision.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'compositionPageTitle';
const routes: Routes = [
  {
    path: '',
    data: { title, role: ProjectRole.UserRoleEnum.VIEWER } ,
    children : [
      {
        path: '',
        pathMatch: 'full',
        redirectTo: 'tiles'
      },
      {
        path: 'tiles',
        component: FunctionalBlockUiProductVisionComponent,
        data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
      },
      {
        path: 'folders',
        component: FunctionalBlockUiProductVisionComponent,
        data: { title, role: ProjectRole.UserRoleEnum.VIEWER } ,
      }
    ]
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class FunctionalBlockUiProductVisionRoutingModule {}
