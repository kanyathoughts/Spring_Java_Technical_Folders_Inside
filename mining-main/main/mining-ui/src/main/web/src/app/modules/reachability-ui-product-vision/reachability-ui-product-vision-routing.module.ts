import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ReachabilityUiProductVisionComponent } from './reachability-ui-product-vision.component';
import { BlockDetailsComponent } from './block-details/block-details.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';
import { ReachabilityBlockGraphComponent } from './reachability-block-graph/reachability-block-graph-component';

const title = 'reachabilityPageTitle';
const routes: Routes = [
  {
    path: '',
    data: { title, role: ProjectRole.UserRoleEnum.VIEWER },
    children: [
      {
        path: '',
        pathMatch: 'full',
        redirectTo: 'overview'
      },
      {
        path: 'selection',
        component: ReachabilityUiProductVisionComponent,
        data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
      },
      {
        path: 'overview',
        component: ReachabilityUiProductVisionComponent,
        data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
      },
      {
        path: ':blockId/table',
        component: BlockDetailsComponent,
        data: { title: 'reachabilityblockPageTitle' , role: ProjectRole.UserRoleEnum.VIEWER }
      },
      {
        path: 'table',
        component: BlockDetailsComponent,
        data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
      },
      {
        path: ':blockId/graph',
        component: ReachabilityBlockGraphComponent,
        data: { title: 'reachabilityGraphPageTitle', role: ProjectRole.UserRoleEnum.VIEWER }
      }
    ]
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class ReachabilityUiProductVisionRoutingModule {}
