import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { FunctionalBlocksLayoutComponent } from './functional-blocks-layout/functional-blocks-layout.component';
import { ClientProjectAuthorizationGuard } from '@app/core/authorization/client-project-authorization.guard';

const routes: Routes = [
  {
      path: ':uuid',
      component: FunctionalBlocksLayoutComponent ,
      canActivate: [ClientProjectAuthorizationGuard ]
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class FunctionalBlocksRoutingModule { }
