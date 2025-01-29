import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ControlFlowComponent } from './control-flow.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const controlFlowTitle = 'controlFlow.pageTitle';

const routes: Routes = [
    {
      path: '',
      component: ControlFlowComponent,
      data: { title: controlFlowTitle, role: ProjectRole.UserRoleEnum.VIEWER }
    }
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule],
    providers: []
  })
  export class ControlFlowRoutingModule {}
