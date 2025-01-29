import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { CallChainExportComponent } from './call-chain-export.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const routes: Routes = [
    {
        path: '',
        component: CallChainExportComponent,
        data: { role: ProjectRole.UserRoleEnum.VIEWER },
        children: [
            {
                path: 'details/call-chain',
                loadChildren: () => import('./call-chain-export.module').then(m => m.CallChainExportModule)
            }
        ]
    }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class CallChainExportRoutingModule { }
