import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ProjectRole } from '@innowake/mining-api-angular-client';
import { CallChainExportComponent } from '@app/modules/module-details/call-chain-export/call-chain-export.component';

const routes: Routes = [
  {
     path: '',
     data: { role: ProjectRole.UserRoleEnum.VIEWER },
     component: CallChainExportComponent,
     children: [
       {
         path: 'call-chain',
         loadChildren: () => import('./global-call-chain.module').then(m => m.GlobalCallChainModule)
       }
      ]
   }
 ];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class GlobalCallChainRoutingModule { }
