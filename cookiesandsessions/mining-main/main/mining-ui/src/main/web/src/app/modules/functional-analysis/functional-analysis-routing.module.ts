import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { FunctionalAnalysisComponent } from './functional-analysis.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'functionalAnalysis.pageTitle';
const routes: Routes = [
    {
      path: '',
      component: FunctionalAnalysisComponent,
      data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
    }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class FunctionalAnalysisRoutingModule {}
