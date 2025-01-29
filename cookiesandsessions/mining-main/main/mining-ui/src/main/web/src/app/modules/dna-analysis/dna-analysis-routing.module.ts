import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { DnaAnalysisComponent } from './dna-analysis.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'dnaAnalysis.pageTitle';
const routes: Routes = [
    {
      path: '',
      component: DnaAnalysisComponent,
      data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
    }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class DnaAnalysisRoutingModule {}
