import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';
import { Routes } from '@angular/router';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const routes: Routes = [
 {
    path: '',
    data: { role: ProjectRole.UserRoleEnum.VIEWER },
    children: [
      {
        path: 'summary',
        loadChildren: () => import('./summary/summary.module').then(m => m.SummaryModule)
      },
      {
        path: 'technologies',
        loadChildren: () => import('./technologies/technologies.module').then(m => m.TechnologiesModule)
      },
      {
        path: 'utilities',
        loadChildren: () => import('./utilities/utilities.module').then(m => m.UtilitiesModule)
      },
      {
        path: 'interfaces',
        loadChildren: () => import('./interfaces/interfaces.module').then(m => m.InterfacesModule)
      },
      {
        path: 'sql',
        loadChildren: () => import('./sql-decomposition/sql-decomposition.module').then(m => m.SqlDecompositionModule)
      },
      {
        path: 'decomposition',
        loadChildren: () => import('./application-decomposition/application-decomposition.module').then(m => m.ApplicationDecompositionModule)
      },
      {
        path: 'candidates',
        loadChildren: () => import('./rule-candidates/rule-candidates.module').then(m => m.RuleCandidatesModule)
      },
      {
        path: 'code-quality',
        loadChildren: () => import('./code-quality/code-quality.module').then(m => m.CodeQualityModule)
      },
      {
        path: 'ims',
        loadChildren: () => import('./ims/ims.module').then(m => m.ImsModule)
      }
    ]
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class MetricsRoutingModule { }
