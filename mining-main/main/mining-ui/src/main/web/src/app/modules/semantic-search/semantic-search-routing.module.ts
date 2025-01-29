import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { SemanticSearchComponent } from './semantic-search.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'semanticSearch.navigation-entry';
const routes: Routes = [
  {
    path: '',
    component: SemanticSearchComponent,
    data: { title, role: ProjectRole.UserRoleEnum.VIEWER },
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class SemanticSearchRoutingModule { }
