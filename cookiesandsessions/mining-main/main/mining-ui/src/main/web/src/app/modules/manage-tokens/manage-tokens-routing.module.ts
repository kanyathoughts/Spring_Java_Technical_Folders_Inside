import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { TokensOverviewComponent } from './tokens-overview/tokens-overview.component';

const title = 'manageTokens.pageTitle';
const routes: Routes = [
  { path: '', component: TokensOverviewComponent, data: { title } }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class ManageTokensRoutingModule { }
