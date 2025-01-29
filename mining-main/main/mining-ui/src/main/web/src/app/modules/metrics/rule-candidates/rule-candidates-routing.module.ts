import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { RuleCandidatesComponent } from './rule-candidates.component';


const routes: Routes = [
  {
    path: '',
    component: RuleCandidatesComponent,
    data: { title: 'metrics.ruleCandidates.pageTitle' }
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class RuleCandidatesRoutingModule { }
