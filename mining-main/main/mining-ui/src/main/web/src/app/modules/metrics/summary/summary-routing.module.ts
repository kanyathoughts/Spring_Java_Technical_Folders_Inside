import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { SummaryComponent } from './summary.component';


const routes: Routes = [
  {
      path: '',
      component: SummaryComponent,
      data: { title: 'metrics.summary.pageTitle' }
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class SummaryRoutingModule { }
