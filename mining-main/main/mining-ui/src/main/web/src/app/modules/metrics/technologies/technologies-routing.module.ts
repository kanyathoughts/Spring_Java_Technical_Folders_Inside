import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { TechnologiesComponent } from './technologies.component';

const routes: Routes = [
  {
      path: '',
      component: TechnologiesComponent,
      data: { title: 'metrics.technologies.pageTitle' }
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class TechnologiesRoutingModule { }
