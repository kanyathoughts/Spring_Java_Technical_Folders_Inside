import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { UtilitiesComponent } from './utilities.component';


const routes: Routes = [
  {
    path: '',
    component: UtilitiesComponent,
    data: { title: 'metrics.utilities.pageTitle' }
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class UtilitiesRoutingModule { }
