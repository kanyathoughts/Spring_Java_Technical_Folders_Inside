import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ApplicationDecompositionComponent } from './application-decomposition.component';


const routes: Routes = [
  {
    path: '',
    component: ApplicationDecompositionComponent,
    data: { title: 'metrics.applicationDecomposition.pageTitle' }
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class ApplicationDecompositionRoutingModule { }
