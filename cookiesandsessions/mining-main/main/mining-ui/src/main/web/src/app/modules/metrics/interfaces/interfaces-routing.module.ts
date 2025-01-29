import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { InterfacesComponent } from './interfaces.component';


const routes: Routes = [
  {
      path: '',
      component: InterfacesComponent,
      data: { title: 'metrics.interfaces.pageTitle' }
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class InterfacesRoutingModule { }
