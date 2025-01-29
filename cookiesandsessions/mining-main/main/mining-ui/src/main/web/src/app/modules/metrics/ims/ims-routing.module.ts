import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ImsComponent } from './ims.component';

const routes: Routes = [
  {
      path: '',
      component: ImsComponent,
      data: { title: 'metrics.ims.pageTitle', requiredFeature: 'IMS' }
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ImsRoutingModule { }
