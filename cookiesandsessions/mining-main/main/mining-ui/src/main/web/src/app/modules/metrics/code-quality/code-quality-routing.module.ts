import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { CodeQualityComponent } from './code-quality.component';

const routes: Routes = [
  {
    path: '',
    component: CodeQualityComponent,
    data: { title: 'metrics.codeQuality.pageTitle' }
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class CodeQualityRoutingModule { }
