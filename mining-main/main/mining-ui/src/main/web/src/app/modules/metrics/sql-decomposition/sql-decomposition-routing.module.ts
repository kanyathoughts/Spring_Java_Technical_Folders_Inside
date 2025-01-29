import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { SqlDecompositionComponent } from './sql-decomposition.component';


const routes: Routes = [
  {
      path: '',
      component: SqlDecompositionComponent,
      data: { title: 'metrics.sqlDecomposition.pageTitle' }
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class SqlDecompositionRoutingModule { }
