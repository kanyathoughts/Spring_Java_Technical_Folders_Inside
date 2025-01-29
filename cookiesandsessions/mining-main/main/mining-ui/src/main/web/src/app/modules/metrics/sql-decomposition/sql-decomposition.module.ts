import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { SqlDecompositionRoutingModule } from './sql-decomposition-routing.module';
import { SqlDecompositionComponent } from './sql-decomposition.component';
import { TranslateModule } from '@ngx-translate/core';
import { MetricsSharedModule } from '../shared/metrics-shared.module';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';


@NgModule({
  declarations: [SqlDecompositionComponent],
  imports: [
    CommonModule,
    SqlDecompositionRoutingModule,
    TranslateModule.forChild(),
    MetricsSharedModule,
    AntDesignImportsModule
  ]
})
export class SqlDecompositionModule { }
