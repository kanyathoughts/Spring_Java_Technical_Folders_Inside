import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { ApplicationDecompositionRoutingModule } from './application-decomposition-routing.module';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { MetricsSharedModule } from '../shared/metrics-shared.module';
import { ApplicationDecompositionComponent } from './application-decomposition.component';


@NgModule({
  declarations: [ApplicationDecompositionComponent],
  imports: [
    CommonModule,
    ApplicationDecompositionRoutingModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
    MetricsSharedModule
  ]
})
export class ApplicationDecompositionModule { }
