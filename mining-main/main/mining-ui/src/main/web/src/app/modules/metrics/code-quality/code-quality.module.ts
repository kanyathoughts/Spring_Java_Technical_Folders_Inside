import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { MetricsSharedModule } from '../shared/metrics-shared.module';
import { TranslateModule } from '@ngx-translate/core';
import { CodeQualityComponent } from './code-quality.component';
import { CodeQualityRoutingModule } from './code-quality-routing.module';

@NgModule({
  declarations: [CodeQualityComponent],
  imports: [
    CommonModule,
    CodeQualityRoutingModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
    MetricsSharedModule
  ]
})
export class CodeQualityModule { }
