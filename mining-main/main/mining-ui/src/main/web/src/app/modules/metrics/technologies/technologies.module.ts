import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { MetricsSharedModule } from '../shared/metrics-shared.module';
import { TechnologiesRoutingModule } from './technologies-routing.module';
import { TechnologiesComponent } from './technologies.component';

@NgModule({
  declarations: [TechnologiesComponent],
  imports: [
    CommonModule,
    TechnologiesRoutingModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
    MetricsSharedModule
  ]
})
export class TechnologiesModule { }
