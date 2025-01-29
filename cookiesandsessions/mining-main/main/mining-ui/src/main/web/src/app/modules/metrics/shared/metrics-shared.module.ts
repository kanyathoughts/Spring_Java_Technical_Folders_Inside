import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TranslateModule } from '@ngx-translate/core';
import { MetricsCardComponent } from './components/metrics-card/metrics-card.component';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { MetricsLayoutComponent } from './components/metrics-layout/metrics-layout.component';
import { SharedModule } from '@app/shared';

@NgModule({
  imports: [
    CommonModule,
    TranslateModule,
    AntDesignImportsModule,
    SharedModule
  ],
  providers: [],
  declarations: [
    MetricsCardComponent,
    MetricsLayoutComponent
  ],
  exports: [
    MetricsCardComponent,
    MetricsLayoutComponent
  ]
})
export class MetricsSharedModule {}
