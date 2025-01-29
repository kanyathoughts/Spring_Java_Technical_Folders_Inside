import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { UtilitiesRoutingModule } from './utilities-routing.module';
import { UtilitiesComponent } from './utilities.component';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { MetricsSharedModule } from '../shared/metrics-shared.module';
import { TranslateModule } from '@ngx-translate/core';


@NgModule({
  declarations: [UtilitiesComponent],
  imports: [
    CommonModule,
    UtilitiesRoutingModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
    MetricsSharedModule
  ]
})
export class UtilitiesModule { }
