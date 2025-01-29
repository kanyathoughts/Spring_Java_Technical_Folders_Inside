import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ImsComponent } from './ims.component';
import { ImsRoutingModule } from './ims-routing.module';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SharedModule } from '@app/shared';
import { TranslateModule } from '@ngx-translate/core';
import { MetricsSharedModule } from '../shared/metrics-shared.module';

@NgModule({
  declarations: [
    ImsComponent
  ],
  imports: [
    CommonModule,
    ImsRoutingModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
    MetricsSharedModule,
    SharedModule
  ]
})
export class ImsModule { }
