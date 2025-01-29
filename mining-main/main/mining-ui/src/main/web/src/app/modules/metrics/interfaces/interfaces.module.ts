import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { InterfacesRoutingModule } from './interfaces-routing.module';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { MetricsSharedModule } from '../shared/metrics-shared.module';
import { InterfacesComponent } from './interfaces.component';

@NgModule({
  declarations: [InterfacesComponent],
  imports: [
    CommonModule,
    InterfacesRoutingModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
    MetricsSharedModule,
  ],
})
export class InterfacesModule {}
