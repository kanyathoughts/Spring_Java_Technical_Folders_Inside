import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { DataDictionaryReportingComponent } from './data-dictionary-reporting.component';
import { DataDictionaryReportingRoutingModule } from './data-dictionary-reporting-routing.module';


@NgModule({
  declarations: [DataDictionaryReportingComponent],
  imports: [
    CommonModule,
    SharedModule,
    DataDictionaryReportingRoutingModule,
    AntDesignImportsModule,
    TranslateModule.forChild()
  ]
})

export class DataDictionaryReportingModule { }
