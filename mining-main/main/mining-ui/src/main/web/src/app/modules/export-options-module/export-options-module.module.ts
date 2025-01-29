import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { ExportOptionsModuleRoutingModule } from './export-options-module-routing.module';
import { ExportOptionsModuleComponent } from './export-options-module.component';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { GraphmlExportComponent } from './graphml-export/graphml-export.component';


@NgModule({
  declarations: [ExportOptionsModuleComponent, GraphmlExportComponent],
  imports: [
    CommonModule,
    ExportOptionsModuleRoutingModule,
    SharedModule,
    ReactiveFormsModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
    FormsModule
  ]
})
export class ExportOptionsModuleModule { }
