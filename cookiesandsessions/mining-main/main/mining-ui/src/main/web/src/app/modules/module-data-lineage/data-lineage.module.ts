import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SharedModule } from '@app/shared/shared.module';
import { AdminClientProjectModule } from '../admin-client-project/admin-client-project.module';
import { TranslateModule } from '@ngx-translate/core';
import { DataLineageReportingModule } from './data-lineage-routing.module';
import { DataLineageComponent } from './data-lineage.component';
import { DataFlowGraphComponent } from './data-flow-graph/data-flow-graph.component';
import { GraphModule } from '../graph/graph.module';
import { LanguageProviderService } from '../../core/services/monaco-editor/language-provider.service';



@NgModule({
  declarations: [DataLineageComponent, DataFlowGraphComponent],
  imports: [
    CommonModule,
    AntDesignImportsModule,
    DataLineageReportingModule,
    SharedModule,
    AdminClientProjectModule,
    GraphModule,
    TranslateModule.forChild()
  ],
  providers: [LanguageProviderService]
})
export class DataLineageModule { }
