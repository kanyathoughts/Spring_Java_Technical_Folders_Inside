import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { SharedModule } from '@app/shared/shared.module';
import { TranslateModule } from '@ngx-translate/core';
import { GraphModule } from '../graph.module';
import { ControlFlowGraphComponent } from './control-flow-graph/control-flow-graph.component';
import { ControlFlowRoutingModule } from './control-flow-routing.module';
import { ControlFlowComponent } from './control-flow.component';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { LanguageProviderService } from '../../../core/services/monaco-editor/language-provider.service';

@NgModule({
    declarations: [
      ControlFlowComponent,
      ControlFlowGraphComponent
    ],
    imports: [
      CommonModule,
      FormsModule,
      ReactiveFormsModule,
      SharedModule,
      ControlFlowRoutingModule,
      AntDesignImportsModule,
      GraphModule,
      TranslateModule.forChild()
    ],
    providers: [LanguageProviderService]
  })
  export class ControlFlowModule { }
