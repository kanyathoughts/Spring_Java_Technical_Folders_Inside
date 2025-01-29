import { NgModule } from '@angular/core';
import { CallChainExportComponent } from '@app/modules/module-details/call-chain-export/call-chain-export.component';
import { TranslateModule } from '@ngx-translate/core';
import { AntDesignImportsModule } from '../../../shared/ant-design-imports.module';
import { CommonModule } from '@angular/common';
import { DependencyGraphModule } from '../../graph/dependency/dependency-graph.module';
import { SharedModule } from '@app/shared';
import { CallChainExportRoutingModule } from './call-chain-export-routing.module';
import { ModuleSelectionComponent } from './module-selection/module-selection.component';

@NgModule({
    declarations: [
        CallChainExportComponent,
        ModuleSelectionComponent
    ],
    imports: [
        CommonModule,
        AntDesignImportsModule,
        DependencyGraphModule,
        CallChainExportRoutingModule,
        SharedModule,
        TranslateModule.forChild(),
    ],
    exports: [
        CallChainExportComponent,
    ]
})

export class CallChainExportModule {}
