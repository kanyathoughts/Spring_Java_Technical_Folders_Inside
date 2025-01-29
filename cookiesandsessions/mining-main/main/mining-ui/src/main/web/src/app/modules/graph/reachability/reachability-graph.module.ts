import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { ReachabilityExplorerComponent } from './reachability-explorer/reachability-explorer.component';
import { ReachabilityGraphComponent } from './reachability-graph/reachability-graph.component';
import { AntDesignImportsModule } from '../../../shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { SharedModule } from '@app/shared';
import { GraphModule } from '../graph.module';
import { DependencyGraphModule } from '../dependency/dependency-graph.module';
import { ReachabilityEdgeMetaDataComponent } from './reachability-graph/reachability-edge-meta-data/reachability-edge-meta-data.component';

@NgModule({
    declarations: [
        ReachabilityExplorerComponent,
        ReachabilityGraphComponent,
        ReachabilityEdgeMetaDataComponent
    ],
  imports: [
    CommonModule,
    SharedModule,
    GraphModule,
    DependencyGraphModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
  ],
  exports: [
    ReachabilityExplorerComponent
  ]
})
export class ReachabilityGraphModule { }
