import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { DependencyExplorerComponent } from '@app/modules/graph/dependency/dependency-explorer/dependency-explorer.component';
import { ContextualToolbarComponent } from '@app/modules/graph/dependency/dependency-graph/context-menu-toolbar/context-menu-toolbar.component';
import { DependencyGraphComponent } from '@app/modules/graph/dependency/dependency-graph/dependency-graph.component';
import { AntDesignImportsModule } from '../../../shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { SharedModule } from '@app/shared';
import { GraphModule } from '../graph.module';
import { DependencyGraphRoutingModule } from './dependency-graph-routing.module';
import { GraphEdgeMetaDataComponent } from './dependency-graph/graph-edge-meta-data/graph-edge-meta-data.component';

@NgModule({
  declarations: [
    DependencyGraphComponent,
    DependencyExplorerComponent,
    ContextualToolbarComponent,
    GraphEdgeMetaDataComponent
  ],
  imports: [
    CommonModule,
    SharedModule,
    GraphModule,
    DependencyGraphRoutingModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
  ],
  exports: [
    DependencyExplorerComponent,
    ContextualToolbarComponent
  ]
})
export class DependencyGraphModule { }
