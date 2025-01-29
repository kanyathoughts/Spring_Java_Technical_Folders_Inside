import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { GraphOverviewPanelComponent } from './yfiles-graph-overview-panel/graph-overview-panel.component';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { MiningGraphsExportComponent } from './mining-graphs-export/mining-graphs-export.component';


@NgModule({
  declarations: [
    GraphOverviewPanelComponent,
    MiningGraphsExportComponent
  ],
  imports: [
    CommonModule,
    AntDesignImportsModule,
    TranslateModule.forChild()
  ],

  exports: [
    GraphOverviewPanelComponent,
    MiningGraphsExportComponent
  ],
})
export class GraphModule { }
