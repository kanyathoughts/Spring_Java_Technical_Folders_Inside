import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { FormsModule } from '@angular/forms';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { ReachabilityUiProductVisionComponent } from './reachability-ui-product-vision.component';
import { ReachabilityUiProductVisionRoutingModule } from './reachability-ui-product-vision-routing.module';
import { LanguageProviderService } from '@app/core/services/monaco-editor/language-provider.service';
import { BlockViewComponent } from './block-view/block-view.component';
import { BlockCardComponent } from './block-view/block-card/block-card.component';
import { BlockDetailsComponent } from './block-details/block-details.component';
import { AnalysisModalComponent } from './analysis-modal/analysis-modal.component';
import { FilterFormComponent } from './analysis-modal/filter-form/filter-form.component';
import { MergeBlockComponent } from './block-view/merge-block/merge-block.component';
import {BlockViewDetailsComponent} from './block-view/view-block-details/block-view-details.component';
import { DependencyGraphModule } from '@app/modules/graph/dependency/dependency-graph.module';
import { ReachabilityBlockGraphComponent } from './reachability-block-graph/reachability-block-graph-component';
import { InitiateReachabilityComponent } from './initiate-reachability/initiate-reachability.component';
import { ReachabilityGraphModule } from '../graph/reachability/reachability-graph.module';
@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReachabilityUiProductVisionRoutingModule,
    SharedModule,
    AntDesignImportsModule,
    DependencyGraphModule,
    ReachabilityGraphModule,
    TranslateModule.forChild()
  ],
  declarations: [ReachabilityUiProductVisionComponent, BlockViewComponent, BlockCardComponent, BlockDetailsComponent, AnalysisModalComponent,
     FilterFormComponent, MergeBlockComponent, BlockViewDetailsComponent, ReachabilityBlockGraphComponent, InitiateReachabilityComponent],
  providers: [LanguageProviderService]
})
export class ReachabilityUiProductVisionModule {}
