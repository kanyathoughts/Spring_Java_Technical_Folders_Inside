import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { FunctionalAnalysisComponent } from './functional-analysis.component';
import { FunctionalAnalysisRoutingModule } from './functional-analysis-routing.module';
import { FunctionalAnalysisTreeComponent } from './functional-analysis-tree/functional-analysis-tree.component';
import { LanguageProviderService } from '@app/core/services/monaco-editor/language-provider.service';
import { CreateFunctionalGroupComponent } from './create-functional-group/create-functional-group.component';
import { FunctionalAnalysisGraphComponent } from './functional-analysis-graph/functional-analysis-graph.component';
import { DependencyGraphModule } from '../graph/dependency/dependency-graph.module';
import { ScrollingModule } from '@angular/cdk/scrolling';
import { InfiniteScrollModule } from 'ngx-infinite-scroll';

@NgModule({
    declarations: [FunctionalAnalysisComponent, FunctionalAnalysisTreeComponent, CreateFunctionalGroupComponent, FunctionalAnalysisGraphComponent],
    imports: [
        CommonModule,
        SharedModule,
        AntDesignImportsModule,
        FunctionalAnalysisRoutingModule,
        DependencyGraphModule,
        TranslateModule.forChild(),
        ScrollingModule,
        InfiniteScrollModule
    ],
    exports: [
        FunctionalAnalysisComponent
    ],
    providers: [LanguageProviderService]
})
export class FunctionalAnalysis { }
