import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { DnaAnalysisComponent } from './dna-analysis.component';
import { DnaAnalysisRoutingModule } from './dna-analysis-routing.module';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { DnaCardComponent } from './dna-card/dna-card.component';

@NgModule({
    declarations: [
        DnaAnalysisComponent,
        DnaCardComponent
    ],
    imports: [
        CommonModule,
        DnaAnalysisRoutingModule,
        SharedModule,
        AntDesignImportsModule,
        TranslateModule.forChild()
    ],
    exports: [
        DnaAnalysisComponent
    ]
})
export class DnaAnalysisModule { }
