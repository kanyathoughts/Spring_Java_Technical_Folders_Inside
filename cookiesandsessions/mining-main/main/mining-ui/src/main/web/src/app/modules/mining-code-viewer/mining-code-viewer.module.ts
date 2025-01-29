import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedModule } from '@app/shared';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MiningCodeViewerComponent } from './mining-code-viewer.component';
import { MiningCodeViewerRoutingModule } from './mining-code-viewer-routing.module';
import { TranslateModule } from '@ngx-translate/core';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { LanguageProviderService } from '../../core/services/monaco-editor/language-provider.service';

@NgModule({
    declarations: [
        MiningCodeViewerComponent,
    ],
    imports: [
        CommonModule,
        FormsModule,
        SharedModule,
        ReactiveFormsModule,
        MiningCodeViewerRoutingModule,
        TranslateModule.forChild(),
        AntDesignImportsModule
    ],
    providers: [LanguageProviderService]
})

/**
 * Adding definition of the custom element created for Code annotation to the module.
 */
export class MiningCodeViewerModule { }
