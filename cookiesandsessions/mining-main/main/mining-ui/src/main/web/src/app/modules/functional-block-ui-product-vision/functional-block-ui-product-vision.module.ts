import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { FormsModule } from '@angular/forms';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { FunctionalBlockUiProductVisionComponent } from './functional-block-ui-product-vision.component';
import { FunctionalBlockUiProductVisionRoutingModule } from './functional-block-ui-product-vision-routing.module';
import { LanguageProviderService } from '@app/core/services/monaco-editor/language-provider.service';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    FunctionalBlockUiProductVisionRoutingModule,
    SharedModule,
    AntDesignImportsModule,
    TranslateModule.forChild()
  ],
  declarations: [FunctionalBlockUiProductVisionComponent],
  providers: [LanguageProviderService]
})
export class FunctionalBlockUiProductVisionModule {}
