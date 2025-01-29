import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FunctionalBlocksRoutingModule } from './functional-blocks-routing.module';
import { FunctionalBlocksLayoutComponent } from './functional-blocks-layout/functional-blocks-layout.component';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { TranslateModule } from '@ngx-translate/core';
import { FunctionalBlockCardComponent } from './functional-block-card/functional-block-card.component';



@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    SharedModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
    FunctionalBlocksRoutingModule
  ],
  declarations: [
    FunctionalBlocksLayoutComponent,
    FunctionalBlockCardComponent
  ]
})
export class FunctionalBlocksModule { }
