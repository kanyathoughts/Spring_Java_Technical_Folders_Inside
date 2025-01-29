import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { SemanticSearchRoutingModule } from './semantic-search-routing.module';
import { SemanticSearchComponent } from './semantic-search.component';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '@app/shared';
import { TranslateModule } from '@ngx-translate/core';


@NgModule({
  declarations: [
    SemanticSearchComponent
  ],
  imports: [
    CommonModule,
    SemanticSearchRoutingModule,
    AntDesignImportsModule,
    SharedModule,
    FormsModule,
    TranslateModule.forChild()
  ]
})
export class SemanticSearchModule { }
