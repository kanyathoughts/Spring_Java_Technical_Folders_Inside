import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { ManageTokensRoutingModule } from './manage-tokens-routing.module';
import { TokensOverviewComponent } from './tokens-overview/tokens-overview.component';
import { TranslateModule } from '@ngx-translate/core';
import { SharedModule } from '@app/shared/shared.module';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { FormsModule } from '@angular/forms';
import { AuthControllerService } from '@app/core/services/auth-controller/auth-controller.service';


@NgModule({
  declarations: [
    TokensOverviewComponent
  ],
  imports: [
    CommonModule,
    SharedModule,
    FormsModule,
    AntDesignImportsModule,
    ManageTokensRoutingModule,
    TranslateModule.forChild()
  ],
  providers: [
    AuthControllerService
  ]
})
export class ManageTokensModule { }
