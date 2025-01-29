import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SharedModule } from '@app/shared/shared.module';
import { TranslateModule } from '@ngx-translate/core';
import { GlobalCallChainRoutingModule } from './global-call-chain-routing.module';
import { CallChainExportModule } from '@app/modules/module-details/call-chain-export/call-chain-export.module';

@NgModule({
  imports: [
    CommonModule,
    GlobalCallChainRoutingModule,
    AntDesignImportsModule,
    CallChainExportModule,
    TranslateModule.forChild(),
    SharedModule
  ]
})
export class GlobalCallChainModule { }
