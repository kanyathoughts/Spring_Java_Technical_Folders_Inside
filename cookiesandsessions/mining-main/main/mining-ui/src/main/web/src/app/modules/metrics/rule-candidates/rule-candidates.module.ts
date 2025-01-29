import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { RuleCandidatesRoutingModule } from './rule-candidates-routing.module';
import { RuleCandidatesComponent } from './rule-candidates.component';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { MetricsSharedModule } from '../shared/metrics-shared.module';
import { TranslateModule } from '@ngx-translate/core';


@NgModule({
  declarations: [RuleCandidatesComponent],
  imports: [
    CommonModule,
    RuleCandidatesRoutingModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
    MetricsSharedModule
  ]
})
export class RuleCandidatesModule { }
