import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MiningModuleRoutingModule } from './module-reporting-routing.module';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { ReactiveFormsModule } from '@angular/forms';
import { TranslateModule } from '@ngx-translate/core';
import { ModuleReportingComponent } from './module-reporting.component';

@NgModule({
    declarations: [
        ModuleReportingComponent
    ],
    imports: [
        CommonModule,
        MiningModuleRoutingModule,
        SharedModule,
        AntDesignImportsModule,
        ReactiveFormsModule,
        TranslateModule.forChild()
    ]
})
export class BrowseMiningModulesModule { }
