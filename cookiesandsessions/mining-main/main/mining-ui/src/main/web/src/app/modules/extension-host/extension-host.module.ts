import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { ReactiveFormsModule } from '@angular/forms';
import { TranslateModule } from '@ngx-translate/core';
import { ExtensionHostComponent } from './extension-host.component';
import { ExtensionHostRoutingModule } from './extension-host-routing.module';

@NgModule({
    declarations: [
        ExtensionHostComponent
    ],
    imports: [
        CommonModule,
        ExtensionHostRoutingModule,
        SharedModule,
        AntDesignImportsModule,
        ReactiveFormsModule,
        TranslateModule.forChild()
    ]
})
export class ExtensionHostModule { }
