import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { ReactiveFormsModule } from '@angular/forms';
import { TranslateModule } from '@ngx-translate/core';
import { TableExtensionHostComponent } from './table-extension-host.component';
import { TableExtensionHostRoutingModule } from './table-extension-host-routing.module';

@NgModule({
    declarations: [
        TableExtensionHostComponent
    ],
    imports: [
        CommonModule,
        TableExtensionHostRoutingModule,
        SharedModule,
        AntDesignImportsModule,
        ReactiveFormsModule,
        TranslateModule.forChild()
    ]
})
export class TableExtensionHostModule { }
