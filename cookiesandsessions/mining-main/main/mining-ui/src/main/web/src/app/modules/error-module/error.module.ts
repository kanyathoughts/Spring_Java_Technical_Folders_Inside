import { NgModule } from '@angular/core';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SharedModule } from '@app/shared/shared.module';
import { TranslateModule } from '@ngx-translate/core';
import { ErrorComponent } from './error.component';
import { ShellModule } from '../../shell/shell.module';
import { ErrorRoutingModule } from './error-routing.module';
@NgModule({
  declarations: [ErrorComponent],
  imports: [
    SharedModule,
    ShellModule,
    ErrorRoutingModule,
    AntDesignImportsModule,
    TranslateModule.forChild()
  ],
})
export class ErrorModule {
}
