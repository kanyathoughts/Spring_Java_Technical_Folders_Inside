import { NgModule } from '@angular/core';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SharedModule } from '@app/shared/shared.module';
import { TranslateModule } from '@ngx-translate/core';
import { LoginErrorComponent } from './login-error.component';
import { ShellModule } from '../../shell/shell.module';
import { LoginErrorRoutingModule } from './login-error-routing.module';

@NgModule({
  declarations: [LoginErrorComponent],
  imports: [
    SharedModule,
    ShellModule,
    LoginErrorRoutingModule,
    AntDesignImportsModule,
    TranslateModule.forChild()
  ],
})
export class LoginErrorModule {
}
