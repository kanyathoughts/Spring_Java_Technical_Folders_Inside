import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { TranslateModule } from '@ngx-translate/core';
import { LoginRoutingModule } from './login-routing.module';
import { LoginComponent } from './login.component';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';


@NgModule({
  imports: [CommonModule, ReactiveFormsModule, TranslateModule.forChild(), LoginRoutingModule, AntDesignImportsModule],
  declarations: [LoginComponent]
})
export class LoginModule {}
