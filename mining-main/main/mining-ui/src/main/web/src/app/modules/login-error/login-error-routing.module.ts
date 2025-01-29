import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { LoginErrorModule } from './login-error.module';

const routes: Routes = [{ path: '', component: LoginErrorModule, data: { title: 'Login Error' } }];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
  providers: []
})
export class LoginErrorRoutingModule {}
