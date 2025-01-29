import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ClientFormModalComponent } from './client-form-modal/client-form-modal.component';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { TranslateModule } from '@ngx-translate/core';
import { ProjectFormModalComponent } from './project-form-modal/project-form-modal.component';
import { SharedModule } from '@app/shared';
import { MemberFormModalComponent } from './member-form-modal/member-form-modal.component';



@NgModule({
  declarations: [ClientFormModalComponent, ProjectFormModalComponent, MemberFormModalComponent],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    FormsModule,
    AntDesignImportsModule,
    TranslateModule.forChild(),
    SharedModule
  ],
  exports: [
    ClientFormModalComponent,
    ProjectFormModalComponent,
    MemberFormModalComponent
  ]
})
export class AdminClientProjectModule { }
