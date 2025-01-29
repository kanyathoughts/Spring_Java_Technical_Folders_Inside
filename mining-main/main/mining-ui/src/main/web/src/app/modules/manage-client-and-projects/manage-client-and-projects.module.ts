import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SharedModule } from '@app/shared/shared.module';
import { AdminClientProjectModule } from '../admin-client-project/admin-client-project.module';
import { ManageClientAndProjectsComponent } from './manage-client-and-projects.component';
import { ManageClientAndProjectsRoutingModule } from './manage-client-and-projects-routing.module';
import { TranslateModule } from '@ngx-translate/core';



@NgModule({
  declarations: [ManageClientAndProjectsComponent],
  imports: [
    CommonModule,
    AntDesignImportsModule,
    ManageClientAndProjectsRoutingModule,
    SharedModule,
    AdminClientProjectModule,
    TranslateModule.forChild()
  ]
})
export class ManageClientAndProjectsModule { }
