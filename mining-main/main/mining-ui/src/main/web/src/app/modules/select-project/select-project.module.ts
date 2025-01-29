import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ProjectsOverviewComponent } from './projects-overview/projects-overview.component';
import { SelectProjectRoutingModule } from './select-project-routing.module';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { ProjectCardComponent } from './project-card/project-card.component';
import { AdminClientProjectModule } from '../admin-client-project/admin-client-project.module';
import { TranslateModule } from '@ngx-translate/core';



@NgModule({
  declarations: [ProjectsOverviewComponent, ProjectCardComponent],
  imports: [
    CommonModule,
    AntDesignImportsModule,
    SelectProjectRoutingModule,
    AdminClientProjectModule,
    SharedModule,
    TranslateModule.forChild()
  ]
})
export class SelectProjectModule { }
