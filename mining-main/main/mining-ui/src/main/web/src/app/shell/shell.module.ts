import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TranslateModule } from '@ngx-translate/core';
import { RouterModule } from '@angular/router';
import { ProjectShellComponent } from './project-shell/project-shell.component';
import { HeaderComponent } from './header/header.component';
import { FormsModule } from '@angular/forms';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { BaseHeaderShellComponent } from './base-shell/base-header-shell.component';
import { JobProgressMonitorComponent } from './header/job-progress-monitor/job-progress-monitor.component';
import { UniversalSearchComponent } from './header/universal-search/universal-search.component';

@NgModule({
  imports: [
    CommonModule,
    TranslateModule,
    RouterModule,
    FormsModule,
    AntDesignImportsModule
  ],
  declarations: [HeaderComponent, ProjectShellComponent, BaseHeaderShellComponent, JobProgressMonitorComponent, UniversalSearchComponent],
  exports: [HeaderComponent, JobProgressMonitorComponent]
})
export class ShellModule {}
