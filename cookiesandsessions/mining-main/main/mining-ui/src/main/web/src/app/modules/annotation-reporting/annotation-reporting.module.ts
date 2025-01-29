import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AnnotationReportingRoutingModule } from './annotation-reporting-routing.module';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { SharedModule } from '@app/shared/shared.module';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { AnnotationReportingComponent } from
  './annotation-reporting.component';
import { AnnotationsImportModalComponent } from './annotations-import-modal/annotations-import-modal.component';

@NgModule({
  declarations: [
    AnnotationReportingComponent,
    AnnotationsImportModalComponent
  ],
  imports: [
    CommonModule,
    FormsModule,
    AnnotationReportingRoutingModule,
    SharedModule,
    ReactiveFormsModule,
    AntDesignImportsModule,
    TranslateModule.forChild()
  ],
})
export class AnnotationReportingModule {
}
