import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SharedModule } from '@app/shared/shared.module';
import { TranslateModule } from '@ngx-translate/core';
import { EclipseAnnotationEditorRoutingModule } from './eclipse-annotation-editor-routing.module';
import { EclipseAnnotationEditorComponent } from './eclipse-annotation-editor.component';

@NgModule({
  declarations: [EclipseAnnotationEditorComponent],
  imports: [
    CommonModule,
    SharedModule,
    AntDesignImportsModule,
    EclipseAnnotationEditorRoutingModule,
    TranslateModule.forChild()
  ]
})
export class EclipseAnnotationEditorModule {
}
