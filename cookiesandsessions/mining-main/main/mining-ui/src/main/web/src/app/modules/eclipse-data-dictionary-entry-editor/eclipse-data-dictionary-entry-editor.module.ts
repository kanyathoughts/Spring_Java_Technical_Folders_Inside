import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SharedModule } from '@app/shared/shared.module';
import { TranslateModule } from '@ngx-translate/core';
import { EclipseDataDictionaryEntryEditorComponent } from './eclipse-data-dictionary-entry-editor.component';
import { EclipseDataDictionaryEntryEditorRoutingModule } from './eclipse-data-dictionary-entry-editor-routing.module';



@NgModule({
  declarations: [
    EclipseDataDictionaryEntryEditorComponent
  ],
  imports: [
    CommonModule,
    SharedModule,
    AntDesignImportsModule,
    EclipseDataDictionaryEntryEditorRoutingModule,
    TranslateModule.forChild()
  ]
})
export class EclipseDataDictionaryEntryEditorModule { }
