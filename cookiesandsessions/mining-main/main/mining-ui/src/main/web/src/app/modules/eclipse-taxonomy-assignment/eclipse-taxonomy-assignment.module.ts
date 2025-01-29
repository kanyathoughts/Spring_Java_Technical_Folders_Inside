import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SharedModule } from '@app/shared/shared.module';
import { TranslateModule } from '@ngx-translate/core';
import { EclipseTaxonomyAssignmentRoutingModule } from './eclipse-taxonomy-assignment-routing.module';
import { EclipseTaxonomyAssignmentComponent } from './eclipse-taxonomy-assignment.component';

@NgModule({
  declarations: [EclipseTaxonomyAssignmentComponent],
  imports: [
    CommonModule,
    SharedModule,
    AntDesignImportsModule,
    EclipseTaxonomyAssignmentRoutingModule,
    TranslateModule.forChild()
    ],
})
export class EclipseTaxonomyAssignmentModule {
}
