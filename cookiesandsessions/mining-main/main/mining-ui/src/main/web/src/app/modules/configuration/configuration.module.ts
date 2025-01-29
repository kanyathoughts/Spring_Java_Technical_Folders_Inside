import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { TranslateModule } from '@ngx-translate/core';
import { ConfigurationComponent } from './configuration.component';
import { ConfigurationRoutingModule } from './configuration-routing.module';
import { TaxonomyConfigurationComponent } from './taxonomy-configuration/taxonomy-configuration.component';
import { TaxonomiesModalComponent } from './taxonomy-configuration/taxonomies-modal/taxonomies-modal.component';
import { CustomPropertiesComponent } from '../../shared/components/custom-properties/custom-properties.component';
import { TaxonomiesErrorWarningModalComponent } from './taxonomy-configuration/taxonomies-warning-error-modal/taxonomies-warning-error-modal.component';
import { ReactiveFormsModule } from '@angular/forms';
import { DbSchemaConfigurationComponent } from './db-schema-configuration/db-schema-configuration.component';
import { AnnotationConfigurationComponent } from './annotation-configuration/annotation-configuration.component';
import { MetadataConfigurationComponent } from './metadata-configuration/metadata-configuration.component';
import { ReachabilityConfigurationComponent } from './reachability-configuration/reachability-configuration.component';
import { SchedulerDataConfigurationComponent } from './scheduler-data-configuration/scheduler-data-configuration.component';
import { ViewConfigurationComponent } from './view-configuration/view-configuration.component';
import { SchedulerDataFormComponent } from './scheduler-data-configuration/scheduler-data-form/scheduler-data-form.component';
import { SchedulerDataModalComponent } from './scheduler-data-configuration/scheduler-data-modal/scheduler-data-modal.component';

@NgModule({
  declarations: [ConfigurationComponent, TaxonomyConfigurationComponent, AnnotationConfigurationComponent, TaxonomiesModalComponent, CustomPropertiesComponent,
    TaxonomiesErrorWarningModalComponent, DbSchemaConfigurationComponent, MetadataConfigurationComponent,
    ReachabilityConfigurationComponent, SchedulerDataConfigurationComponent,
     ViewConfigurationComponent, SchedulerDataFormComponent, SchedulerDataModalComponent],
  imports: [
    CommonModule,
    SharedModule,
    ConfigurationRoutingModule,
    AntDesignImportsModule,
    ReactiveFormsModule,
    TranslateModule.forChild()
  ],
  exports: [
    TaxonomiesModalComponent,
  ]
})

export class ConfigurationModule { }
