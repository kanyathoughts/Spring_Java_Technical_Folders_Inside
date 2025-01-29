import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { ProjectDashboardRoutingModule } from './project-dashboard-routing.module';
import { ProjectDashboardComponent } from './project-dashboard.component';

import { FormsModule } from '@angular/forms';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { HotSpotCardComponent } from './hotspot-card/hotspot-card.component';
import { TranslateModule } from '@ngx-translate/core';
import { IMSCardComponent } from './ims-card/ims-card.component';
import { TaxonomyCardComponent } from './taxonomy-card/taxonomy-card.component';
import { BasicFactsComponent } from './basic-facts/basic-facts.component';
import { SavedSearchCardComponent } from './saved-search-card/saved-search-card.component';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ProjectDashboardRoutingModule,
    SharedModule,
    AntDesignImportsModule,
    TranslateModule.forChild()
  ],
  declarations: [ProjectDashboardComponent, HotSpotCardComponent, IMSCardComponent, TaxonomyCardComponent, BasicFactsComponent, SavedSearchCardComponent],
  providers: []
})
export class ProjectDashboardModule {}
