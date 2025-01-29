import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ClientsOverviewComponent } from './clients-overview/clients-overview.component';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SelectClientRoutingModule } from './select-client-routing.module';
import { ClientCardComponent } from './client-card/client-card.component';
import { SharedModule } from '@app/shared/shared.module';
import { AdminClientProjectModule } from '../admin-client-project/admin-client-project.module';
import { TranslateModule } from '@ngx-translate/core';



@NgModule({
  declarations: [ClientsOverviewComponent, ClientCardComponent],
  imports: [
    CommonModule,
    AntDesignImportsModule,
    SelectClientRoutingModule,
    SharedModule,
    AdminClientProjectModule,
    TranslateModule.forChild()
  ]
})
export class SelectClientModule { }
