import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ModuleDetailsComponent } from './module-details.component';
import { ModuleDetailsRoutingModule } from './module-details-routing.module';
import { SharedModule } from '@app/shared';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { ModuleAnnotationsComponent } from './module-annotations/module-annotations.component';
import { ModuleDataDictionaryComponent } from './module-data-dictionary/module-data-dictionary.component';
import { ModuleDependenciesComponent } from './module-dependencies/module-dependencies.component';
import { ModuleCharacteristicsComponent } from './module-characteristics/module-characteristics.component';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { TranslateModule } from '@ngx-translate/core';
import { ModalWindowComponent } from '@app/shared/components/modal-window/modal-window.component';
import { CustomPropertiesCardComponent } from './custom-properties-card/custom-properties-card.component';
import { ModuleDNAComponent } from './module-dna/module-dna.component';
import { ModuleErrorsComponent } from './module-errors/module-errors.component';
import { CallChainExportModule } from './call-chain-export/call-chain-export.module';
import { DependencyGraphModule } from '../graph/dependency/dependency-graph.module';

@NgModule({
    declarations: [
        ModuleDetailsComponent,
        ModuleAnnotationsComponent,
        ModuleCharacteristicsComponent,
        ModuleDataDictionaryComponent,
        ModuleDependenciesComponent,
        ModuleErrorsComponent,
        CustomPropertiesCardComponent,
        ModuleDNAComponent
    ],
    providers: [
        ModalWindowComponent
    ],
    imports: [
        CommonModule,
        ModuleDetailsRoutingModule,
        SharedModule,
        CallChainExportModule,
        DependencyGraphModule,
        AntDesignImportsModule,
        ReactiveFormsModule,
        FormsModule,
        TranslateModule.forChild()
    ]
})
export class ModuleDetailsModule { }
