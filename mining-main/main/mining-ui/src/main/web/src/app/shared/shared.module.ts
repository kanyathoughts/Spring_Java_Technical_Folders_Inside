import { NgModule } from '@angular/core';
import { CommonModule, DatePipe } from '@angular/common';
import { MiningTableComponent } from './components/mining-table/mining-table.component';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { LoaderComponent } from './loader/loader.component';
import { LoaderDirective } from './loader/loader.directive';
import { MessageComponent } from './components/message/message.component';
import { ModalWindowComponent } from '@app/shared/components/modal-window/modal-window.component';
import { TranslateModule } from '@ngx-translate/core';
import { MiningToolbarComponent } from './mining-toolbar/mining-toolbar.component';
import { MiningToolbarGroupDirective } from './mining-toolbar/mining-toolbar-group/mining-toolbar-group.directive';
import { MiningToolbarButtonDirective } from './mining-toolbar/mining-toolbar-button/mining-toolbar-button.directive';
import { MiningToolbarSpacerDirective } from './mining-toolbar/mining-toolbar-spacer/mining-toolbar-spacer.directive';
import { SettingsComponent } from './components/settings/settings.component';
import { EclipseButtonComponent } from './components/eclipse-button/eclipse-button.component';
import { DefaultCardCoverComponent } from './components/default-card-cover/default-card-cover.component';
import { BaseFormModalComponent } from './components/base-form-modal/base-form-modal.component';
import { ConfirmDeleteModalComponent } from './components/confirm-delete-modal/confirm-delete-modal.component';
import { AntDesignImportsModule } from './ant-design-imports.module';
import { CustomPropertyFieldComponent } from './components/custom-property-editor/custom-property-field/custom-property-field.component';
import { SharedAnnotationEditorComponent } from './components/shared-annotation-editor/shared-annotation-editor.component';
import { RouterModule } from '@angular/router';
import { WebAnnotationEditorComponent } from './components/web-annotation-editor/web-annotation-editor.component';
import { TaxonomyFilterComponent } from './components/taxonomy-filter/taxonomy-filter.component';
import { ModuleOverViewTaxonomyComponent } from './components/module-overview-taxonomy/module-overview-taxonomy.component';
import { SharedEditorHeaderComponent } from './components/shared-editor-header/shared-editor-header.component';
import { VirtualModuleDetailsComponent } from './components/virtual-module-details/virtual-module-details.component';
import { MiningSettingPopOverComponent } from './components/mining-table/mining-setting-popover/mining-setting-popover.component';
import { AssignTaxonomiesComponent } from './components/assign-taxonomies/assign-taxonomies.component';
import { NzHighlightModule } from 'ng-zorro-antd/core/highlight';
import { ModuleBreadcrumbComponent } from './module-breadcrumb/module-breadcrumb.component';
import { DisplayValueAsPerTypeComponent } from './components/mining-table/display-value-as-per-type/display-value-as-per-type.component';
import { TypeBasedFilterComponent } from './components/type-based-filter/type-based-filter.component';
import { BaseReportingPageComponent } from './components/base-customizable-table/base-reporting-page.component';
import { CustomPropertyEditorComponent } from './components/custom-property-editor/custom-property-editor.component';
import { CustomPropertyFieldListComponent } from './components/custom-property-editor/custom-property-field-list/custom-property-field-list.component';
import { SharedDataDictionaryEditorComponent } from './components/shared-data-dictionary-editor/shared-data-dictionary-editor.component';
import { SharedModuleDetailsComponent } from './components/shared-module-details/shared-module-details.component';
import { MiningTableTitleComponent } from './components/mining-table/mining-table-title/mining-table-title.component';
import { MiningMonacoEditorComponent } from './components/mining-monaco-editor/mining-monaco-editor.component';
import { ModuleListingComponent } from './components/module-listing/module-listing.component';
import { CodeViewerButtonComponent } from './components/code-viewer-button/code-viewer-button.component';
import { DnaChartDetailsComponent } from './components/dna-chart-details/dna-chart-details.component';
import { CustomPropertyFormComponent } from './components/custom-property-form/custom-property-form.component';
import { DataPointEditorComponent } from './components/datapoint-editor/datapoint-editor.component';
import { CustomizableTableCoreComponent } from './components/customizable-table-core/customizable-table-core.component';
import { MiningToolbarButtonComponent } from './mining-toolbar/mining-toolbar-button/mining-toolbar-button.component';
import { SavedSearchComponent } from './components/base-customizable-table/saved-search/saved-search.component';
import { DataLineageExportComponent } from './components/data-lineage-export/data-lineage-export.component';
import { TaxonomyPropagationComponent } from './taxonomy-propagation/taxonomy-propagation/taxonomy-propagation.component';
import { IdentifyAffectedModulesComponent } from './taxonomy-propagation/identify-modules/identify-affected-modules-by-propagation.component';
import { NumberFormatter } from './pipes/number-formatter';
import { GraphModule } from '@app/modules/graph/graph.module';
import { CodeAnnotationEditorComponent } from '@app/modules/mining-code-viewer/code-annotation/code-annotation-editor.component';
import { ControlFlowMetaDataComponent } from './components/control-flow-meta-data/control-flow-meta-data.component';
import { TableRowEllipsisDirective } from './components/mining-table/mining-table-row-ellipsis.directive';
import { Injector } from '@angular/core';
import { createCustomElement } from '@angular/elements';
import { AssignFunctionalGroupComponent } from './components/assign-functional-groups/assign-functional-groups.component';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { ListFunctionalGroupComponent } from './components/list-funtional-group/list-functional-group.component';
import { CreateEditFunctionalGroupComponent } from './components/create-edit-functional-group/create-edit-functional-group.component';
import { NavigationGuard } from '@app/core/pendingChanges/navigationGuard';
import { IdentifyBusinessRelatedModalComponent } from './components/identify-business-related-modal/identify-business-related-modal.component';
import { BaseEditFunctionalBlockComponent } from './components/base-edit-functional-block/base-edit-functional-block.component';
import { EditReachabilityBlockComponent } from './components/edit-reachability-block/edit-reachability-block.component';
import { AutoDetectReachabilityComponent } from './components/auto-detect-reachability/auto-detect-reachability-component';
import { WaitModalComponent } from './components/wait-modal/wait-modal.component';
import { ModuleSchemaInfoComponent } from '@app/modules/module-details/module-schema-info/module-schema-info.component';
@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    TranslateModule,
    AntDesignImportsModule,
    RouterModule,
    NzHighlightModule,
    GraphModule,
    DragDropModule
  ],
  providers: [DatePipe , NumberFormatter , NavigationGuard],
  declarations: [
    MiningTableComponent,
    LoaderComponent,
    LoaderDirective,
    MessageComponent,
    ModalWindowComponent,
    MiningToolbarComponent,
    MiningToolbarGroupDirective,
    MiningToolbarButtonDirective,
    MiningToolbarSpacerDirective,
    TableRowEllipsisDirective,
    SettingsComponent,
    EclipseButtonComponent,
    DefaultCardCoverComponent,
    BaseFormModalComponent,
    DnaChartDetailsComponent,
    ConfirmDeleteModalComponent,
    CustomPropertyFieldComponent,
    SharedAnnotationEditorComponent,
    SharedDataDictionaryEditorComponent,
    WebAnnotationEditorComponent,
    TaxonomyFilterComponent,
    ModuleOverViewTaxonomyComponent,
    SharedEditorHeaderComponent,
    ModuleBreadcrumbComponent,
    MiningSettingPopOverComponent,
    VirtualModuleDetailsComponent,
    SharedEditorHeaderComponent,
    VirtualModuleDetailsComponent,
    AssignTaxonomiesComponent,
    AssignFunctionalGroupComponent,
    DisplayValueAsPerTypeComponent,
    TypeBasedFilterComponent,
    BaseReportingPageComponent,
    CustomPropertyEditorComponent,
    SharedModuleDetailsComponent,
    CustomPropertyFieldListComponent,
    MiningTableTitleComponent,
    MiningMonacoEditorComponent,
    ModuleListingComponent,
    CodeViewerButtonComponent,
    CustomPropertyFormComponent,
    CustomizableTableCoreComponent,
    MiningToolbarButtonComponent,
    DataPointEditorComponent,
    SavedSearchComponent,
    DataLineageExportComponent,
    ModuleSchemaInfoComponent,
    IdentifyAffectedModulesComponent,
    TaxonomyPropagationComponent,
    CustomizableTableCoreComponent,
    CodeAnnotationEditorComponent,
    NumberFormatter,
    ControlFlowMetaDataComponent,
    ListFunctionalGroupComponent,
    CreateEditFunctionalGroupComponent,
    BaseEditFunctionalBlockComponent,
    EditReachabilityBlockComponent,
    IdentifyBusinessRelatedModalComponent,
    AutoDetectReachabilityComponent,
    WaitModalComponent,
 ],
  exports: [
    MiningTableComponent,
    MiningSettingPopOverComponent,
    LoaderComponent,
    LoaderDirective,
    MessageComponent,
    MiningToolbarComponent,
    MiningToolbarGroupDirective,
    MiningToolbarButtonDirective,
    MiningToolbarSpacerDirective,
    TableRowEllipsisDirective,
    ModuleSchemaInfoComponent,
    SettingsComponent,
    EclipseButtonComponent,
    DefaultCardCoverComponent,
    BaseFormModalComponent,
    DnaChartDetailsComponent,
    CustomPropertyFieldComponent,
    SharedAnnotationEditorComponent,
    SharedDataDictionaryEditorComponent,
    TaxonomyFilterComponent,
    ModuleOverViewTaxonomyComponent,
    SharedEditorHeaderComponent,
    ModuleBreadcrumbComponent,
    VirtualModuleDetailsComponent,
    AssignTaxonomiesComponent,
    AssignFunctionalGroupComponent,
    DisplayValueAsPerTypeComponent,
    CustomPropertyEditorComponent,
    CustomPropertyFieldListComponent,
    FormsModule,
    ReactiveFormsModule,
    SharedModuleDetailsComponent,
    MiningMonacoEditorComponent,
    ModuleListingComponent,
    CodeViewerButtonComponent,
    CustomPropertyFormComponent,
    CustomizableTableCoreComponent,
    MiningToolbarButtonComponent,
    CustomPropertyFormComponent,
    SavedSearchComponent,
    DataLineageExportComponent,
    IdentifyAffectedModulesComponent,
    TaxonomyPropagationComponent,
    CustomizableTableCoreComponent,
    CodeAnnotationEditorComponent,
    NumberFormatter,
    ControlFlowMetaDataComponent,
    ListFunctionalGroupComponent,
    EditReachabilityBlockComponent,
    IdentifyBusinessRelatedModalComponent,
    AutoDetectReachabilityComponent
  ]
})
export class SharedModule {
  constructor(injector: Injector) {
    /**
     * Below Code is to use the CodeAnnotationEditorComponent both as custom element and as angular component.
     * 'code-annotation-editor-element' is created for custom elements.
     */
    const codeEditorComponent = createCustomElement(CodeAnnotationEditorComponent, { injector });
    if ( ! customElements.get('code-annotation-editor-element')) {
      customElements.define('code-annotation-editor-element', codeEditorComponent);
    }
  }
}
