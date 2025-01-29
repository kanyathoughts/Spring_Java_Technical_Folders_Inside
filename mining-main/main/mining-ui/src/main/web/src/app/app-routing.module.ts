import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ModuleResolverService } from './core/services/route-data-resolver/module-resolver.service';
import { ErrorComponent } from './modules/error-module/error.component';
import { Shell } from './shell/shell.utils';
import { FeaturesGuard } from './core/features/features.guard';
import { ProjectRole } from '@innowake/mining-api-angular-client';
import { NavigationGuard } from './core/pendingChanges/navigationGuard';

const routes: Routes = [
  { path: '', redirectTo: 'clients', pathMatch: 'full' },
  {
    path: 'login',
    loadChildren: () => import('./modules/login/login.module').then(m => m.LoginModule)
  },
  Shell.headerShellChildRoutes([
    {
      path: 'clients',
      canDeactivate: [NavigationGuard],
      loadChildren: () => import('./modules/select-client/select-client.module').then(m => m.SelectClientModule)
    },
    {
      path: 'tokens',
      canDeactivate: [NavigationGuard],
      loadChildren: () => import('./modules/manage-tokens/manage-tokens.module').then(m => m.ManageTokensModule)
    },
    {
      path: ':clientId/projects',
      canDeactivate: [NavigationGuard],
      loadChildren: () => import('./modules/select-project/select-project.module').then(m => m.SelectProjectModule),
    },
    Shell.projectShellChildRoutes([
      {
        path: ':projectId/dashboard',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/project-dashboard/project-dashboard.module').then(m => m.ProjectDashboardModule),
      },
      {
        path: ':projectId/modules',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/module-reporting/module-reporting.module').then(m => m.BrowseMiningModulesModule),
      },
      {
        path: ':projectId/functional-blocks',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/functional-blocks/functional-blocks.module').then(m => m.FunctionalBlocksModule),
      },
      {
        path: ':projectId/annotations',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/annotation-reporting/annotation-reporting.module').then(m => m.AnnotationReportingModule),
      },
      {
        path: ':projectId/export',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/export-options-module/export-options-module.module').then(m => m.ExportOptionsModuleModule),
      },
      {
        path: ':projectId/dna',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/dna-analysis/dna-analysis.module').then(m => m.DnaAnalysisModule),
      },
      {
        path: ':projectId/functional-analysis',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/functional-analysis/functional-analysis.module').then(m => m.FunctionalAnalysis),
      },
      {
        path: ':projectId/configuration',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/configuration/configuration.module').then(m => m.ConfigurationModule),
      },
      {
        path: ':projectId/data-dictionary',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/data-dictionary-reporting/data-dictionary-reporting.module').then(m => m.DataDictionaryReportingModule),
      },
      {
        path: ':projectId/:moduleHash/data-lineage',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/module-data-lineage/data-lineage.module').then(m => m.DataLineageModule),
        resolve: {
          module: ModuleResolverService
        }
      },
      {
        path: ':projectId/:moduleHash/details',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/module-details/module-details.module').then(m => m.ModuleDetailsModule),
      },
      {
        path: ':projectId/:moduleHash/code-viewer',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/mining-code-viewer/mining-code-viewer.module').then(m => m.MiningCodeViewerModule),
        resolve: {
          module: ModuleResolverService
        }
      },
      {
        path: ':projectId/:moduleHash/control-flow',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/graph/module-control-flow/control-flow.module').then(m => m.ControlFlowModule),
        resolve: {
          module: ModuleResolverService
        },
        data: { includeSource: true },
      },
      {
        path: ':projectId/:moduleHash/dependencies',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/graph/dependency/dependency-graph.module').then(m => m.DependencyGraphModule),
        resolve: {
          module: ModuleResolverService
        }
      },
      {
        path: ':projectId/metrics',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/metrics/metrics.module').then(m => m.MetricsModule),
        data: {
          subMenu: 'metricsSubMenu'
        }
      },
      {
        path: ':projectId/composition',
        loadChildren: () => import('./modules/functional-block-ui-product-vision/functional-block-ui-product-vision.module').
        then(m => m.FunctionalBlockUiProductVisionModule),
        canActivate: [FeaturesGuard],
        data: {
          requiredFeature: 'functionalBlockUiProductVision'
        }
      },
      {
        path: ':projectId/reachability',
        loadChildren: () => import('./modules/reachability-ui-product-vision/reachability-ui-product-vision.module').
        then(m => m.ReachabilityUiProductVisionModule),
        canDeactivate: [NavigationGuard]
      },
      {
        path: ':projectId/reachability/call-chain',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/reachability-ui-product-vision/global-call-chain/global-call-chain.module').then(m => m.GlobalCallChainModule),
        data: { title: 'globalCallChainPageTitle', role: ProjectRole.UserRoleEnum.VIEWER }
      },
      {
        path: ':projectId/:moduleHash/annotation-editor',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/eclipse-annotation-editor/eclipse-annotation-editor.module').then(m => m.EclipseAnnotationEditorModule)
      },
      {
        path: ':projectId/:moduleHash/data-dictionary-entry-editor',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/eclipse-data-dictionary-entry-editor/eclipse-data-dictionary-entry-editor.module')
                              .then(m => m.EclipseDataDictionaryEntryEditorModule)
      },
      {
        path: ':projectId/taxonomy-assignments',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/eclipse-taxonomy-assignment/eclipse-taxonomy-assignment.module').then(m => m.EclipseTaxonomyAssignmentModule)
      },
      {
        path: ':projectId/hash/:linkHash', data: { role: ProjectRole.UserRoleEnum.VIEWER } , component: ErrorComponent
      },
      {
        path: ':projectId/external/:pageIdentifier',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/extension-host/extension-host.module').then(m => m.ExtensionHostModule),
      },
      {
        path: ':projectId/custom-table/:pageIdentifier',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/table-extension-host/table-extension-host.module').then(m => m.TableExtensionHostModule),
      },
      {
        path: ':projectId/semantic-search',
        canDeactivate: [NavigationGuard],
        loadChildren: () => import('./modules/semantic-search/semantic-search.module').then(m => m.SemanticSearchModule),
        canActivate: [FeaturesGuard],
        data: {
          requiredFeature: 'semanticSearch'
        }
      }
    ]),
    {
      path: 'admin/client-projects',
      canDeactivate: [NavigationGuard],
      loadChildren: () => import('./modules/manage-client-and-projects/manage-client-and-projects.module').then(m => m.ManageClientAndProjectsModule)
    }
  ]),
  {
    path: 'error',
    loadChildren: () => import('./modules/error-module/error.module')
      .then(m => m.ErrorModule),
    data: { title: 'Error' }
  },
  {
    path: 'login-error',
    loadChildren: () => import('./modules/login-error/login-error.module')
      .then(m => m.LoginErrorModule),
    data: { title: 'Login Error' }
  },
  // Fallback when no prior route is matched
 { path: '**', redirectTo: 'clients', pathMatch: 'full', data: { error: 404 } }
];

@NgModule({
  imports: [
    RouterModule.forRoot(routes, {
    useHash: true,
    scrollPositionRestoration: 'enabled'
})],
  exports: [RouterModule],
  providers: []
})
export class AppRoutingModule {}
