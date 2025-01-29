import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ClientProjectAuthorizationGuard } from '@app/core/authorization/client-project-authorization.guard';
import { ModuleResolverService } from '@app/core/services/route-data-resolver/module-resolver.service';
import { EclipseAnnotationEditorComponent } from './eclipse-annotation-editor.component';
import { ProjectRole } from '@innowake/mining-api-angular-client';

const title = 'annotationReporting.pageTitle';
const routes: Routes = [
    {
      path: '',
      resolve: {
        module: ModuleResolverService
      },
      canActivate: [ClientProjectAuthorizationGuard],
      data: {reuse: false},
      children: [
        {
          path: '',
          component: EclipseAnnotationEditorComponent,
          data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
        },
        {
          path: ':annotationId',
          component: EclipseAnnotationEditorComponent,
          data: { title, role: ProjectRole.UserRoleEnum.VIEWER }
        }
      ]
    }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class EclipseAnnotationEditorRoutingModule { }
