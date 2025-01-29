import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Data, Params } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { AnnotationEditor, FormResponse, FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { combineLatest, Subscription } from 'rxjs';
import { AnnotationControllerService, AnnotationPojo, ModuleLocation } from '@innowake/mining-api-angular-client';

declare const java__callback__onSave: () => Promise<string>;

declare const java__callback__onDelete: () => Promise<string>;

declare const java__callback__onCancel: () => Promise<string>;

@Component({
  selector: 'app-eclipse-annotation-editor',
  templateUrl: './eclipse-annotation-editor.component.html'
})
export class EclipseAnnotationEditorComponent implements OnInit, OnDestroy {
  currentAnnotation: AnnotationPojo;
  currentClientProject: ClientProjectRelationship;
  isCreateMode: boolean;
  moduleId: number;
  modulePath: string;
  moduleName: string;
  parentComponent = AnnotationEditor.ECLIPSE_EDITOR;
  private clientProjectSubscription: Subscription;

  constructor(
    private route: ActivatedRoute,
    private annotationService: AnnotationControllerService,
    private translateService: TranslateService,
    private clientProjectRelationshipService: ClientProjectRelationshipService) {
  }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationshipService.getClientProjectObservable()
      .subscribe((clientProjectRelationship: ClientProjectRelationship) => {
        this.currentClientProject = clientProjectRelationship;
        combineLatest([
          this.route.params,
          this.route.queryParams,
          this.route.data
        ]).subscribe(([routeParams, queryParams, routeData]: [Params, Params, Data]) => {
          this.modulePath = routeData.module?.path;
          this.moduleName = this.translateService.instant('annotationReporting.sharedAnnotationEditorTitle', {moduleName: routeData.module?.name});
          const annotationId: number = routeParams.annotationId;
          if (routeParams.annotationId) {
            this.annotationService.findAnnotationById(this.currentClientProject.getProjectId(), annotationId)
              .subscribe((annotation: AnnotationPojo) => {
                this.currentAnnotation = annotation;
              });
            this.isCreateMode = false;
          } else if (queryParams.length && queryParams.offset) {
            this.isCreateMode = true;
            const moduleLocation: ModuleLocation = {
              length: queryParams.length,
              offset: queryParams.offset
            };
            this.currentAnnotation = {
              projectId: this.currentClientProject.getProjectId(),
              location: moduleLocation,
            };
            this.moduleId = routeData.module.id;
          }
        });
      });
  }

  handleFormResult(response: FormResponse<AnnotationPojo>): void {
    switch (response.result) {
      case FormResult.Saved:
        void java__callback__onSave();
        break;
      case FormResult.Canceled:
        void java__callback__onCancel();
        break;
      case FormResult.Deleted:
        void java__callback__onDelete();
        break;
      default:
        break;
    }
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
  }
}
