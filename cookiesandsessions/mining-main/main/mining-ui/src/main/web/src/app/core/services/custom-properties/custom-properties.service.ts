import { Injectable } from '@angular/core';
import { forkJoin, Observable } from 'rxjs';
import { map, mergeMap } from 'rxjs/operators';
import { GraphQlControllerService } from '../graphql.service';
import { CustomPropertyMetadata, MetamodelControllerService } from '@innowake/mining-api-angular-client';

export interface CustomPropertyMetadataWithClass extends CustomPropertyMetadata {
  customPropertyClassName?: string
}

@Injectable({
  providedIn: 'root'
})
export class CustomPropertiesService {

  constructor(
    private graphQlControllerService: GraphQlControllerService,
    private metamodelService: MetamodelControllerService
  ) { }

  /**
   * Get the list of custom property Metadata by custom property classes for a class
   * @param className Name of the class for which we wand the custom properties metadata (ie: Annotation, Module,...)
   * @param projectId Id of the current to which the custom properties are linked
   * @returns An object containing the Custom property classes as properties and the Custom Properties Metadat array as values
   */
  getCustomPropertiesMetadataForClass(className: string, projectId: number): Observable<CustomPropertyMetadataWithClass[]> {
    return this.graphQlControllerService.graphQl({
      query: 'query ($projectId: EntityId!) { project(projectId: $projectId) { customPropertyClasses } }',
      variables: { projectId }
    }).pipe(
      map((graphQlResult: { [key: string]: any }) => graphQlResult.data.project.customPropertyClasses[className]),
      mergeMap((customPropertyClasses: string[]) => {
        if ( ! customPropertyClasses) {
          return [];
        }
        return forkJoin(customPropertyClasses.map((customPropertyClass) => this.metamodelService.findMetaModel(customPropertyClass))).pipe(
          map((metadataList) => {
            let customProperties: CustomPropertyMetadata[] = [];
            customPropertyClasses.forEach((customPropertyClass, index) => {
              customProperties = customProperties.concat(metadataList[index].map(metadata => ({...metadata, customPropertyClassName: customPropertyClass })));
            });
            customProperties.sort((prop1, prop2) => +prop1.customViewIndex - +prop2.customViewIndex);
            return customProperties;
          })
        );
      })
    );
  }

  /**
   * Get the POroject's map of autocompletion values for all Custom properties
   * @param projectId Id of the current project
   * @returns Map of autocompletion values
   */
  getAutoCompletionValues(projectId: number): Observable<{ [key: string]: string[]}> {
    return this.graphQlControllerService.graphQl({
      query: 'query ($projectId: EntityId!) { project(projectId: $projectId) { autoCompletionMap } }',
      variables: { projectId }
    }, projectId).pipe(
      map((response: { [key: string]: any }) => response.data.project.autoCompletionMap)
    );
  }
}
