import { Injectable } from '@angular/core';
import { GraphQlControllerService } from './graphql.service';
import { NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { Observable } from 'rxjs';
import { map } from 'rxjs/internal/operators/map';

export interface AnnotationItem {
  title: string;
  key: string;
  uid: string;
  isCheckBox: boolean;
  type: string;
  isDescriptionProvided: boolean;
  parentUid: string;
  generatedFrom?: { annotationId: string };
  annotationId?: number;
}

@Injectable({
  providedIn: 'root'
})
export class FunctionalGroupService {
  totalTreeElements: number;
  constructor(private graphQlControllerService: GraphQlControllerService) {}

  getFunctionalGroupTree(
    projectId: number,
    filterDetails: { [key: string]: any },
    pageNo: number,
    perPage: number
  ): Observable<NzTreeNodeOptions[]> {
    const taxonomyIds: string[] = [];
    if (filterDetails?.taxonomyIds) {
      filterDetails.taxonomyIds.forEach((taxonomyIdItem: string) => {
        taxonomyIds.push(...taxonomyIdItem.split('_')[1].split(','));
      });
    }
    let contentTypeFilter = '{content_type: {eq: FUNCTIONAL_GROUP}';
    const contentParentFilter = 'content_parents:{notEq: {content_type: {eq: FUNCTIONAL_GROUP}}}}';

    if (taxonomyIds.length) {
      contentTypeFilter += `, content_resolvedModuleParts_referencedTaxonomies_id: { in: [${taxonomyIds}] }`;
    }

    if (filterDetails?.moduleIds?.length) {
      contentTypeFilter += `, content_resolvedModuleParts_moduleId: { in: [${filterDetails.moduleIds}] }`;
    }

    if (filterDetails?.reachabilityIds?.length) {
      contentTypeFilter += `, content_peers: { eq: { content_uid: { in: [${filterDetails?.reachabilityIds}] } } }`;
    }
    const finalFilter = `${contentTypeFilter}, ${contentParentFilter}`;
    const requestQuery = {
      query: `{
        functionalBlocks(projectId: ${projectId},
          page :${pageNo},
          size:${perPage}
          filterObject: ${finalFilter}) {
          totalElements
          content {
            uid
            name
            description
            type
            flags
            generatedFrom {
              annotationId
            }
            childrenDeep  {
              content {
                name
                uid
                flags
                generatedFrom {
                annotationId
                }
                type
                children {
                  content {
                    uid
                    name
                    type
                    children {
                  content {
                    uid
                    name
                    type
                    flags
                    generatedFrom {
                      annotationId
                    }
                  }
                }
                    flags
                    generatedFrom {
                      annotationId
                    }
                  }
                }
              }
            }
          }
        }
      }`,
    };
    return this.graphQlControllerService.graphQl(requestQuery).pipe(
      map((response: { [key: string]: any }) => {
        if (response) {
          this.totalTreeElements = response.data.functionalBlocks.totalElements;
          return this.prepareTreeData(response.data.functionalBlocks.content as Array<{ [key: string]: any }>);
        } else {
          return [];
        }
      })
    );
  }

  private prepareTreeData(data: Array<{ [key: string]: any }>): NzTreeNodeOptions[] {
    const childrenArray: Array<{ [key: string]: any }> = [];
    const annotationsArray: Array<{ [key: string]: any }> = [];
    const processedData: any[] = [];
    data.forEach((dataItem: { [key: string]: any }) => {
      const functionalAnalysisItem = {
        title: dataItem.name,
        key: dataItem.uid,
        uid: dataItem.uid,
        description: dataItem.description,
        isCheckBox: false,
        type: dataItem.flags?.TYPE[0],
        isDescriptionProvided: true,
        children: childrenArray,
        annotations: annotationsArray as AnnotationItem[]
      };
      if (dataItem.childrenDeep?.content?.length > 0) {
        const currentItem: Array<{ [key: string]: any }> = dataItem.childrenDeep?.content.filter(
          (content: { flags: { TYPE: string[] } }) => content.flags?.TYPE[0] === 'FUNCTIONAL_GROUP'
        );
        functionalAnalysisItem.children = this.prepareNestedLevels(
          currentItem as Array<{ [key: string]: any }>,
          dataItem.uid as string
        );
        functionalAnalysisItem.annotations = ((dataItem.childrenDeep.content as any[]) || [])
          .filter((subChild: { [key: string]: any }) => subChild.flags?.TYPE[0] === 'FUNCTIONAL_UNIT')
          .reduce((annotations: AnnotationItem[], subChild: { [key: string]: any }) => {
            const existingAnnotation = annotations.find((annotation) => annotation.uid === subChild.uid);
            if ( ! existingAnnotation) {
              annotations.push({
                title: subChild.name,
                key: subChild.uid,
                uid: subChild.uid,
                isCheckBox: false,
                type: 'FUNCTIONAL_UNIT',
                isDescriptionProvided: true,
                parentUid: subChild.uid,
                annotationId: subChild?.generatedFrom?.annotationId
              });
            }
            return annotations;
          }, []);
      }
      processedData.push(functionalAnalysisItem);
    });
    return this.removeDuplicates(processedData);
  }

  private removeDuplicates<T>(processedData: T[]): T[] {
    const uniqueUids = new Set<string>();
    const removeDuplicatesRecursive = (data: T[]): T[] => {
      const filteredData: T[] = [];
      for (const item of data) {
        if ( ! uniqueUids.has(String((item as any).uid))) {
          uniqueUids.add(String((item as any).uid));
          const newItem = { ...item };
          if ((newItem as any).children && (newItem as any).children.length > 0) {
            (newItem as any).children = removeDuplicatesRecursive((newItem as any).children as T[]);
          }
          filteredData.push(newItem);
        }
      }
      return filteredData;
    };
    return removeDuplicatesRecursive(processedData);
  }

  private prepareNestedLevels(data: any[], parentUid: string): any[] {
    return data
      .map((child: { [key: string]: any }) => {
        if (child?.flags?.TYPE[0] !== 'FUNCTIONAL_UNIT') {
          return {
            title: child.name,
            key: child.uid,
            uid: child.uid,
            isCheckBox: false,
            type: child.flags?.TYPE[0],
            isDescriptionProvided: true,
            parentUid,
            children: ((child.children?.content as any[]) || [])
              .filter((subChild: { [key: string]: any }) => subChild.flags?.TYPE[0] !== 'FUNCTIONAL_UNIT')
              .map((subChild: { [key: string]: any }) => ({
                title: subChild.name,
                key: subChild.uid,
                uid: subChild.uid,
                isCheckBox: false,
                type: subChild.flags?.TYPE[0],
                isDescriptionProvided: true,
                parentUid: subChild.uid,
                annotations: ((subChild.children?.content as any[]) || [])
                  .filter((subChild: { [key: string]: any }) => subChild?.flags?.TYPE[0] === 'FUNCTIONAL_UNIT')
                  .map((subChild: { [key: string]: any }) => ({
                    title: subChild.name,
                    key: subChild.uid,
                    uid: subChild.uid,
                    isCheckBox: false,
                    type: 'FUNCTIONAL_UNIT',
                    isDescriptionProvided: true,
                    parentUid: subChild.uid,
                    annotationId: subChild?.generatedFrom?.annotationId,
                  })) as AnnotationItem[],
                children: this.prepareNestedLevels((subChild.children?.content as any[]) || [], subChild.uid as string),
              })),
            annotations: ((child.children?.content as any[]) || [])
              .filter((subChild: { [key: string]: any }) => subChild?.flags?.TYPE[0] === 'FUNCTIONAL_UNIT')
              .map((subChild: { [key: string]: any }) => ({
                title: subChild.name,
                key: subChild.uid,
                uid: subChild.uid,
                isCheckBox: false,
                type: 'FUNCTIONAL_UNIT',
                isDescriptionProvided: true,
                parentUid: subChild.uid,
                annotationId: subChild?.generatedFrom?.annotationId,
              })) as AnnotationItem[]
          };
        }
        return null;
      })
      .filter(Boolean);
  }
}
