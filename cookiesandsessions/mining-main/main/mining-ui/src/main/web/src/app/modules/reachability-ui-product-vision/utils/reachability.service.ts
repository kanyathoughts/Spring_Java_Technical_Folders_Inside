import { Inject, Injectable } from '@angular/core';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { buildFilterObject } from '@app/core/utils/graphql.util';
import { Aggregation, BlockViewFilter,
  MergeBlockData,
  ReachabilityBlocks, ReachabilityCallchainData, ReferencedTaxonomies } from './reachability-interface';
import { catchError, map } from 'rxjs/operators';
import { BehaviorSubject, Observable, Subject, forkJoin, of } from 'rxjs';
import { Logger } from '@app/core';
import {
  AggregationRequestModuleFieldName, FunctionalBlockControllerService,
  FunctionalBlockMergeRequest, ModuleControllerService
} from '@innowake/mining-api-angular-client';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { TranslateService } from '@ngx-translate/core';
import { MergeBlockComponent } from '../block-view/merge-block/merge-block.component';
import { NzMessageService } from 'ng-zorro-antd/message';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { WindowToken } from '@app/core/utils/window';
import { dateFormatter } from '@app/core/utils/date-formatter.utils';
import { FbTotalElementsCountGQL } from '@app/graphql/generated/generated';

const log = new Logger('Reachability Block View');

@Injectable({
  providedIn: 'root'
})
export class ReachabilityService {
  totalElements: number;
  updateOutdatedAndDeletedRbBanner = new BehaviorSubject<boolean>(false);
  updateGraph = new Subject<boolean>;
  mergeStatus = new Subject<string>;
  updateOutdatedBlock: boolean;
  private showSwitchState = false;
  private updateBlocks = false;

  constructor(private graphQlControllerService: GraphQlControllerService,
    private moduleService: ModuleControllerService,
    private modalService: NzModalService,
    private translateService: TranslateService,
    private functionalBlockControllerService: FunctionalBlockControllerService,
    private fbCountGql: FbTotalElementsCountGQL,
    private messageService: NzMessageService,
    @Inject(WindowToken) private $window: Window
  ) { }

  /**
   * Method to get the reachability blocks
   * @param projectId project id of the project.
   * @param pageSize page size.
   * @param pageIndex current page index.
   * @param nameSearch search string for block name.
   * @param blockViewFilter filters applied for reachability block
   * @returns list of reachability blocks present in the page.
   */
  getReachabilityBlocks(projectId: number, pageSize: number, pageIndex: number,
     nameSearch?: string, blockViewFilter?: BlockViewFilter, blockId?: string):
    Observable<ReachabilityBlocks[]> {

    const blockFilter = this.buildReachabilityFilter(blockViewFilter, nameSearch, blockId);
    const reachabilityRequest = {
      'query': `query ($filter: FilterObject_functionalBlocks) {
          reachabilityBlocks: functionalBlocks(projectId: ${projectId}, size: ${pageSize}, page: ${pageIndex}, filterObject: $filter) {
            content {
              uid
              name
              description
              type
              status
              outdatedBlock
              blocksWithDeletedUB
              resolvedModuleParts {
                module {
                  id
                  errorCount
                  dependencies(direction: OUT, filterObject: { content_identification: { eq: MISSING } }) {
                    module {
                      id
                    }
                  }
                }
                referencedTaxonomies {
                  name
                }
              }
              peers(peerType:FUNCTIONAL_GROUP) {
                totalElements
              }
              upperBound: childrenDeep(maxDepth: 2, filterObject: {content_type: {eq: RA_UPPER_BOUND}}) {
                content {
                  children {
                    content {
                      generatedFrom {
                        module {
                          name
                          linkHash
                          id
                          technology
                          type
                        }
                      }
                    }
                  }
                }
              }
              lowerBound: childrenDeep(maxDepth: 2, filterObject: {content_type: {eq: RA_LOWER_BOUND}}) {
                aggregations {
                  groupBy {
                    REFERENCED_MODULE_TYPE
                    REFERENCED_MODULE_TECHNOLOGY
                  }
                  fields {
                    UID {
                      COUNT
                    }
                  }
                }
              }
            }
            totalElements
          }
        }`,
      'variables': {
        filter: blockFilter
      }
    };

    return this.graphQlControllerService.graphQl(reachabilityRequest).pipe(
      map((reachabilityResult: { [key: string]: any }) => {
        const reachabilityBlocks: ReachabilityBlocks[] = [];
        this.totalElements = reachabilityResult?.data.reachabilityBlocks.totalElements || 0;
        if (reachabilityResult && reachabilityResult?.data.reachabilityBlocks.content?.length) {
          reachabilityResult.data.reachabilityBlocks.content.forEach((blocks: { [key: string]: any }) => {
            const reachabilityBlock: ReachabilityBlocks = this.createReachabilityBlock(blocks);
            reachabilityBlock['totalCount'] = blocks.peers?.totalElements ?? 0;
            reachabilityBlock['resolvedModuleParts'] = blocks.resolvedModuleParts;
            reachabilityBlock['upperBound'].push(...this.createUpperBound(blocks).upperBound);
            reachabilityBlock['lowerBound'] = [{
              aggregations: this.createLowerBoundAggregation(blocks)
            }];
            reachabilityBlocks.push(reachabilityBlock);
          });
          return reachabilityBlocks;
        } else {
          return [];
        }
      }),
      catchError(() => of([]))
    );
  }

  /**
   * Method to get the count of reachability block
   * @param projectId project id of the project.
   * @param size page size.
   * @param page current page index.
   * @param nameSearch search string for block name.
   * @param blockViewFilter filters applied for reachability block
   * @returns number of reachability blocks present.
   */
  getReachabilityBlocksCount(projectId: number, size: number, page: number,
      nameSearch?: string, blockViewFilter?: BlockViewFilter, blockId?: string): Observable<number> {
    const filter = this.buildReachabilityFilter(blockViewFilter, nameSearch, blockId);

    return this.fbCountGql.fetch({ projectId, page, size, filter }, { fetchPolicy: 'network-only' }).pipe(
      map((result) => result.data.functionalBlocks.totalElements),
      catchError(() => of(0))
    );
  }


  /**
   * Method to get details(lowerbound, contains & shared) of a reachability block by its uid.
   * @param projectId project id of the project.
   * @param blockId uid of the block.
   * @returns details of given reachability block.
   */
  getReachabilityBlockDetails(projectId: number, blockId: string): Observable<ReachabilityBlocks> {
    const requestQuery = {
      'query': `{
        functionalBlock(projectId: ${projectId},uid: ${blockId}) {
          uid
          name
          resolvedModuleParts {
            module {
              id
              errorCount
              dependencies(direction: OUT, filterObject: { content_identification: { eq: MISSING } }) {
                module {
                  id
                }
              }
            }
          }
          status
          upperBound: childrenDeep(maxDepth: 2, filterObject: {content_type: {eq: RA_UPPER_BOUND}}) {
            content {
              resolvedModuleParts {
                moduleId
                referencedTaxonomies {
                  id
                  name
                }
              }
              children {
                content {
                  generatedFrom {
                    module {
                      name
                      linkHash
                      id
                      type
                      technology
                    }
                  }
                }
              }
            }
            aggregations {
              groupBy {
                REFERENCED_MODULE_TYPE
                REFERENCED_MODULE_TECHNOLOGY
              }
              fields {
                UID {
                  COUNT
                }
              }
            }
          }
          lowerBound: childrenDeep(maxDepth: 2, filterObject: {content_type: {eq: RA_LOWER_BOUND}}) {
            aggregations {
              groupBy {
                REFERENCED_MODULE_TYPE
                REFERENCED_MODULE_TECHNOLOGY
              }
              fields {
                UID {
                  COUNT
                }
              }
            }
          }
          contains:childrenDeep(maxDepth: 2, filterObject: {content_type: {eq: CALL_CHAIN}}) {
            aggregations {
              groupBy {
                REFERENCED_MODULE_TYPE
                REFERENCED_MODULE_TECHNOLOGY
              }
              fields {
                UID {
                  COUNT
                }
              }
            }
          }
          shared: childrenDeep(
            maxDepth: 2
            filterObject: {content_type: {eq: CALL_CHAIN}}
          ) {
            content {
              children(
                filterObject: {content_parents: {eq: {content_type: {eq: CALL_CHAIN},
                content_parents: {notEq: {content_uid: {eq: ${blockId}}}, eq: {content_type: {eq: RA_TOP_DOWN}}}}}}
              ) {
                aggregations {
                  groupBy {
                    REFERENCED_MODULE_TYPE
                    REFERENCED_MODULE_TECHNOLOGY
                  }
                  fields {
                    UID {
                      COUNT
                    }
                  }
                }
              }
            }
          }
        }
      }`
    };
    return this.graphQlControllerService.graphQl(requestQuery).pipe(map((reachabilityResult: { [key: string]: any }) => {
      if (reachabilityResult && reachabilityResult.data?.functionalBlock) {
        const reachabilityBlocks: { [key: string]: any } = reachabilityResult?.data.functionalBlock;
        const reachabilityDetail: ReachabilityBlocks = {
          uid: reachabilityResult.data.functionalBlock.uid,
          name: reachabilityResult.data.functionalBlock?.name,
          upperBound: [],
          lowerBound: [],
          linesOfCode: 0,
          complexity: 'Unknown',
          status: null
        };
        reachabilityDetail['resolvedModuleParts'] = reachabilityBlocks.resolvedModuleParts;
        reachabilityDetail['upperBound'].push(...this.createUpperBound(reachabilityBlocks).upperBound);
        reachabilityDetail['lowerBound'] = [{
          aggregations: this.createLowerBoundAggregation(reachabilityBlocks)
        }];
        reachabilityDetail['contains'] = [{
          aggregations: this.createContainsAggregation(reachabilityBlocks as { [key: string]: any })
        }];
        reachabilityDetail['sharedModule'] = [{
          aggregations: this.createSharedAggregation(reachabilityBlocks?.shared as { [key: string]: any })
        }];
        reachabilityDetail['status'] = reachabilityBlocks.status;
        return reachabilityDetail;
      } else {
        return {};
      }
    }),
      catchError(() => of({}))
    );
  }

  /**
   * Method to get all reachability block Ids irrespective of pagination.
   * @param projectId project id of the project.
   * @param taxonomyIds list of taxonomy ids.
   * @param showInactiveBlocks boolean value hide/show inactive blocks.
   * @returns list of all block Ids.
   */
  getReachabilityBlocksId(projectId: number, blockViewFilter: BlockViewFilter, nameSearch: string): Observable<string[]> {
    const blockFilter = this.buildReachabilityFilter(blockViewFilter, nameSearch);
    const requestQuery = {
      'query': `query ($filter: FilterObject_functionalBlocks) {
        functionalBlocks(projectId: ${projectId}, filterObject: $filter) {
          content {
            uid
          }
        }
      }`,
      'variables': {
        filter: blockFilter
      }
    };
    return this.graphQlControllerService.graphQl(requestQuery).pipe(map((response: { [key: string]: any }) => {
      const functionalBlocks = response?.data.functionalBlocks?.content;
      return functionalBlocks?.map((block: any) => block.uid);
    }));
  }

  /**
   * Method to get all merged blocks irrespective of pagination.
   * @param projectId project id of the project.
   * @param blockId uid of the block.
   * @returns list of merged block Ids.
   */
  getMergedBlocksId(projectId: number, blockId: string): Observable<string[]> {
    const requestQuery = {
      'query': `{
        functionalBlocks(
          projectId: ${projectId}
          filterObject: {content_uid: {eq: ${blockId}}}
        ) {
          content {
            reachabilityData: children(filterObject: {content_type: {eq: RA_TOP_DOWN}}) {
              content {
                uid
              }
            }
          }
        }
      }`
    };
    return this.graphQlControllerService.graphQl(requestQuery).pipe(map((response: { [key: string]: any}) => {
      if (response && response?.data?.functionalBlocks?.content) {
        const mergedBlocks =  response.data.functionalBlocks.content[0]?.reachabilityData.content;
        return mergedBlocks?.map((block: any) => block.uid);
      } else {
        return [];
      }
    }),
    catchError(() => of([])));
  }

  /**
   * Method tp get the total number of outdated RB.
   * @param projectId Id of the project
   * @returns count of outdated RB.
   */
  getCountOfOutdatedBlocks(projectId: number, blockId?: string): Observable<number> {
    const blockFilter: { [key: string]: any } = {content_type: {eq: 'RA_TOP_DOWN'}};
    blockFilter['content_outdatedBlock'] = {eq: true};
    blockFilter['content_blocksWithDeletedUB'] = {eq: false};
    if (blockId) {
      blockFilter['content_uid'] = { eq: blockId };
    }
    const requestQuery = {
      'query': `query ($filter: FilterObject_functionalBlocks) {
        functionalBlocks(projectId: ${projectId}, filterObject: $filter) {
          totalElements
        }
      }`,
      'variables': {
        filter: blockFilter
      }
    };
    return this.graphQlControllerService.graphQl(requestQuery).pipe(map((outdatedBlocks: { [key: string]: any }) => {
      if (outdatedBlocks && outdatedBlocks.data?.functionalBlocks) {
        return outdatedBlocks.data.functionalBlocks?.totalElements;
      }
    }));
  }

  /**
   * Method tp get the total number of deleted Upper bound Modules.
   * @param projectId Id of the project
   * @param blockId Id the RB.
   * @returns count of deleted Upper bound Modules.
   */
  getCountOfDeletedModules(projectId: number, blockId?: string, mergeParent?: boolean): Observable<number> {
    if (mergeParent) {
      return this.getCountOfMergedDeletedModules(projectId, blockId);
    } else {
      const blockFilter: { [key: string]: any } = {content_type: {eq: 'RA_TOP_DOWN'}};
      blockFilter['content_blocksWithDeletedUB'] = {eq: true};
      if (blockId) {
        blockFilter['content_uid'] = { eq: blockId };
      }
      const requestQuery = {
        'query': `query ($filter: FilterObject_functionalBlocks) {
          functionalBlocks(projectId: ${projectId}, filterObject: $filter) {
            totalElements
          }
        }`,
        'variables': {
          filter: blockFilter
        }
      };
      return this.graphQlControllerService.graphQl(requestQuery).pipe(map((deletedModules: { [key: string]: any }) => {
        if (deletedModules && deletedModules.data?.functionalBlocks) {
          return deletedModules.data?.functionalBlocks.totalElements;
        }
      }));
    }
  }

  /**
   * Fetches name and description of a reachability block.
   * @param uid uid of the reachability block.
   * @param projectId project id of the project.
   * @returns name and description of the reachability block.
   */
  fetchReachabilityBlockDetails(uid: string, projectId: number): Observable<{ name: string, description: string }> {
    const requestQuery = {
      'query': `{
          functionalBlocks(
            projectId:  ${projectId},
            filterObject: { content_uid: { eq: "${uid}" } }
          ) {
            content {
              name
              description
            }
          }
        }`
    };

    return this.graphQlControllerService.graphQl(requestQuery).pipe(
      map((response: { [key: string]: any}) => {
        if (response && response?.data?.functionalBlocks?.content) {
          const functionalBlockRecord: { [key: string]: any} = response.data.functionalBlocks.content[0];
          return {
            name: functionalBlockRecord.name,
            description: functionalBlockRecord.description
          };
        } else {
          return { name: '', description: '' };
        }
      }),
      catchError(() => of({ name: '', description: '' }))
    );
  }

  checkToEnableMerge(projectId: number): Observable<string> {
    const requestQuery = {
      'query': `{
            functionalBlocks(projectId: ${projectId}, filterObject: {content_type:{eq: REACHABILITY_NETWORK}}) {
              content {
                uid
              }
            }
          }`
    };
    return this.graphQlControllerService.graphQl(requestQuery).pipe(map((result: { [key: string]: any }) => {
      if (result.data?.functionalBlocks?.content && result.data?.functionalBlocks?.content.length) {
        return result.data?.functionalBlocks?.content[0]?.uid;
      } else {
        return '';
      }
    }),
      catchError(() => of('')));
  }

  /**
   * Method to fetch call chain details of a reachability block.
   * @param projectId project id of the project.
   * @param blockId uid of a reachability block.
   * @returns call chain details of a reachability block.
   */
  fetchReachabilityCallChainDetails(projectId: number, blockId: string): Observable<ReachabilityCallchainData> {
    const requestQuery = {
      'query': `{
        functionalBlock(
          projectId: ${projectId},
          uid: ${blockId}) {
          name
          callChain: children(filterObject: {content_type: {eq: CALL_CHAIN}}) {
              content {
                uid
              }
          }
        }
      }`
    };
    return this.graphQlControllerService.graphQl(requestQuery).pipe(map((result: { [key: string]: any }) => {
      if (result && result.data?.functionalBlock) {
        return result.data.functionalBlock;
      } else {
        return {};
      }
    }),
      catchError(() => of({})));
  }

  /**
   * Method to get the selected blocks details.
   * @param blocks details.
   * @returns the selected blocks details.
   */
  createReachabilityBlock(blocks: { [key: string]: any }): ReachabilityBlocks {
    const resolvedModuleParts = blocks.resolvedModuleParts as Array<Record<string, string>> || [];
    return {
      uid: blocks.uid,
      name: blocks.name,
      description: blocks.description,
      type: blocks.type,
      isSelected: false,
      status: blocks.status,
      outdatedModule: blocks.outdatedBlock,
      deletedModule: blocks.blocksWithDeletedUB,
      blockState: this.getBlockState(resolvedModuleParts),
      upperBound: [],
      lowerBound: [],
      totalCount: blocks.peers?.totalElements ?? 0
    };
  }

  /**
   * Method to create the upper bound details.
   * @param blocks contains blocks details.
   * @returns the updated upper bound details.
   */
  createUpperBound(blocks: { [key: string]: any }): ReachabilityBlocks {
    const referencedTaxonomies: ReferencedTaxonomies[] = blocks?.resolvedModuleParts?.map((ref: any) => ({
        moduleId: ref.module?.id ? ref.module.id : null,
        referencedTaxonomies: ref.referencedTaxonomies ? ref.referencedTaxonomies : []
      })) || [];
    const taxonomies = [].concat(...referencedTaxonomies);
    const upperBlock: ReachabilityBlocks = {
      upperBound: []
    };
    const upperBounds = blocks?.upperBound?.content.reduce(
      (acc: any, item: any) => (acc.concat(({content:item.children.content, resolvedModuleParts: item.resolvedModuleParts}))), []);
    upperBounds?.forEach((bound: any) => {
      const {content, resolvedModuleParts} = bound;
      let upperTaxonomies = [];
      if(resolvedModuleParts && resolvedModuleParts.length && resolvedModuleParts[0].referencedTaxonomies) {
        upperTaxonomies = resolvedModuleParts[0].referencedTaxonomies;
      }
      if (content[0]?.generatedFrom?.module) {
        upperBlock['upperBound'].push({
          name: content[0].generatedFrom.module.name,
          id: content[0].generatedFrom.module.id,
          linkHash: content[0].generatedFrom.module.linkHash,
          technology: content[0].generatedFrom.module.technology,
          type: content[0].generatedFrom.module.type,
          taxonomy: taxonomies,
          upperTaxonomies
        });
      } else {
        upperBlock['upperBound'].push({});
      }
    });
    return upperBlock;
  }


  /**
   * Description
   * @param selectedNode:particular selected node
   * @param projectId: current project id
   * @param mergeParent=false
   * @param selectedBlocks: selected Nodes in case of multiple selection
   */
  storeReachabilityDetails(selectedNode: {[key: string]: any}, projectId: number, mergeParent=false,
    selectedBlocks: Array<{[key: string]: any}>= []): void {
    let blockDetails = {};
    const resolvedModuleParts = selectedNode?.resolvedModuleParts as Array<Record<string, string>> || [];
    if (selectedBlocks.length === 0) {
      blockDetails = {
        pageTitle: selectedNode.name,
        blockId: selectedNode.uid,
        mergeParent,
        outdated: selectedNode.info?.OUTDATED || selectedNode?.outdatedModule,
        deleted: selectedNode.info?.DELETED || selectedNode?.deletedModule,
        errorCount: selectedNode.errorCount,
        warningsCount: selectedNode.warningsCount
      };
      const moduleIds = resolvedModuleParts.filter((part: any) => part.module['errorCount'] > 0).map((part: any) => part.module.id);
      const warningIds = resolvedModuleParts.filter((part: any) => part.module['dependencies'].length > 0).map((part: any) => part.module.id);
      localStorage.setItem('ModulesWithErrorsAndWarnings', JSON.stringify({ errors: moduleIds, warnings: warningIds }));
    } else {
      blockDetails = {
        outdated: selectedBlocks.some(x => x.info.OUTDATED),
        deleted: selectedBlocks.some(x => x.info.Type?.includes('MERGE_PARENT'))
    };
    }
    localStorage.setItem(`${projectId}-reachabilityDetails`, JSON.stringify(blockDetails));
  }

  /**
   * Opens the modules table with the specified tag and project ID.
   * @param tag - The tag to filter the modules table. Can be 'errors' or 'warnings'.
   * @param projectId - The ID of the project.
   */
  openModulesTable(tag: string, projectId: number): void {
    const randomValue = new Uint8Array(5);
    crypto.getRandomValues(randomValue);
    const uniqueIdsKeys = `MissingErrorModules-${Array.from(randomValue)
      .map((byte) => byte.toString(36))
      .join('')}`;
    const modulesWithErrorsAndWarnings = JSON.parse(localStorage.getItem('ModulesWithErrorsAndWarnings'));
    if (tag === 'errors') {
      const uniqueIdsValue = { createdTime: dateFormatter(new Date()), moduleIds: modulesWithErrorsAndWarnings.errors };
      localStorage.setItem(uniqueIdsKeys, JSON.stringify(uniqueIdsValue));
      const path = 'modules?preFilter=' + `${uniqueIdsKeys}&columns=Module.errorCount`;
      openInNewTab(projectId, null , path, this.$window);
    } else {
      const uniqueIdsValue = { createdTime: dateFormatter(new Date()), moduleIds: modulesWithErrorsAndWarnings.warnings };
      localStorage.setItem(uniqueIdsKeys, JSON.stringify(uniqueIdsValue));
      const path = 'modules?preFilter=' + `${uniqueIdsKeys}&columns=Module.missingDependencyNames`;
      openInNewTab(projectId, null , path, this.$window);
    }
  }

  /**
   * Open modal to merge RB
   * @returns modal to input name and description for merge
   */
  openModalToMergeReachabilityBlocks(): NzModalRef {
    return this.modalService.create({
      nzTitle: this.translateService.instant('reachability.mergeBlockTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzWrapClassName: 'vertical-center-modal',
      nzContent: MergeBlockComponent
    });
  }

  /**
   * Method to merge blocks and use message service to display result
   * @param data: Name and Description from merge modal
   * @param projectId: project id
   * @param commonParent: common parent of the blocks
   * @param selectedBlockIds: selected block ids
   */
  mergeReachabilityBlocks(data: MergeBlockData, projectId: number, commonParent: string, selectedBlockIds: string[]): void {
    if (data) {
      const requestParams: FunctionalBlockMergeRequest = {
        commonParent,
        mergeParentPrototype: {
          name: data.blockName,
          description: data.blockDescription,
          project: projectId.toString(),
          flags: { TYPE: ['REACHABILITY', 'RA_TOP_DOWN'] }
        },
        mergeChildren: selectedBlockIds,
        removeEmptyBlocks: true
      };
      this.functionalBlockControllerService.mergeFunctionalBlock(projectId, requestParams).subscribe(() => {
          const successContent: string = this.translateService.instant('reachability.mergedSuccessfully');
          this.messageService.success(successContent);
          this.setMergeStatus(this.translateService.instant('messageService.success') as string);
      }, () => {
        const errorContent: string = this.translateService.instant('reachability.mergeBlockError');
        this.messageService.error(errorContent);
      });
    }
  }

  /**
   * method to create the lower bound details.
   * @param blocks contains blocks details.
   * @returns aggregated lower bound details.
   */
  createLowerBoundAggregation(blocks: { [key: string]: any }): Aggregation[] {
    const lowAggregation: Aggregation[] = [];
    // merge and add counts for same module type
    blocks?.lowerBound?.aggregations?.forEach((low: Aggregation) => {
      const existingItem = lowAggregation.find(i => i.groupBy.REFERENCED_MODULE_TYPE === low.groupBy.REFERENCED_MODULE_TYPE);
      if (existingItem) {
        existingItem.fields.UID.COUNT += low.fields.UID.COUNT;
      } else {
        lowAggregation.push(low);
      }
    });
    lowAggregation?.sort((a, b) => b.fields.UID?.COUNT - a.fields.UID?.COUNT);
    return lowAggregation;
  }

  /**
   * Makes the request to fetch LOC and Complexity for the Reachability block Modules.
   * @param reachabilityBlock block for which data needs to be fetched
   * @param projectId project id of the block.
   */
  createLocAndComplexity(reachabilityBlock: ReachabilityBlocks, projectId: number): void {
    const filterId: number[] = this.getModuleIdsFromBlockResolvedModule(reachabilityBlock.resolvedModuleParts);
    let linesOfCode = 0;
    let complexity = 'Unknown';
    let locFilter = {};
    let complexityFilter = {};
    if (filterId.length) {
      locFilter = {
        LINES_OF_CODE: { gte: 1 },
        ID: { in: filterId }
      };
      complexityFilter = {
        COMPLEXITY: { gte: 0 },
        ID: { in: filterId }
      };
    }
    const locRequest: AggregationRequestModuleFieldName = {
      filterObject: locFilter,
      fields: {
        LINES_OF_CODE: 'SUM'
      }
    };
    const complexityRequest: AggregationRequestModuleFieldName = {
      filterObject: complexityFilter,
      fields: {
        COMPLEXITY: 'SUM'
      }
    };
    forkJoin([this.moduleService.getAggregatedValues2(projectId, locRequest),
    this.moduleService.getAggregatedValues2(projectId, complexityRequest)
    ]).subscribe(([locResult, complexityResult]: any) => {
      if (locResult?.length) {
        locResult.forEach((loc: any) => {
          linesOfCode = loc?.fields?.LINES_OF_CODE;
        });
        reachabilityBlock['linesOfCode'] = linesOfCode;
      }
      if (complexityResult?.length) {
        complexityResult.forEach((res: any) => {
          if (res.fields.COMPLEXITY > 0 && res.fields.COMPLEXITY <= 10) {
            complexity = 'Low';
          } else if (res.fields.COMPLEXITY <= 20) {
            complexity = 'Medium';
          } else if (res.fields.COMPLEXITY <= 50) {
            complexity = 'High';
          } else if (res.fields.COMPLEXITY > 50) {
            complexity = 'Very High';
          } else {
            complexity = 'Unknown';
          }
        });
        reachabilityBlock['complexity'] = complexity;
      }
    }, (err) => {
        log.error('Error while fetching types: ' + err.message);
    });
  }

  /**
   * Updates the status of Reachability blocks in a project.
   *
   * @param projectId - The ID of the project.
   * @param status - The status to set for the blocks. Must be either 'ACTIVE' or 'INACTIVE'.
   * @param ids - An array of block IDs to update.
   * @param inactiveSuccessDescription - The success description to display when setting blocks to 'INACTIVE'.
   * @param onSuccess - Optional. A callback function to execute on success.
   * @param onError - Optional. A callback function to execute on error.
   */
  updateBlockStatus(projectId: number, status: 'ACTIVE' | 'INACTIVE', ids: string[], inactiveSuccessDescription: string,
    onSuccess: () => void = () => {}, onError: () => void = () => {}): void {
    this.functionalBlockControllerService.updateBlockStatus(projectId, status, ids).subscribe(() => {
      const successContent: string = status === 'ACTIVE' ? this.translateService.instant('reachability.activeSuccess') : inactiveSuccessDescription;
      this.messageService.success(successContent);
      onSuccess();
    }, () => {
      const errorContent: string = this.translateService.instant('reachability.statusErrorMessage');
      this.messageService.error(errorContent);
      onError();
    });
  }

  /**
   * Getter method for state of switch
   */
  getSwitchState(): boolean {
    return this.showSwitchState;
  }

  /**
   * Setter method for state of switch
   * @param value State of switch
   */
  setSwitchState(value: boolean): void {
    this.showSwitchState = value;
  }

  /**
   * Getter method for state of update blocks
   * @returns boolean value based on the status.
   */
  getUpdateBlocks(): boolean {
    return this.updateBlocks;
  }

  /**
   * Setter method for state of update blocks
   * @param value State of update blocks
   */
  setUpdateBlocks(value: boolean): void {
    this.updateBlocks = value;
  }

  /**
   * Getter method updating the outdated RB.
   */
  getUpdateOutdatedState(): boolean {
    return this.updateOutdatedBlock;
  }

  /**
   * Setter method for updating the outdated RB.
   * @param state state of outdated RB to update.
   */
  setUpdateOutdatedState(state: boolean): void {
    this.updateOutdatedBlock = state;
  }

  /**
   * Get status of blocks updated or removed.
   * @returns boolean value based on the status.
   */
  getNotifyAlertBannerForOutdatedOrRemovedRb(): Observable<boolean> {
    return this.updateOutdatedAndDeletedRbBanner;
  }

  /**
   * set the blocks status based on user action.
   * @param value boolean value to be set.
   */
  setNotifyAlertBannerForOutdatedOrRemovedRb(value: boolean): void {
    this.updateOutdatedAndDeletedRbBanner.next(value);
  }

  /**
   * method to get the updated graph
   * @returns boolean value based on the status.
   */
  getUpdateGraph(): Observable<boolean> {
    return this.updateGraph;
  }

  /**
   * method to set the updated graph
   * @param value: true or false
   */
  setUpdateGraph(value: boolean): void {
    this.updateGraph.next(value);
  }

  /**
   * method to get the status of merge block
   * @returns boolean value based on the status.
   */
  getMergeStatus(): Observable<string> {
    return this.mergeStatus;
  }

  /**
   * method to set the status of merge block
   * @param value: true or false
   */
  setMergeStatus(value: string): void {
    this.mergeStatus.next(value);
  }

  getBlockState(resolvedModuleParts: Array<Record<string, string>>): { errorCount: number, warningsCount: number } {
    return {
      errorCount: resolvedModuleParts.filter((part: any) => part.module['errorCount'] > 0).length,
      warningsCount: resolvedModuleParts.filter((item: any) => item.module['dependencies'].length > 0).length
    };
  }

  private getCountOfMergedDeletedModules(projectId: number, blockId: string): Observable<number> {
    const mergedFilter = {content_uid: {eq: blockId}};
    const mergedRequest = {
      'query': `query ($filter: FilterObject_functionalBlocks) {
        functionalBlocks(projectId: ${projectId}, filterObject: $filter) {
          content {
            reachabilityData: children(filterObject: {content_type: {eq: RA_TOP_DOWN}}) {
              content {
                blocksWithDeletedUB
              }
            }
          }
        }
      }`,
      'variables': {
        filter: mergedFilter
      }
    };
    return this.graphQlControllerService.graphQl(mergedRequest).pipe(map((deletedModules: { [key: string]: any }) => {
      if (deletedModules && deletedModules.data?.functionalBlocks?.content.length) {
        const reachabilityBlocks = deletedModules.data?.functionalBlocks?.content[0].reachabilityData;
        return reachabilityBlocks?.content?.filter((blocks: any) => blocks.blocksWithDeletedUB).length;
      }
    }));
  }

  private buildReachabilityFilter(blockViewFilter: BlockViewFilter, nameSearch?: string, blockId?: string): { [key: string]: any } {
    const reachabilityFilter = blockViewFilter?.reachabilityFilter;
    const content = [
      { name: 'type', path: 'content.type' },
      { name: 'id', path: 'content.resolvedModuleParts.referencedTaxonomies.id' }
    ];
    let blockFilter: { [key: string]: any } = {};

    const rbType = buildFilterObject([content[0]], [['RA_TOP_DOWN']]);
    blockFilter = { ...blockFilter, ...rbType };
    if (!blockViewFilter?.inActiveSwitch) {
      blockFilter['content_status'] = { notEq: 'INACTIVE' };
    }
    if (reachabilityFilter?.excludeParentTypes?.length > 1) {
      blockFilter['content_parents'] = { notEq: { content_type: { in: reachabilityFilter.excludeParentTypes } } };
    } else if (reachabilityFilter?.excludeParentTypes?.length === 1) {
      blockFilter['content_parents'] = { notEq: { content_type: { eq: reachabilityFilter.excludeParentTypes[0] } } };
    }
    if (nameSearch) {
      blockFilter['content_name'] = { eq: nameSearch };
    };
    if (blockId) {
      blockFilter['content_uid'] = { notEq: blockId };
    };
    // build filter object for reachability block for both block view and network view.
    if (reachabilityFilter?.taxonomies?.length) {
      const taxonomyFilter = buildFilterObject([content[1]], [reachabilityFilter.taxonomies as unknown as string[]]);
      blockFilter = { ...blockFilter, ...taxonomyFilter };
    };
    if (reachabilityFilter?.modulesIds?.length) {
      blockFilter['content_resolvedModuleParts_module_name'] = { in: reachabilityFilter.modulesIds };
    }
    if (reachabilityFilter?.functionalBlocks?.length) {
      blockFilter['content_peers'] = { eq: { content_name: { in: reachabilityFilter.functionalBlocks } } };
    }
    // build filter object for block view.
    if ( blockViewFilter && Object.entries(blockViewFilter).length && (reachabilityFilter?.type?.length || reachabilityFilter?.technology?.length ||
      reachabilityFilter?.referenceType?.length)) {
      blockFilter['content_children'] = {
        'eq': {
          'content_type': {
            'eq': 'RA_LOWER_BOUND'
          }
        }
      };

      if (reachabilityFilter?.type?.length) {
        blockFilter['content_children']['eq']['content_resolvedModuleParts_module_type'] = { in: reachabilityFilter.type };
      }

      if (reachabilityFilter?.technology?.length) {
        blockFilter['content_children']['eq']['content_resolvedModuleParts_module_technology'] = { in: reachabilityFilter.technology };
      }

      if (reachabilityFilter?.referenceType?.length) {
        blockFilter['content_children']['eq']['content_lowerBoundAccessTypes'] = { in: reachabilityFilter.referenceType };
      }
    };
    return blockFilter;
  }

  private createSharedAggregation(blocks: { [key: string]: any }): Aggregation[] {
    const sharedModule = blocks?.content.reduce((acc: any, item: any) =>
      (acc.concat((item.children.aggregations))), []);
    const sharedAggregation: Aggregation[] = [];
    sharedModule?.forEach((sharedItem: Aggregation) => {
      const existingItem = sharedAggregation.find(i => i.groupBy.REFERENCED_MODULE_TYPE === sharedItem.groupBy.REFERENCED_MODULE_TYPE);
      if (existingItem) {
        existingItem.fields.UID.COUNT += sharedItem.fields.UID.COUNT;
      } else {
        sharedAggregation.push(sharedItem);
      }
    });
    return sharedAggregation;
  }

  private createContainsAggregation(blocks: { [key: string]: any }): Aggregation[] {
    const contains = blocks.contains?.aggregations;
    const bounds = [...blocks.upperBound.aggregations];
    if (blocks.lowerBound?.aggregations) {
      bounds.push(...blocks.lowerBound.aggregations as Aggregation[]);
    }
    for (const bound of bounds) {
      const boundIndex = contains.findIndex((contain: Aggregation) =>
        contain.groupBy.REFERENCED_MODULE_TYPE === bound.groupBy.REFERENCED_MODULE_TYPE &&
        contain.groupBy.REFERENCED_MODULE_TECHNOLOGY === bound.groupBy.REFERENCED_MODULE_TECHNOLOGY);
      if (boundIndex !== -1) {
        const countInBounds = bound.fields.UID.COUNT;
        contains[boundIndex].fields.UID.COUNT -= countInBounds;
        if (contains[boundIndex].fields.UID.COUNT <= 0) {
          contains.splice(boundIndex, 1);
        }
      }
    }
    return contains;
  }

  private getModuleIdsFromBlockResolvedModule(resolvedModules: Array<Record<string, string>>): number[] {
    const resolvedModuleId = new Set<number>();
    resolvedModules.forEach((moduleObj: any) => {
      resolvedModuleId.add(moduleObj.module.id as number);
    });
    return [...resolvedModuleId];
  }
}
