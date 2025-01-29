import { TestBed } from '@angular/core/testing';
import { FunctionalGroupService } from './functional-group.service';
import { GraphQlControllerService } from './graphql.service';
import { of } from 'rxjs';

describe('FunctionalGroupService', () => {
  let service: FunctionalGroupService;
  let graphQlControllerService: jasmine.SpyObj<GraphQlControllerService>;

  beforeEach(() => {
    const spy = jasmine.createSpyObj('GraphQlControllerService', ['graphQl']);

    TestBed.configureTestingModule({
      providers: [
        FunctionalGroupService,
        { provide: GraphQlControllerService, useValue: spy }
      ]
    });

    service = TestBed.inject(FunctionalGroupService);
    graphQlControllerService = TestBed.inject(GraphQlControllerService) as jasmine.SpyObj<GraphQlControllerService>;
  });

  
  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should get functional group tree', (done: DoneFn) => {
    const response = {
      data: {
        functionalBlocks: {
          totalElements: 1,
          content: [
            {
              uid: '1',
              name: 'Functional Group 1',
              type: 'type1',
              flags: { TYPE: ['FUNCTIONAL_GROUP'] },
              generatedFrom: {
                annotationId: '1'
              },
              childrenDeep: {
                content: [
                  {
                    name: 'Functional Unit 1',
                    uid: '2',
                    flags: { TYPE: ['FUNCTIONAL_UNIT'] },
                    generatedFrom: {
                      annotationId: '2'
                    },
                    type: 'type2',
                    children: {
                      content: [
                        {
                          uid: '3',
                          name: 'Functional Unit 2',
                          type: 'type3',
                          flags: { TYPE: ['FUNCTIONAL_UNIT'] },
                          generatedFrom: {
                            annotationId: '3'
                          }
                        }
                      ]
                    }
                  }
                ]
              }
            }
          ]
        }
      }
    };

    const filterDetails = {
      taxonomyIds: ['taxonomy_1,2,3'],
      moduleIds: ['1', '2', '3'],
      reachabilityIds: ['1', '2', '3']
    };

    graphQlControllerService.graphQl.and.returnValue(of(response) as any);
    service.getFunctionalGroupTree(1, filterDetails, 1, 30).subscribe(data => {
      expect(data).toBeTruthy();
      expect(graphQlControllerService.graphQl).toHaveBeenCalled();
      done();
    });
  });
});