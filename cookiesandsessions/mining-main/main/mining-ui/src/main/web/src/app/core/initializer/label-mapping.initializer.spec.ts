import { TestBed, waitForAsync } from "@angular/core/testing";
import { LabelMappingService } from "../services/label-mapping.service";
import { initializeLabelMapping } from "./label-mapping.initializer";

describe('LabelMappingInitializer', () => {
  const labelMappingServiceSpy = jasmine.createSpyObj('LabelMappingService', ['init']);
  let labelMappingService: LabelMappingService;

  beforeEach(() => {
      TestBed.configureTestingModule({
          imports: [
              // HttpClientTestingModule,
              // HttpClientModule,
              // RouterTestingModule.withRoutes([])
          ],
          providers: [
              // HttpHandler,
              // ErrorHandlerInterceptor,
              // ApiPrefixInterceptor,
              { provide: LabelMappingService, useValue: labelMappingServiceSpy }
          ]
      });
      labelMappingService = TestBed.inject(LabelMappingService);
  });

  it('should resolve the promise', waitForAsync(() => {
      initializeLabelMapping(labelMappingService)()
          .then(() => { }, (error) => {
              /* If error occurs it should fail the test case so it would mean that the promise was not resolved.
              Hence expecting the error to be undefined to fail the test. */
              expect(error).toBeUndefined();
          });
  }));

})
