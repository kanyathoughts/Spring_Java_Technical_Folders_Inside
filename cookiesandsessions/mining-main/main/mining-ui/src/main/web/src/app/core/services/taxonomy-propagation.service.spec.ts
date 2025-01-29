import { TestBed } from '@angular/core/testing';

import { TaxonomyPropagationService } from './taxonomy-propagation.service';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { TranslateModule } from '@ngx-translate/core';
import { CustomizableTableColumnService } from './user-customizable-table/customizable-table-column.service';
import { NzModalService } from 'ng-zorro-antd/modal';
import { RouterTestingModule } from '@angular/router/testing';
import { Router } from '@angular/router';
import { JobManagerService } from './job-manager/job-manager.service';
import { JobControllerService } from '@innowake/mining-api-angular-client';

xdescribe('TaxonomyPropagationService', () => {
  let service: TaxonomyPropagationService;
  const jobControllerServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('JobControllerService', [
    'getJobInformation', 'submitJobExtensionV2' , 'submitJobExtension' , 'getJobResult'
  ]);

  const userCustomizableTableServiceSpy: jasmine.SpyObj<CustomizableTableColumnService> = jasmine.createSpyObj('UserCustomizableTableService',
    ['getSelectedDataPoints', 'checkSelectedColumns', 'updateTableConfig', 'setDataPointList', 'handleQueryParameterChange', 'handleFilters', 'getGraphQlParam', 'onPageLoad', 'getQueryParams', 'queryParamsDetails']);

  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);

  const jobManagerServiceSpy: jasmine.SpyObj<JobManagerService> = jasmine.createSpyObj('JobManagerService', ['register']);
  
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        BrowserAnimationsModule,
        RouterTestingModule,
        TranslateModule.forRoot({})
      ],
      providers: [
        { provide: JobControllerService, useValue: jobControllerServiceSpy }, 
        { provide: JobManagerService, useValue: jobManagerServiceSpy },
        {
          provide: CustomizableTableColumnService, useValue: userCustomizableTableServiceSpy
        },
        { provide: NzModalService, useValue: modalServiceSpy }
      ]
    });
    service = TestBed.inject(TaxonomyPropagationService);
  });

  it('should be created', () => {
    const router: Router = TestBed.inject(Router);
    spyOn(router, 'navigate');
    expect(service).toBeTruthy();
  });
});
