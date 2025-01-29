import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SchedulerDataConfigurationComponent } from './scheduler-data-configuration.component';
import { of } from 'rxjs';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { SchedulerImporterControllerService } from '@innowake/mining-api-angular-client';

xdescribe('SchedulerDataConfigurationComponent', () => {
  let component: SchedulerDataConfigurationComponent;
  let fixture: ComponentFixture<SchedulerDataConfigurationComponent>;
  const graphQlSpy = jasmine.createSpyObj<GraphQlControllerService>('GraphQlControllerService', ['graphQl']);
  const schedulerImporterControllerServiceSpy = jasmine.createSpyObj('SchedulerImporterControllerService', ['getSupportedImporters']);

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [SchedulerDataConfigurationComponent],
      providers: [
        { provide: GraphQlControllerService, useValue: graphQlSpy },
        { provide: SchedulerImporterControllerService, useValue: schedulerImporterControllerServiceSpy }
      ]
    })
    .compileComponents();
    fixture = TestBed.createComponent(SchedulerDataConfigurationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize component', () => {
    const projectId = 123;
    const fileList: any = [];
    const optionList = ['option1', 'option2'];
    const response = {
      data: {
        schedulerImports: {
          content: [
            {
              identifier: 'identifier1',
              description: 'description1',
              schedulerType: 'type1'
            },
            {
              identifier: 'identifier2',
              description: 'description2',
              schedulerType: 'type2'
            }
          ]
        }
      }
    };
  
    schedulerImporterControllerServiceSpy.getSupportedImporters.and.returnValue(of({ optionlist: optionList }));
    graphQlSpy.graphQl.and.returnValue(of(response as any));

    component.ngOnInit();
    expect(component.projectId).toBe(projectId);
    expect(component.fileList).toEqual(fileList);
    expect(component.tableConfig.isExportVisible).toBe(false);
    expect(component.tableConfig.isImportVisible).toBe(true);
    expect(component.tableConfig.importToolTipText).toBe('schedulerDataComponent.importFile');
    expect(component.isSchedulerDataPresent).toBe(true);
  });
});