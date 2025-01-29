import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateModule } from '@ngx-translate/core';

import { DbSchemaConfigurationComponent } from './db-schema-configuration.component';
import { DataSchemaControllerService } from '@innowake/mining-api-angular-client';

describe('DbSchemaConfigurationComponent', () => {
  let component: DbSchemaConfigurationComponent;
  let fixture: ComponentFixture<DbSchemaConfigurationComponent>;
  const jobManagerServiceSpy: jasmine.SpyObj<JobManagerService> = jasmine.createSpyObj('JobManagerService', ['register']);

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ DbSchemaConfigurationComponent ],
      imports: [
        HttpClientTestingModule,
        TranslateModule.forRoot({}),
      ],
      providers: [DataSchemaControllerService ,   { provide: JobManagerService, useValue: jobManagerServiceSpy },]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(DbSchemaConfigurationComponent);
    component = fixture.componentInstance;
    component.clientProjectData = new ClientProjectRelationship(1, 'client', 1, 'project');
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
