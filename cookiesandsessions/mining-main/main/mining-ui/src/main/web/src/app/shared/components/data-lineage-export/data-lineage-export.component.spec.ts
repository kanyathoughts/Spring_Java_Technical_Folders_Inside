import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { SharedModule } from '@app/shared/shared.module';
import { TranslateModule } from '@ngx-translate/core';
import { NzModalModule, NzModalRef } from 'ng-zorro-antd/modal';
import { DataLineageExportComponent } from './data-lineage-export.component';
import { Observable, of } from 'rxjs';
import { JobControllerService, JobInformation } from '@innowake/mining-api-angular-client';


describe('DataLineageExportComponent', () => {
    let component: DataLineageExportComponent;
    let fixture: ComponentFixture<DataLineageExportComponent>;
    const nzModalRefSpy = jasmine.createSpyObj<NzModalRef>('NzModalRef', ['close', 'updateConfig']);
    const jobControllerServiceSpy: jasmine.SpyObj<JobControllerService> = jasmine.createSpyObj('JobControllerService', [
        'getJobInformation',
        'submitJobExtension'
      ]);
    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
          declarations: [],
          imports: [SharedModule,
            RouterTestingModule,
            BrowserAnimationsModule,
            TranslateModule.forRoot({}),
            HttpClientTestingModule, 
            NzModalModule],
            providers: [{ provide: NzModalRef, useValue: nzModalRefSpy },
                { provide: JobControllerService, useValue: jobControllerServiceSpy }

            ]
        }).compileComponents();
        jobControllerServiceSpy.submitJobExtension.and.returnValue(of('some-job-id') as Observable<any>);
        jobControllerServiceSpy.getJobInformation.and.returnValue(of('some-job-id') as Observable<any>);

    }));
    beforeEach(() => {
        fixture = TestBed.createComponent(DataLineageExportComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });
    afterEach(() => {
        fixture.destroy();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should test onDataLineageChange', () => {
        component.dataLineageForm.controls.dataLineageSelection.setValue('Module Detail Level');
        component.onDataLineageChange();
        expect(component.selectedDataLineageLevel).toBe('Module Detail Level');
    });

    it('should test onDataLineageChange when data lineage level one is selected', () => {
        component.dataLineageForm.controls.dataLineageSelection.setValue('dataLineage.fieldLevel');
        component.onDataLineageChange();
        expect(component.selectedDataLineageLevel).toBe('dataLineage.fieldLevel');
    });

    it('should test onDataLineageChange when data lineage level two is selected', () => {
        component.dataLineageForm.controls.dataLineageSelection.setValue('dataLineage.statementLevel');
        component.onDataLineageChange();
        expect(component.selectedDataLineageLevel).toBe('dataLineage.statementLevel');
    });

    it('should test exportDataLineage when job is success', () => {
        component.moduleId = '123';
        component.exportDataLineage();
        expect(component.inputData['moduleId']).toEqual(['123']);
        expect(component.isDataLineageExporting).toBeTrue();
    });

    it('should test exportDataLineage when job failure', () => {
        component.moduleId = '123';
        jobControllerServiceSpy.getJobInformation.and.returnValue(of(JobInformation.StatusEnum.FAILURE) as Observable<any>);
        component.exportDataLineage();
        expect(component.inputData['moduleId']).toEqual(['123']);
    });
})