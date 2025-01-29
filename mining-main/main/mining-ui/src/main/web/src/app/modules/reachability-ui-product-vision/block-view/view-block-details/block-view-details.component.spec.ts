import { ComponentFixture, TestBed } from '@angular/core/testing';
import { BlockViewDetailsComponent } from './block-view-details.component';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { NzModalService } from 'ng-zorro-antd/modal';
import { NzMessageService } from 'ng-zorro-antd/message';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { FunctionalBlockControllerService } from '@innowake/mining-api-angular-client';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { ReachabilityService } from '../../utils/reachability.service';
import { of } from 'rxjs';
import { ReachabilityBlocks } from '../../utils/reachability-interface';

describe('BlockViewDetailsComponent', () => {
    let component: BlockViewDetailsComponent;
    let fixture: ComponentFixture<BlockViewDetailsComponent>;

    const reachabilityBlocks: ReachabilityBlocks = {
        name: "READACCT Reachability",
        description: "",
        uid: "dhwdhwihiwjdowjd",
        isSelected: false,
        upperBound: [{
            name: "READACCT",
            id: 2002,
            linkHash: "cnskckscjsjc",
            technology: "JCL",
            type: "JOB"
        }]
    }

    const graphQlSpy = jasmine.createSpyObj<GraphQlControllerService>('GraphQlControllerService', ['graphQl']);
    const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
    const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['success', 'error']);
    const labelMappingServiceSpy = jasmine.createSpyObj<LabelMappingService>('LabelMappingService', ['mapLabel']);
    const functionalBlockControllerSpy = jasmine.createSpyObj<FunctionalBlockControllerService>('FunctionalBlockControllerService', ['mergeFunctionalBlock', 'unmergeFunctionalBlock']);
    const reachabilityServiceSpy = jasmine.createSpyObj<ReachabilityService>('ReachabilityService', ['getReachabilityBlockDetails']);

    beforeEach(async () => {
        await TestBed.configureTestingModule({
            declarations: [BlockViewDetailsComponent],
            providers: [
                NumberFormatter,
                TranslateService,
                { provide: GraphQlControllerService, useValue: graphQlSpy },
                { provide: NzModalService, useValue: modalServiceSpy },
                { provide: LabelMappingService, useValue:labelMappingServiceSpy },
                { provide: NzMessageService, useValue: messageServiceSpy },
                { provide: FunctionalBlockControllerService, useValue: functionalBlockControllerSpy },
                { provide: ReachabilityService, useValue: reachabilityServiceSpy }
            ],
            imports: [
                RouterTestingModule,
                HttpClientTestingModule,
                TranslateModule.forRoot({}),
                BrowserAnimationsModule,
                AntDesignImportsModule
            ]
        }).compileComponents();
        reachabilityServiceSpy.getReachabilityBlockDetails.and.returnValue(of(reachabilityBlocks as any));
        graphQlSpy.graphQl.and.returnValue(of(reachabilityBlocks as any));
    });

    beforeEach(() => {
        fixture = TestBed.createComponent(BlockViewDetailsComponent);
        component = fixture.componentInstance;
        component.blockDetails = reachabilityBlocks;
        fixture.detectChanges();
    });

    it('should create the component', () => {
        expect(component).toBeTruthy();
    });

    xit('should test getNonMergedBlocks', () => {
        component.getNonMergedBlocks();
        expect(component.isLoaded).toBeTruthy();
    });

    it('should fetch reachability block details', () => {
        const blockId = JSON.stringify(component.blockDetails.uid);
        component.projectId = 1;
        component.ngOnInit();
        expect(reachabilityServiceSpy.getReachabilityBlockDetails).toHaveBeenCalledWith(component.projectId, blockId);
        expect(component.isLoaded).toBeTrue();
        expect(component.blockData).toEqual(reachabilityBlocks);
        expect(component.isDetailsAvailable).toBeTrue();
    });
});