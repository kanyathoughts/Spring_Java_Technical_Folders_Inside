import { ComponentFixture, TestBed, fakeAsync, flush, tick } from '@angular/core/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { BlockCardComponent } from './block-card.component';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ReachabilityBlocks } from '../../utils/reachability-interface';
import { NzModalService } from 'ng-zorro-antd/modal';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { ModuleControllerService } from '@innowake/mining-api-angular-client';
import { ReachabilityService } from '../../utils/reachability.service';
import { NzDrawerService } from 'ng-zorro-antd/drawer';
import { of } from 'rxjs/internal/observable/of';
import { KeycloakAuthorizationService } from '@app/core/authorization/keycloak-authorization.service';
import { NoAuthorizationService } from '@app/core/authorization/no-authorization.service';

describe('BlockCardComponent', () => {
  let component: BlockCardComponent;
  let fixture: ComponentFixture<BlockCardComponent>;
  const modalServiceSpy = jasmine.createSpyObj<NzModalService>('NzModalService', ['create']);
  let reachabilityServiceSpy: jasmine.SpyObj<ReachabilityService>;
  let drawerServiceSpy: jasmine.SpyObj<NzDrawerService>;

  const labelMappingServiceSpy: jasmine.SpyObj<LabelMappingService> = jasmine.createSpyObj('LabelMappingService', ['mapLabel']);

  const reachabilityBlocks: ReachabilityBlocks = {
    name: "READACCT Reachability",
    description: "",
    uid: "dhwdhwihiwjdowjd",
    isSelected: false,
    type: ["MERGE_PARENT", "RA_TOP_DOWN", "REACHABILITY"],
    upperBound: [{
      name: "READACCT",
      id: 2002,
      linkHash: "cnskckscjsjc",
      technology: "JCL",
      type: "JOB"
    }],
    blockState: {
      errorCount: 0,
      warningsCount: 0,
    }
  };
  
  beforeEach(async () => {
    reachabilityServiceSpy = jasmine.createSpyObj('ReachabilityService', ['getUpdateBlocks', 'setUpdateBlocks', 'storeReachabilityDetails']);
    drawerServiceSpy = jasmine.createSpyObj('NzDrawerService', ['create']);

    await TestBed.configureTestingModule({
      declarations: [ BlockCardComponent ],
      providers: [
        NumberFormatter, 
        TranslateService,
        GraphQlControllerService,
        ModuleControllerService,
        { provide: NzModalService, useValue: modalServiceSpy },
        { provide: LabelMappingService, useValue: labelMappingServiceSpy },
        { provide: ReachabilityService, useValue: reachabilityServiceSpy },
        { provide: NzDrawerService, useValue: drawerServiceSpy },
        { provide: KeycloakAuthorizationService, useValue: new NoAuthorizationService()}
      ],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        AntDesignImportsModule,
        TranslateModule.forRoot({})
    ],
    }).compileComponents();

    drawerServiceSpy.create.and.returnValue({
      afterClose: of(null),
      afterOpen: undefined,
      close: undefined,
      open: undefined,
      getContentComponent: undefined,
      getContentComponentRef: undefined
    });
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockCardComponent);
    component = fixture.componentInstance;
    component.reachabilityBlockData = reachabilityBlocks;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('3-dot button should be visible', () => {
    expect(fixture.nativeElement.querySelector('#reachability-block-actions-button')).toBeTruthy();
  });

  it('should stop event propagation', () => {
    const eventMock = jasmine.createSpyObj<Event>('Event', ['stopPropagation']);
    component.stopBubbling(eventMock);
    expect(eventMock.stopPropagation).toHaveBeenCalled();
  });

  it('should emit true when reachabilityService.getUpdateBlocks() returns true and then call setUpdateBlocks(false)', () => {
    reachabilityServiceSpy.getUpdateBlocks.and.returnValue(true);
    spyOn(component.siderEvent, 'emit');

    component.openSidePanel();

    expect(reachabilityServiceSpy.getUpdateBlocks).toHaveBeenCalled();
    expect(component.siderEvent.emit).toHaveBeenCalledWith(true);
    expect(reachabilityServiceSpy.setUpdateBlocks).toHaveBeenCalledWith(false);
  });

  it('should not do anything when reachabilityService.getUpdateBlocks() returns false', () => {
    reachabilityServiceSpy.getUpdateBlocks.and.returnValue(false);
    spyOn(component.siderEvent, 'emit');

    component.openSidePanel();

    expect(reachabilityServiceSpy.getUpdateBlocks).toHaveBeenCalled();
    expect(component.siderEvent.emit).not.toHaveBeenCalled();
    expect(reachabilityServiceSpy.setUpdateBlocks).not.toHaveBeenCalled();
  });

  it('should stringify reachabilityBlockData.uid correctly', () => {
    component.reachabilityBlockData = { uid: '123' };
    const result = component.stringifyData({ reachabilityIds: component.stringifyData(component.reachabilityBlockData.uid) });
    expect(result).toEqual('{"reachabilityIds":"\\"123\\""}');
  });
});
