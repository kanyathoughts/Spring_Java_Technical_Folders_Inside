import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';

import FileSaveSupport from '@app/core/utils/file-save-support.utils';
import { GraphComponent } from 'yfiles';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { NzMessageService } from 'ng-zorro-antd/message';
import { MiningGraphsExportComponent } from './mining-graphs-export.component';
import { NzModalModule } from 'ng-zorro-antd/modal';

describe('GraphExportComponent', () => {
  let component: MiningGraphsExportComponent;
  let fixture: ComponentFixture<MiningGraphsExportComponent>;
  let translateService: TranslateService;
  const messageServiceSpy = jasmine.createSpyObj<NzMessageService>('NzMessageService', ['create']);

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [MiningGraphsExportComponent,
      ],
      providers: [FileSaveSupport,
        TranslateService,
        { provide: NzMessageService, useValue: messageServiceSpy },
      ],
      imports: [
        TranslateModule.forRoot({}),
        NzDropDownModule, NzModalModule
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MiningGraphsExportComponent);
    translateService = TestBed.inject(TranslateService);
    component = fixture.componentInstance;
    component.moduleName = "Test_Module_Name";
    component.graphComponent = new GraphComponent();
    messageServiceSpy.create.and.returnValue(null);
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should save graph in SVG format', (done) => {
    spyOn(FileSaveSupport, 'save');
    component.download("SVG");
    expect(FileSaveSupport.save).toHaveBeenCalledWith(jasmine.anything(), 'Test_Module_Name.svg');
    done();
  });

  it('should display error while saving in SVG format', (done) => {
    spyOn(FileSaveSupport, 'save');
    component.isGraphEmpty = true;
    component.download("SVG");
    expect(messageServiceSpy.create).toHaveBeenCalledWith(MiningGraphsExportComponent.SEVERITY_WARN, translateService.instant('controlFlowGraph.emptyGraphExportError'));
    done();
  });

  it('should save graph in PNG format', (done) => {
    spyOn(FileSaveSupport, 'save').and.returnValue(null);
    component.download("PNG");
    setTimeout(() => {
      expect(FileSaveSupport.save).toHaveBeenCalledWith(jasmine.anything(), 'Test_Module_Name.png');
      done();
    }, 600);
  });

  it('should  display error while saving in PNG format', (done) => {
    spyOn(FileSaveSupport, 'save').and.returnValue(null);
    component.isGraphEmpty = true;
    component.download("PNG");
    expect(messageServiceSpy.create).toHaveBeenCalledWith(MiningGraphsExportComponent.SEVERITY_WARN, translateService.instant('controlFlowGraph.emptyGraphExportError'));
    done();
  });

  it('should save graph in PDF format', (done) => {
    spyOn(FileSaveSupport, 'save').and.returnValue(null);
    component.download("PDF");
    setTimeout(() => {
      expect(FileSaveSupport.save).toHaveBeenCalledWith(jasmine.anything(), 'Test_Module_Name.pdf');
      done();
    }, 600);
  });

  it('should  display error while saving in PDF format', (done) => {
    spyOn(FileSaveSupport, 'save').and.returnValue(null);
    component.isGraphEmpty = true;
    component.download("PDF");
    expect(messageServiceSpy.create).toHaveBeenCalledWith(MiningGraphsExportComponent.SEVERITY_WARN, translateService.instant('controlFlowGraph.emptyGraphExportError'));
    done();
  });
});
