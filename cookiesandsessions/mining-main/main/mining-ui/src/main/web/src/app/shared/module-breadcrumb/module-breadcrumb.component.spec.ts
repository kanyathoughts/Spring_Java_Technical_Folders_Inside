import { SimpleChanges } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { SelectClientModule } from '@app/modules/select-client/select-client.module';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { of } from 'rxjs';
import { ModuleBreadcrumbComponent } from './module-breadcrumb.component';
import { ModuleControllerService, ModulePojo } from '@innowake/mining-api-angular-client';

describe('ModuleBreadcrumbComponent', () => {
  let component: ModuleBreadcrumbComponent;
  let fixture: ComponentFixture<ModuleBreadcrumbComponent>;
  const testModule: ModulePojo = {
    id: 2,
    name: 'test Module',
    projectId: 2
  }
  const moduleServiceSpy = jasmine.createSpyObj<ModuleControllerService>('ModuleControllerService', ['findModuleById']);

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ModuleBreadcrumbComponent],
      imports: [RouterTestingModule.withRoutes([{ path: 'clients', component: SelectClientModule }]), TranslateModule.forRoot({}),],
      providers: [TranslateService, { provide: ModuleControllerService, useValue: moduleServiceSpy }],
    })
      .compileComponents();
    moduleServiceSpy.findModuleById.and.returnValue(of(testModule as any));
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ModuleBreadcrumbComponent);
    component = fixture.componentInstance;
    component.currentModule = {
      name: 'PRGD',
      id: 2002,
      projectId: 1,
      parent: '2009'
    }
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should check for changes', () => {
    component.subPageTitle = ['test'];
    const changes: SimpleChanges = {
      currentModule: {
        currentValue: {
          id: 2196,
          name: "BEN_PRD_BUD_17",
          projectId: 4
        },
        firstChange: true,
        previousValue: undefined,
        isFirstChange: undefined
      },
      parentModule: {
        currentValue: {
          id: 2730,
          name: "test",
          projectId: 4
        },
        firstChange: true,
        previousValue: undefined,
        isFirstChange: undefined
      },
    }
    component.ngOnChanges(changes);
    expect(component.breadcrumbItems.length).toBeGreaterThan(1);
    expect(component.breadcrumbItems[component.breadcrumbItems.length - 1]).toBe(component.subPageTitle[0]);
    component.breadcrumbItems = ['Module', 'PRG', 'PRGD'];
    component.ngOnChanges(changes);
    expect(component.breadcrumbItems.length).toBeGreaterThan(1);
    expect(component.breadcrumbItems[component.breadcrumbItems.length - 1]).toBe(component.subPageTitle[0]);
    const noChanges: SimpleChanges = {
      currentModule: {
        currentValue: undefined,
        firstChange: true,
        previousValue: undefined,
        isFirstChange: undefined
      },
      parentModule: {
        currentValue: undefined,
        firstChange: true,
        previousValue: undefined,
        isFirstChange: undefined
      },
    }
    component.ngOnChanges(noChanges);
    expect(component.breadcrumbItems.length).toBe(4);
    const changeCase3: SimpleChanges = {
      currentModule: {
        currentValue: {
          id: 2196,
          name: "BEN_PRD_BUD_17",
          projectId: 4
        },
        firstChange: true,
        previousValue: undefined,
        isFirstChange: undefined
      },
      parentModule: {
        currentValue: undefined,
        firstChange: true,
        previousValue: undefined,
        isFirstChange: undefined
      },
    }
    component.breadcrumbItems = ['Module', 'PRG'];
    component.ngOnChanges(changeCase3);
    expect(component.breadcrumbItems.length).toBe(4);
  });

  it('should call else ngOnChanges', () => {
    const changes: SimpleChanges = { };
    component.breadcrumbItems = ['Module', 'PRG'];
    component.ngOnChanges(changes);
    expect(component.breadcrumbItems.length).toBeGreaterThan(1);
  });

  it('should navigate to the selected page', () => {
    spyOn(component, 'navigate').and.callThrough();
    component.navigate('Modules');
    expect(component.navigate).toHaveBeenCalledWith('Modules');
    component.navigate(component.currentModule.name);
    expect(component.navigate).toHaveBeenCalledWith('PRGD');
    component.navigate('test');
    expect(component.navigate).toHaveBeenCalledWith('test');
  });
});
