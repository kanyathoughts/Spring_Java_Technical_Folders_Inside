import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';

import { MiningToolbarComponent } from './mining-toolbar.component';
import { DebugElement } from '@angular/core';
import { By } from '@angular/platform-browser';

describe('MiningToolbarComponent', () => {
  let component: MiningToolbarComponent;
  let fixture: ComponentFixture<MiningToolbarComponent>;
  let componentEl: DebugElement;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ MiningToolbarComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MiningToolbarComponent);
    component = fixture.componentInstance;
    componentEl = fixture.debugElement.query(By.css('nav'));
  });

  it('should create', () => {
    fixture.detectChanges();
    expect(componentEl.nativeElement.classList.contains('mining-toolbar')).toBe(true);
    expect(componentEl.nativeElement.style.justifyContent).toBe('flex-start');
    expect(component).toBeTruthy();
  });

  it('Should add class for Eclipse view', () => {
    component.eclipseView = true;
    fixture.detectChanges();
    expect(componentEl.nativeElement.classList.contains('mining-toolbar--eclipse-view')).toBe(true);
  });

  it('Should add class for sticky toolbar', () => {
    component.stickyToolbar = true;
    fixture.detectChanges();
    expect(componentEl.nativeElement.classList.contains('mining-toolbar--sticky')).toBe(true);
  });

  it('Should add style property according to property', () => {
    component.justifyContent = 'space-between';
    fixture.detectChanges();
    expect(componentEl.nativeElement.style.justifyContent).toBe('space-between');
  });

  it('Should not add style property according to unexpected value', () => {
    component.justifyContent = 'space-around';
    fixture.detectChanges();
    expect(componentEl.nativeElement.style.justifyContent).toBe('flex-start');
  });
});
