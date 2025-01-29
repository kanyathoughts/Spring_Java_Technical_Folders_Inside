import { MiningToolbarGroupDirective } from './mining-toolbar-group.directive';
import { Component, DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';

describe('MiningToolbarGroupDirective', () => {

  let component: TestMiningToolbarGroupComponent;
  let fixture: ComponentFixture<TestMiningToolbarGroupComponent>;
  let directiveEl: DebugElement;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [TestMiningToolbarGroupComponent, MiningToolbarGroupDirective]
    });
    fixture = TestBed.createComponent(TestMiningToolbarGroupComponent);
    component = fixture.componentInstance;
    directiveEl = fixture.debugElement.query(By.css('mn-toolbar-group'));
  });

  it('should create an instance', () => {
    const directive = new MiningToolbarGroupDirective();
    expect(directive).toBeTruthy();
  });

  it('should apply class', () => {
    fixture.detectChanges();
    expect(directiveEl.nativeElement.classList.contains('mining-toolbar-group')).toBe(true);
  });

  it('should apply css attribute when valid justifyContent input', () => {
    component.justifyContent = 'center';
    fixture.detectChanges();
    expect(directiveEl.nativeElement.style.justifyContent).toBe('center');
  });

  it('should not apply css attribute when unvalid justifyContent input', () => {
    component.justifyContent = 'space-around';
    fixture.detectChanges();
    expect(directiveEl.nativeElement.style.justifyContent).toBe('');
  });
});

@Component({
  template: `<mn-toolbar-group [justifyContent]="justifyContent"></mn-toolbar-group>`
})
class TestMiningToolbarGroupComponent {
  justifyContent: string;
}
