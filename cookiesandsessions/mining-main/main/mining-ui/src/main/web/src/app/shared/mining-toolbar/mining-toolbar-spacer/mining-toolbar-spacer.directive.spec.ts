import { MiningToolbarSpacerDirective } from './mining-toolbar-spacer.directive';
import { Component, DebugElement } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { By } from '@angular/platform-browser';

describe('MiningToolbarSpacerDirective', () => {

  let component: TestMiningToolbarSpacerComponent;
  let fixture: ComponentFixture<TestMiningToolbarSpacerComponent>;
  let directiveEl: DebugElement;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [TestMiningToolbarSpacerComponent, MiningToolbarSpacerDirective]
    });
    fixture = TestBed.createComponent(TestMiningToolbarSpacerComponent);
    component = fixture.componentInstance;
    directiveEl = fixture.debugElement.query(By.css('mn-toolbar-spacer'));
  });

  it('should create an instance', () => {
    const directive = new MiningToolbarSpacerDirective();
    expect(directive).toBeTruthy();
  });

  it('should apply size 1 spacer as default', () => {
    fixture.detectChanges();
    expect(directiveEl.nativeElement.classList.contains('mining-toolbar-spacer-1')).toBe(true);
  });

  it('should apply class matching size parameter', () => {
    component.size = 4;
    fixture.detectChanges();
    expect(directiveEl.nativeElement.classList.contains('mining-toolbar-spacer-4')).toBe(true);
  });
});

@Component({
  template: `<mn-toolbar-spacer [size]="size"></mn-toolbar-spacer>`
})
class TestMiningToolbarSpacerComponent {
  size: number;
}
