import { ChangeDetectorRef, Component, DebugElement, TemplateRef } from '@angular/core';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MiningToolbarButtonDirective } from './mining-toolbar-button.directive';

describe('MiningToolBarButtonDirective', () => {  
    let component: TestMiningToolbarButtonComponent;
    let fixture: ComponentFixture<TestMiningToolbarButtonComponent>;  

    beforeEach(() => {
        TestBed.configureTestingModule({
            declarations: [TestMiningToolbarButtonComponent, MiningToolbarButtonDirective],
            providers: [TemplateRef]
          });
          fixture = TestBed.createComponent(TestMiningToolbarButtonComponent);
          component = fixture.componentInstance;
    });

    it('should create an instance', () => {
        let templateRef: TemplateRef<unknown>;
        const directive = new MiningToolbarButtonDirective(templateRef);
        expect(directive).toBeTruthy();
    });

});

@Component({
    template: `<mn-toolbar-button-dir></mn-toolbar-button-dir>`
  })
  class TestMiningToolbarButtonComponent {
    alwaysHidden = false;
  }