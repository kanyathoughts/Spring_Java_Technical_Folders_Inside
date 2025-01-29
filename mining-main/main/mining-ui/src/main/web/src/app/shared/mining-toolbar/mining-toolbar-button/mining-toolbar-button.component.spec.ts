import { ChangeDetectorRef, Component, DebugElement } from '@angular/core';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MiningToolbarButtonComponent } from './mining-toolbar-button.component';

describe('MiningToolbarButtonComponent', () => {
    let component: MiningToolbarButtonComponent;
    let fixture: ComponentFixture<MiningToolbarButtonComponent>;

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
          declarations: [ MiningToolbarButtonComponent ],
          providers: [ ChangeDetectorRef ]
        })
        .compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(MiningToolbarButtonComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should test onResize', () => {
        component.onResize();
        spyOn((component as any), 'onWindowExpand').and.callThrough();
        expect(component.screenWidth).toEqual(window.innerWidth);
    });
})