import { GraphOverviewPanelComponent, COLLAPSE_ICON_PATH, EXPAND_ICON_PATH } from './graph-overview-panel.component';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('GraphOverviewPanelComponent', () => {

    let component: GraphOverviewPanelComponent;
    let fixture: ComponentFixture<GraphOverviewPanelComponent>;

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            declarations: [GraphOverviewPanelComponent],
            imports: [
                HttpClientTestingModule
            ]
        }).compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(GraphOverviewPanelComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create the component', () => {
        expect(component).toBeTruthy();
    });

    it('should toggle the component', () => {
        expect(component.showOverview).toBeTruthy();
        expect(component.iconPath).toEqual(COLLAPSE_ICON_PATH);

        component.toggleOverview();

        expect(component.showOverview).toBeFalsy();
        expect(component.iconPath).toEqual(EXPAND_ICON_PATH);

        component.toggleOverview();

        expect(component.showOverview).toBeTruthy();
        expect(component.iconPath).toEqual(COLLAPSE_ICON_PATH);
    });
});
