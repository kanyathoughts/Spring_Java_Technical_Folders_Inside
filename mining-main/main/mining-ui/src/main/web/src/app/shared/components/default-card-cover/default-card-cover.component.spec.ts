import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { DefaultCardCoverComponent } from './default-card-cover.component';

describe('DefaultCardCoverComponent', () => {
    let component: DefaultCardCoverComponent;
    let fixture: ComponentFixture<DefaultCardCoverComponent>;

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            declarations: [ DefaultCardCoverComponent ]
        })
        .compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(DefaultCardCoverComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
