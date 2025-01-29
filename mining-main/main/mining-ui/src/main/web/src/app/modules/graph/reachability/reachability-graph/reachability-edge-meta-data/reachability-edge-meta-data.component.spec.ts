import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReachabilityEdgeMetaDataComponent } from './reachability-edge-meta-data.component';

describe('ReachabilityEdgeMetaDataComponent', () => {
  let component: ReachabilityEdgeMetaDataComponent;
  let fixture: ComponentFixture<ReachabilityEdgeMetaDataComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ReachabilityEdgeMetaDataComponent]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ReachabilityEdgeMetaDataComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should emit close event when closePanel is called', () => {
    spyOn(component.closeReachabilitySidePanel, 'emit');
    component.closePanel();
    expect(component.closeReachabilitySidePanel.emit).toHaveBeenCalled();
  });
});