import { TestBed, ComponentFixture, waitForAsync } from '@angular/core/testing';

import { MetricsFilterService } from './metrics-filter.service';
import { Component } from '@angular/core';
const taxonomyDetails = [{taxonomyId: 1, taxonomyTitle: 'Test'}]
@Component({
  template: ''
})
class TestComponent {
  constructor(private metricsFilterService: MetricsFilterService) {
    this.metricsFilterService.setMetricsTaxonomyFilter(taxonomyDetails);
  }

  getmetricsTaxonomyFilter() {
    return this.metricsFilterService.getMetricsTaxonomyFilter();
  }
}

describe('MetricsFilterService', () => {
  let fixture: ComponentFixture<TestComponent>;
  let component: TestComponent;
  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [TestComponent],
      providers: [TestComponent]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TestComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    const service: MetricsFilterService = TestBed.inject(MetricsFilterService);
    expect(service).toBeTruthy();
    service.setMetricsTaxonomyFilter(taxonomyDetails);
    service.getMetricsTaxonomyFilter();
    expect(component).toBeTruthy();
  });
});
