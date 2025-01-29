import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';

import { LoaderComponent } from './loader.component';
import { NzSpinModule } from 'ng-zorro-antd/spin';

describe('LoaderComponent', () => {
  let component: LoaderComponent;
  let fixture: ComponentFixture<LoaderComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [LoaderComponent],
      imports: [NzSpinModule]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(LoaderComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
