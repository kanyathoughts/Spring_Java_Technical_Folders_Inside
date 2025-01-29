import { ComponentFixture, TestBed } from '@angular/core/testing';

import { WaitModalComponent } from './wait-modal.component';
import { TranslateModule } from '@ngx-translate/core';

describe('WaitModalComponent', () => {
  let component: WaitModalComponent;
  let fixture: ComponentFixture<WaitModalComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [WaitModalComponent],
      imports: [TranslateModule.forRoot({})]
    })
    .compileComponents();
    
    fixture = TestBed.createComponent(WaitModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
