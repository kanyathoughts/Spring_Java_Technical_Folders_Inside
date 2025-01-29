import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';

import { MessageComponent } from './message.component';
import { NzMessageModule } from 'ng-zorro-antd/message';

describe('MessageComponent', () => {
  let component: MessageComponent;
  let fixture: ComponentFixture<MessageComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [MessageComponent],
      imports: [NzMessageModule]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MessageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
