import { TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';

import { ScrollEventService } from './scroll-event.service';

describe('ScrollEventService', () => {
  let service: ScrollEventService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule
      ]
    });
    service = TestBed.inject(ScrollEventService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
