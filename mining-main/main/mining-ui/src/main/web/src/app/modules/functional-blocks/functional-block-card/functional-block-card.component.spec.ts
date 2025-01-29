import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FunctionalBlockCardComponent } from './functional-block-card.component';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { TranslateService, TranslateModule } from '@ngx-translate/core';

describe('FunctionalBlockCardComponent', () => {
  let component: FunctionalBlockCardComponent;
  let fixture: ComponentFixture<FunctionalBlockCardComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [TranslateModule.forRoot({})],
      declarations: [FunctionalBlockCardComponent],
      providers: [TranslateService]
    })
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(FunctionalBlockCardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
