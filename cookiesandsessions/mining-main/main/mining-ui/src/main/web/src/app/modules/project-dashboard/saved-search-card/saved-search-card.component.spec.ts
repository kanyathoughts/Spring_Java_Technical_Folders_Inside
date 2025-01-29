
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { By } from '@angular/platform-browser';
import { SavedSearchCardComponent } from './saved-search-card.component';
import { SharedModule } from '@app/shared';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NzCollapseModule } from 'ng-zorro-antd/collapse';
import { RouterTestingModule } from '@angular/router/testing';
import { I18nService } from '@app/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { SavedSearchCountResponse } from '@innowake/mining-api-angular-client';

describe('SavedSearchCardComponent', () => {
  let component: SavedSearchCardComponent;
  let fixture: ComponentFixture<SavedSearchCardComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [SavedSearchCardComponent],
      imports: [SharedModule, AntDesignImportsModule, BrowserAnimationsModule, NzCollapseModule, TranslateModule.forRoot({}), RouterTestingModule],
      providers: [
        TranslateService,
        NumberFormatter,
        { provide: I18nService, useValue: I18nService },
      ]
  

    }).compileComponents();

  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SavedSearchCardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();

  });

  afterEach(() => {
    fixture.destroy();
  });

  it('should create with charts', () => {
    expect(component).toBeTruthy();
  });

  it('should have link to proper page', () => {
    component.projectId = 1;
    const savedSearchCount: Array<SavedSearchCountResponse> = [
      {
        savedSearch: {
          id: 1,
          name: "Business Rule Candidates",
          usage: "miningUi.annotationsTable",
        },
        count: 817

      }
    ]
    component.savedSearchCountResponse = savedSearchCount;
    component.ngOnInit();
    fixture.detectChanges();
    const tableRows = fixture.debugElement.queryAll(By.css('.ant-table-row'));
    expect(tableRows.length).toBe(component.savedSearchCountResponse.length + 1);
    for (let i = 0; i < component.savedSearchCountResponse.length; i++) {
      const row = tableRows[i + 1].queryAll(By.css('.ant-table-cell'));
      expect(row[0].nativeElement.textContent).toContain(component.savedSearchCountResponse[i].savedSearch.name);
      expect(row[1].nativeElement.textContent).toContain(component.mapPageSavedSearch(component.savedSearchCountResponse[i].savedSearch.usage));
      expect(row[2].nativeElement.textContent).toContain(component.savedSearchCountResponse[i].count);
    }
    const link = fixture.debugElement.query(By.css('a')).nativeElement;
    expect(link.href).toContain("project-1/annotations?savedSearch=Business%20Rule%20Candidates");
  });
});
