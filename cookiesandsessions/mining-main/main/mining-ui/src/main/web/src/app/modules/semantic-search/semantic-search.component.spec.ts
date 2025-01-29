import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SemanticSearchComponent } from './semantic-search.component';
import { TranslateModule } from '@ngx-translate/core';
import { AntDesignImportsModule } from '@app/shared/ant-design-imports.module';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { SemanticSearchControllerService } from '@innowake/mining-api-angular-client';
import { NzMessageService } from 'ng-zorro-antd/message';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { HttpClient } from '@angular/common/http';
import { UntypedFormBuilder } from '@angular/forms';

describe('SemanticSearchComponent', () => {
  let component: SemanticSearchComponent;
  let fixture: ComponentFixture<SemanticSearchComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [TranslateModule.forRoot({}), AntDesignImportsModule, HttpClientTestingModule],
      declarations: [ SemanticSearchComponent ],
      providers: [SemanticSearchControllerService, ClientProjectRelationshipService, NzMessageService, HttpClient, UntypedFormBuilder]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SemanticSearchComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should display a span if document.meta.moduleId is undefined', () => {
    component.documents = [{ meta: { name: 'Test Document' } }];
    fixture.detectChanges();
    const compiled = fixture.debugElement.nativeElement;
    expect(compiled.querySelector('.semantic-search__title span')).toBeTruthy();
    expect(compiled.querySelector('.semantic-search__title a')).toBeFalsy();
  });
  
  it('should display a link if document.meta.moduleId is defined', () => {
    component.documents = [{ meta: { name: 'Test Document', moduleId: '1' } }];
    fixture.detectChanges();
    const compiled = fixture.debugElement.nativeElement;
    expect(compiled.querySelector('.semantic-search__title span')).toBeFalsy();
    expect(compiled.querySelector('.semantic-search__title a')).toBeTruthy();
  });
});
