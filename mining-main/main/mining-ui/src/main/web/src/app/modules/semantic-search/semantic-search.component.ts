import { Component, OnInit } from '@angular/core';
import { UntypedFormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { minimumCharactersValidator } from '@app/core/utils/validator.utils';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { SemanticSearchControllerService, SemanticSearchDocumentPojo, SemanticSearchResultPojo } from '@innowake/mining-api-angular-client';
import { NzMessageService } from 'ng-zorro-antd/message';

@Component({
  selector: 'app-semantic-search',
  templateUrl: 'semantic-search.component.html'
})
export class SemanticSearchComponent implements OnInit {

  searchForm: UntypedFormGroup;
  resultText: string;
  query: string;
  searchLoading = false;
  documents: SemanticSearchDocumentPojo[];

  private clientProjectRelationship: ClientProjectRelationship;
  private projectId: number;

  constructor(private semanticSearchService: SemanticSearchControllerService,
    private clientProjectRelationshipService: ClientProjectRelationshipService,
    private messageService: NzMessageService,
    private fb: UntypedFormBuilder) { }


  /**
   * Obtains the project ID and sets up the form group.
   */
  ngOnInit(): void {
    this.clientProjectRelationshipService.getClientProjectObservable()
      .subscribe((response: ClientProjectRelationship) => {
        if (response) {
          this.clientProjectRelationship = response;
          this.projectId = this.clientProjectRelationship.getProjectId();
        }
      });
      this.searchForm = this.fb.group({
        query: ['', [Validators.required, minimumCharactersValidator]]
      });
  }

  /**
   * Submits a semantic search request based on the entered search query.
   */
  search(): void {
    if ( ! this.query || this.query.length < 5) {
      return;
    }
    this.resultText = null;
    this.documents = null;
    this.searchLoading = true;
    this.semanticSearchService.searchRag(this.projectId, this.query).subscribe((result: SemanticSearchResultPojo) => {
      this.resultText = result.answer;
      this.documents = result.documents;
      this.searchLoading = false;
    }, error => {
      this.searchLoading = false;
      this.messageService.error(error.error.message as string);
    });
  }

  /**
   * Creates code-viewer link for a certain document.
   * @param document the document to create the link for
   * @returns the link
   */
  createLink(document: SemanticSearchDocumentPojo): string {
    let offset = '';
    if (document?.meta['offset']) {
      offset = '?offset=' + document?.meta['offset'];
    }
    return '#' + RouteBuilder.buildModuleRoute(this.projectId, document.meta['moduleId'], 'code-viewer' + offset);;
  }

}
