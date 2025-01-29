import { Component, Input, Inject, OnInit, OnDestroy } from '@angular/core';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { debounceTime } from 'rxjs/internal/operators/debounceTime';
import { distinctUntilChanged } from 'rxjs/internal/operators/distinctUntilChanged';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { WindowToken } from '@app/core/utils/window';
import { Subject, Subscription } from 'rxjs';
import { UniversalSearchControllerService, UniversalSearchResult } from '@innowake/mining-api-angular-client';

const debounceTimer = 300;

@Component({
  selector: 'universal-search',
  templateUrl: './universal-search.component.html'
})

export class UniversalSearchComponent implements OnInit, OnDestroy {
  @Input() projectId: number;
  @Input() currentClient: ClientProjectRelationship;

  hideSelection = true;
  groupedSearchResult: {[category: string]: UniversalSearchResult[]} = {};
  selectedValue: string;
  linkMappingWithPath = { 'code viewer': 'code-viewer', 'module details': 'details/overview' };
  universalSearchText = new Subject<string>();
  stringSubscription: Subscription;
  searchSubscription: Subscription;

  constructor(private universalSearchControllerService: UniversalSearchControllerService,
    @Inject(WindowToken) private $window: Window) {
  }

  ngOnInit(): void {
    this.stringSubscription = this.universalSearchText
      .pipe(
        debounceTime(debounceTimer),
        distinctUntilChanged()
      )
      .subscribe((searchTxt: string) => {
        this.searchSubscription?.unsubscribe();
        return this.universalSearch(searchTxt);
      });
  };

  /**
   * method to navigate based on the item passed
   * @param searchObj: passed item to decide the route
   */
  navigateToDetails(searchObj: UniversalSearchResult): void {
    const ofsetpath =  searchObj['linkType'] === 'code viewer' ? '?offset=' + searchObj.links[0].properties.offset: '' ;
    void openInNewTab(this.projectId, searchObj['moduleId'] as string, this.linkMappingWithPath[searchObj['linkType']] + ofsetpath, this.$window);
  }

  /**
   * get the icon and lable for icon
   * @param searchObj: Object passed to decide the type
   * @returns could be label and icon type.
   */
  getIconLabel(searchObj: UniversalSearchResult): { label: string, icon: string } {
    if (searchObj['linkType'].toLowerCase() === 'code viewer') {
      return { label: 'module.codeViewer', icon: 'mining-icons:opencode_outlined' };
    } else {
      return { label: 'moduleDetails', icon: 'graph-context-menu:icon-OpenModuleDetails' };
    }
  }

  ngOnDestroy(): void {
    this.stringSubscription?.unsubscribe();
  }

  private universalSearch(searchText: string): void {
    this.groupedSearchResult = {};
    if (searchText) {
      this.searchSubscription = this.universalSearchControllerService.searchQueryInProject(this.projectId, searchText)
      .subscribe(
        (searchResult: UniversalSearchResult[]) => {
          this.hideSelection = ! searchResult.length;
          this.toggleClasses('universal-search__search-option', 'universal-search__search-option-data');
          searchResult.forEach(searchObj => {
            if ( ! this.groupedSearchResult[searchObj.type]) {
              this.groupedSearchResult[searchObj.type] = [];
            }
            for (const searchLink of searchObj.links) {
              searchObj['moduleId'] = Number(searchLink.properties.moduleId);
              searchObj['linkType'] = searchLink.type.toLowerCase().replace('_', ' ');
              break;
            }
            const pattern = new RegExp(searchText, 'gi');
            searchObj.title = searchObj?.title?.replace(pattern, (match: string) => `<mark class="universal-search__highlight-text">${match}</mark>`);
            searchObj.subTitle = searchObj?.subTitle?.replace(pattern, (match: string) => `<mark class="universal-search__highlight-text">${match}</mark>`);
            searchObj.context = searchObj?.context?.replace(pattern, (match: string) => `<mark class="universal-search__highlight-text">${match}</mark>`);
            this.groupedSearchResult[searchObj.type].push(searchObj);
          });
        });
    } else {
      this.toggleClasses('universal-search__search-option-data', 'universal-search__search-option');
      this.hideSelection = true;
    }
  }

  private toggleClasses(addingClass: string, removingClass: string): void {
    const getElement = document.getElementsByClassName(addingClass)[0];
    if (getElement) {
      getElement.classList.remove(addingClass);
      getElement.classList.add(removingClass);
    }
  }
}
