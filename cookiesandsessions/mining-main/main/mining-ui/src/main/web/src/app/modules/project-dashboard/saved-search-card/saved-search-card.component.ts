import { Component, Input, OnInit } from '@angular/core';
import { LinkType } from '@app/shared/components/mining-table/mining-table-action.interface';
import { FieldTypeEnum, FilterType, MiningTableConfig, ViewMode } from '@app/shared/components/mining-table/mining-table-config.interface';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { Params } from '@angular/router';
import { Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { TranslateService } from '@ngx-translate/core';
import { SavedSearchCountResponse } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'mn-saved-search',
  templateUrl: './saved-search-card.component.html',
  styleUrls: ['./saved-search-card.component.less']
})
export class SavedSearchCardComponent implements OnInit {
  @Input() savedSearchCountResponse: SavedSearchCountResponse[];
  @Input() projectId: number;

  tableConfig: MiningTableConfig = {
    columnMap: {
      name: {
        field: 'name',
        header: 'name',
        filterProperties: {
          filterType: FilterType.freeText,
        },
        columnAction: {
          type: LinkType.HYPERLINK,
          resolveURL: (data: { id: number, name: string, page: string, count: number }) => this.navigateToSavedSearch(data.page),
          resolveURLParams: (data: { id: number, name: string, page: string, count: number }) => this.fetchURLParamsFromData(data.name)
        },
        fieldType: FieldTypeEnum.STRING,
        displayAs: ViewMode.LINK

      },
      page: {
        field: 'page',
        header: 'page',
        fieldType: FieldTypeEnum.STRING
      },
      count: {
        field: 'count',
        header: 'count',
        fieldType: FieldTypeEnum.NUMBER,
        filterProperties: {
          filterType: FilterType.numberValue
        },
        options: [
          {
            icon: 'info-circle',
            title: this.translateService.instant('upeateEverySixHours'),
            styleClass: 'ant-helper-secondary-text',
          }
        ],
      }
    },
    rows: 10
  };
  value: Array<{ id: number, name: string, page: string, count: number}>;
  loadState: LoaderState;

  constructor(
    private translateService: TranslateService,
  ) { }

  ngOnInit(): void {
    this.tableConfig.projectId = this.projectId;
    this.loadState = LoaderState.loading;
    this.value = this.getTableData(this.savedSearchCountResponse);
    if (this.value.length) {
      this.tableConfig.loading = false;
      this.loadState = LoaderState.success;
    }
  }

  /**
   * Navigates to the module savedSearch.
   * @param data The module data to savedSearch page.
   * @returns link for savedSearch page.
   */
  navigateToSavedSearch(page: string): string {
    return RouteBuilder.buildProjectRoute(this.projectId, page.replace(/\s+/g, '-').toLowerCase());
  }

  mapPageSavedSearch(usage: string): string {
    switch (usage) {
      case Usages.ANNOTATIONTABLE.toString():
        return 'Annotations';
      case Usages.DATADICTIONARYTABLE:
        return 'Data Dictionary';
      default:
        return 'Modules';
    }
  }

  private getTableData(savedSearchResponse: SavedSearchCountResponse[]): Array<{ id: number, name: string, page: string, count: number}> {
    if (!Array.isArray(savedSearchResponse)) {
      return [];
    }
    const data: Array<{ id: number, name: string, page: string, count: number }>
     = savedSearchResponse.map((result: SavedSearchCountResponse, index: number) => ({
      id: index + 1,
      name: result.savedSearch.name,
      page: this.mapPageSavedSearch(result.savedSearch.usage),
      count: result.count
    }));
    data.sort((a, b) => b.count - a.count);
    return data;
  }

  private fetchURLParamsFromData(name: string): Params {
    let queryParams: { [key: string]: string } = {};
    queryParams = {
      savedSearch: name
    };
    return queryParams;
  }
}
