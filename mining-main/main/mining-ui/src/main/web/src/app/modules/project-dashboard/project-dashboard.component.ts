import { Component, OnDestroy, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { FieldTypeEnum, FilterType, ViewMode } from '@app/shared/components/mining-table/mining-table-config.interface';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { HotSpotConfig } from './hotspot-card/hotspot-card.component';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { Subscription } from 'rxjs';
import { taxonomyReduceList } from '@app/core/utils/taxonomy.utils';
import { map } from 'rxjs/operators';
import { LinkType } from '@app/shared/components/mining-table/mining-table-action.interface';
import { LabelMappingService } from '@app/core/services/label-mapping.service';
import { LabelType } from '@app/core/utils/mapping.utils';
import {
  ModulePojo,
  SavedSearchControllerService,
  SavedSearchCountResponse,
  TaxonomyControllerService,
  TaxonomyPojo
} from '@innowake/mining-api-angular-client';

export const colourCodes = [
  '#66cc00',
  '#3399ff',
  '#483D8B',
  '#ff3333',
  '#006400',
  '#9400D3',
  '#9932CC',
  '#CD5C5C',
  '#B8860B',
  '#FF6347',
  '#2E8B57',
  '#3CB371',
  '#20B2AA',
  '#7CFC00',
  '#FF0000',
  '#B03060',
  '#9370DB',
  '#27408B',
  '#00008B',
  '#00C5CD',
  '#008B8B',
  '#458B74',
  '#00FF00',
  '#CD5555',
  '#8B4726'
];

@Component({
  selector: 'mn-project-dashboard',
  templateUrl: './project-dashboard.component.html'
})
export class ProjectDashboardComponent implements OnInit, OnDestroy {
  loaderState = LoaderState;
  clientProjectRelationship: ClientProjectRelationship;
  hotspotConfigs: HotSpotConfig[];
  skeletonWidth: string[] = ['40%', '80%'];
  projectId: number;
  labels: { [key: string]: { [key: string]: string } };
  taxonomies: Array<{ category: string, taxonomies: Array<{ name: string }> }> = [];
  showSavedSearchCard = true;
  savedSearchs: SavedSearchCountResponse[] =[];

  showIMSCard = true;
  private clientProjectSubscription: Subscription;

  constructor(
    private clientProjectRelationshipService: ClientProjectRelationshipService,
    private taxonomyControllerService: TaxonomyControllerService,
    private labelMappingService: LabelMappingService,
    private savedSearchControllerService: SavedSearchControllerService
  ) { }

  ngOnInit(): void {
    this.clientProjectSubscription = this.clientProjectRelationshipService.getClientProjectObservable()
      .subscribe((response: ClientProjectRelationship) => {
        if (response) {
          this.clientProjectRelationship = response;
          this.projectId = this.clientProjectRelationship.getProjectId();
          this.hotspotConfigs = this.initializeHotspotConfigs();
        }
      });
    this.fetchTaxonomiesData(this.projectId);
    this.fetchSavedSearchdata(this.projectId);
  }

  initializeHotspotConfigs(): HotSpotConfig[] {
    const rows = 10;

    return [
      new HotSpotConfig('hotspots.mostReferencedPrograms', 'REFERENCES', rows, {
        columnMap: {
          moduleName: {
            field: 'moduleName',
            header: 'moduleName',
            filterProperties: {
              filterType: FilterType.freeText
            },
            columnAction: { type: LinkType.HYPERLINK, resolveURL: (data: ModulePojo) => this.navigateToDetails(data) },
            fieldType: FieldTypeEnum.STRING,
            displayAs: ViewMode.LINK
          },
          language: { field: 'language', header: 'technology', filterProperties: { filterType: FilterType.multiSelect }, options: [] as undefined,
            getLabel: (value: string) => this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, value)
          },
          value: { field: 'count', header: 'count', filterProperties: { filterType: FilterType.numberValue }, fieldType: FieldTypeEnum.NUMBER }
        },
        rows
      }),
      new HotSpotConfig('hotspots.mostFDs', 'CALLS', rows, {
        columnMap: {
          moduleName: {
            field: 'moduleName',
            header: 'moduleName',
            filterProperties: {
              filterType: FilterType.freeText,
            },
            columnAction: { type: LinkType.HYPERLINK, resolveURL: (data: ModulePojo) => this.navigateToDetails(data) },
            fieldType: FieldTypeEnum.STRING,
            displayAs: ViewMode.LINK
          },
          language: { field: 'language', header: 'technology', filterProperties: { filterType: FilterType.multiSelect }, options: [] as undefined,
            getLabel: (value: string) => this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, value)
          },
          type: { field: 'type', header: 'type', filterProperties: { filterType: FilterType.multiSelect }, options: [] as undefined,
            getLabel: (value: string) => this.labelMappingService.mapLabel(LabelType.TYPE, value)
          },
          value: { field: 'count', header: 'count', filterProperties: { filterType: FilterType.numberValue }, fieldType: FieldTypeEnum.NUMBER }
        },
        rows
      }),
      new HotSpotConfig('hotspots.mostDataSets', 'DATA_SETS', rows, {
        columnMap: {
          dataSetName: {
            field: 'moduleName',
            header: 'dataSetName',
            filterProperties: {
              filterType: FilterType.freeText,
            },
            columnAction: { type: LinkType.HYPERLINK, resolveURL: (data: ModulePojo) => this.navigateToDetails(data) },
            fieldType: FieldTypeEnum.STRING,
            displayAs: ViewMode.LINK
          },
          rwcount: { field: 'count', header: 'readWriteCount', filterProperties: { filterType: FilterType.numberValue }, fieldType: FieldTypeEnum.NUMBER }
        },
        rows
      }),
      new HotSpotConfig('hotspots.mostCandidateBusinessRules', 'CANDIDATE_RULE', rows, {
        columnMap: {
          moduleName: {
            field: 'moduleName',
            header: 'moduleName',
            filterProperties: {
              filterType: FilterType.freeText
            },
            columnAction: { type: LinkType.HYPERLINK, resolveURL: (data: ModulePojo) => this.navigateToDetails(data) },
            fieldType: FieldTypeEnum.STRING,
            displayAs: ViewMode.LINK
          },
          language: { field: 'language', header: 'technology', filterProperties: { filterType: FilterType.multiSelect }, options: [] as undefined,
            getLabel: (value: string) => this.labelMappingService.mapLabel(LabelType.TECHNOLOGY, value)
          },
          type: { field: 'type', header: 'type', filterProperties: { filterType: FilterType.multiSelect }, options: [] as undefined,
            getLabel: (value: string) => this.labelMappingService.mapLabel(LabelType.TYPE, value)
          },
          value: { field: 'count', header: 'count', filterProperties: { filterType: FilterType.numberValue }, fieldType: FieldTypeEnum.NUMBER }
        },
        rows
      }),
      new HotSpotConfig('hotspots.mostDbTables', 'DATABASE_TABLES', rows, {
        columnMap: {
          databaseName: {
            field: 'moduleName',
            header: 'databaseName',
            filterProperties: {
              filterType: FilterType.freeText
            },
            columnAction: { type: LinkType.HYPERLINK, resolveURL: (data: ModulePojo) => this.navigateToDetails(data) },
            fieldType: FieldTypeEnum.STRING,
            displayAs: ViewMode.LINK
          },
          rwcount: { field: 'count', header: 'readWriteCount', filterProperties: { filterType: FilterType.numberValue }, fieldType: FieldTypeEnum.NUMBER }
        },
        rows
      })
    ];
  }

  /**
   * Navigates to the module detail page.
   *
   * @param module The module data to navigate to detail page.
   * @returns link for detail page.
   */
  navigateToDetails(module: ModulePojo): string {
    return RouteBuilder.buildModuleRoute(this.clientProjectRelationship.getProjectId(), module.linkHash ? module.linkHash: module.id, 'details/overview');
  }

  /**
   * Show/hides the IMS card by setting the flag for the same.
   * @param $event used for the deciding the current visibility of the card.
   */
  hideImsCard($event: boolean): void {
    this.showIMSCard = $event;
  }

  hideSavedSearchCard($event: boolean): void{
    this.showSavedSearchCard= $event;
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
  }

  private fetchTaxonomiesData(projectId: number): void {
    this.taxonomyControllerService.findAllTaxonomies(projectId).pipe(
      map((resp: TaxonomyPojo[]) => resp.filter((taxonomy: TaxonomyPojo) => taxonomy.taxonomyReferenceCount > 0))
    ).subscribe((response: TaxonomyPojo[]) => {
      if (response && response.length) {
          const groupedTaxonomy = response.reduce((newList: TaxonomyPojo, currentValue: TaxonomyPojo) => taxonomyReduceList(newList, currentValue), {});
          Object.keys(groupedTaxonomy).forEach((category: string) => {
            const parent: { category: string, taxonomies: Array<{ name: string }> } = { category, taxonomies: [] };
            Object.keys(groupedTaxonomy[category] as object).forEach((type: string) => {
              parent.taxonomies.push({ name: type });
            });
            parent.taxonomies.sort((a: any, b: any) => a.name.toLowerCase().localeCompare(b.name.toLowerCase()));
            this.taxonomies.push(parent);
          });
          this.taxonomies.sort((a: any, b: any) => a.category.toLowerCase().localeCompare(b.category.toLowerCase()));
      }
    });
  }

  private fetchSavedSearchdata(projectId: number): void {
    this.savedSearchControllerService.getDashboardSavedSearchCounts(projectId).subscribe(
      (results: SavedSearchCountResponse[]) => {
        this.savedSearchs = results;
      }
    );
  }
}
