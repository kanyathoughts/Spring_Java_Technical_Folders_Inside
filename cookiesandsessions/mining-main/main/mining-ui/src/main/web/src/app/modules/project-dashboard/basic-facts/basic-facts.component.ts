import { Component, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { TranslateService } from '@ngx-translate/core';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { dateFormatter } from '@app/core/utils/date-formatter.utils';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { ModuleControllerService, ModuleStatisticsResponse, ModuleTechnologyStatistic } from '@innowake/mining-api-angular-client';
import { LocalShowcaseMode } from '@app/core/utils/local-showcase.util';

@Component({
  selector: 'app-basic-facts',
  templateUrl: './basic-facts.component.html',
})
export class BasicFactsComponent implements OnInit {

  loaderState = LoaderState;
  clientProjectRelationship: ClientProjectRelationship;
  showSkeleton: boolean;
  lineOfCode: number;
  totalModule: number;
  technologyList: ModuleTechnologyStatistic[];
  errorCount: number;
  lastScanDate: Date;
  privHasError: boolean;
  skeletonWidth: string[] = ['40%', '80%'];
  projectId: number;
  routeModules: string;
  labels: { [key: string]: { [key: string]: string } };
  sourceFileCount: number;
  missingFileCount: number;
  hasMissingFiles: boolean;
  showIMSCard = true;
  localShowcaseMode = new LocalShowcaseMode();

  constructor(
    private clientProjectRelationshipService: ClientProjectRelationshipService,
    private translationService: TranslateService,
    private moduleControllerService: ModuleControllerService,
    private numberFormatter: NumberFormatter
  ) {}

  get hasError(): boolean {
    return this.privHasError && (!this.localShowcaseMode.isInLocalShowcaseMode);
  }

  ngOnInit(): void {
    this.showSkeleton = true;
    this.clientProjectRelationshipService.getClientProjectObservable()
    .subscribe((response: ClientProjectRelationship) => {
      if (response) {
        this.clientProjectRelationship = response;
        this.projectId = this.clientProjectRelationship.getProjectId();
        this.fetchDashboardBasicFacts(this.clientProjectRelationship.getLastScanDate());
      }
      this.routeModules = RouteBuilder.buildProjectRoute(this.clientProjectRelationship.getProjectId(), 'modules');
    });
}

  /**
   * Method used to get the details of basic facts such as Modules/LOC/Last scan date/Technologies.
   *
   * @param lastScanDate is the Metrics date.
   */
   private fetchDashboardBasicFacts(lastScanDate: Date | string): void {
    const dataNotAvailable = this.translationService.instant('notAvailable');
    this.moduleControllerService.getModuleStatistics(this.clientProjectRelationship.getProjectId()).subscribe(
      (response: ModuleStatisticsResponse) => {
        this.showSkeleton = false;
        this.totalModule= response.count ? this.numberFormatter.transform(response.count) : dataNotAvailable;
        this.lineOfCode= response.sourceCodeLineCount ? this.numberFormatter.transform(response.sourceCodeLineCount ) : dataNotAvailable;
        this.technologyList = (response.technologies && response.technologies.length > 0) ?
        response.technologies.map(item => item.name).splice(0, 5).join(', ') : dataNotAvailable;
        this.lastScanDate = lastScanDate ? dateFormatter(lastScanDate) : dataNotAvailable;
        this.errorCount = response.withErrorsCount ? this.numberFormatter.transform(response.withErrorsCount) : dataNotAvailable;
        this.privHasError = response.withErrorsCount > 0;
        this.sourceFileCount = response.sourceFileCount ? response.sourceFileCount : dataNotAvailable;
        this.missingFileCount = response.missingCount ? this.numberFormatter.transform(response.missingCount) : dataNotAvailable;
        this.hasMissingFiles = response.missingCount > 0;
  });
  }

}

