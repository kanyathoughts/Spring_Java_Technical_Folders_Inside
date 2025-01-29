import { Component, OnDestroy, OnInit } from '@angular/core';
import { DomSanitizer } from '@angular/platform-browser';
import { ActivatedRoute } from '@angular/router';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { Subscription } from 'rxjs';
import { MiningUiExtensionsControllerService, WebUiExtensionDescription } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'app-extension-host',
  templateUrl: './extension-host.component.html'
})
export class ExtensionHostComponent implements OnInit, OnDestroy {

  page: WebUiExtensionDescription;
  pageIdentifier: string;
  clientId: number;
  projectId: number;
  sandboxAttribute: string;
  styles: string;
  classes: string;
  styleSheets: string[] = [];
  srcUrl: string;
  clientProjectSubscription: Subscription;
  constructor(
    private miningUiExtensionService: MiningUiExtensionsControllerService,
    private sanitizer: DomSanitizer,
    private route: ActivatedRoute,
    private clientProjectRelationship: ClientProjectRelationshipService) { }

  ngOnInit(): void {
    this.pageIdentifier = this.route.snapshot.paramMap.get('pageIdentifier');
    this.clientProjectSubscription = this.clientProjectRelationship.getClientProjectObservable().subscribe((response: ClientProjectRelationship) => {
      this.projectId = response.getProjectId();
      this.clientId = response.getClientId();
    });
    this.miningUiExtensionService.getWebUiExtensions().subscribe((response: WebUiExtensionDescription[]) => {
      this.page = response.find((extension: WebUiExtensionDescription) => extension.pageIdentifier === this.pageIdentifier);
      this.setSrcUrl();
      this.sandboxAttribute = this.page.properties?.IFRAME_SANDBOX ||  null;
      this.styles = this.page?.inlineStyles ||  null;
      this.classes = this.page.hostElementStyleNames.length ? this.page.hostElementStyleNames.join(' ') : null;
      this.styleSheets = this.page.styleSheets;
    });
  }

  /**
   * Sanitize the URL.
   *
   * @returns safe URL.
   */
  getSafeUrl(url: string): string {
    return this.sanitizer.bypassSecurityTrustResourceUrl(url) as string;
  }

  /**
   * Unsubscribes all the subscription when component destroys.
   */
  ngOnDestroy(): void {
    this.clientProjectSubscription.unsubscribe();
  }

  private setSrcUrl(): void {
    let url = this.page.properties.IFRAME_SRC.replace('${clientId}', `${this.clientId}`);
    url = url.replace('${projectId}', `${this.projectId}`);
    this.srcUrl =  this.getSafeUrl(url);
  }
}
