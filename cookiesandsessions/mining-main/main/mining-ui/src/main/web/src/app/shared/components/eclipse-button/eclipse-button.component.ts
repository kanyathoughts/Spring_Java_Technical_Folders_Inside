import { Component, OnInit, OnDestroy, Output, EventEmitter, Input } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { DeepLinkService } from '@app/core/services/deep-link.service';

const pathIconOnline = 'eclipse-icons:eclipse-online';
const pathIconOffline = 'eclipse-icons:eclipse-offline';

@Component({
  selector: 'mn-eclipse-button',
  templateUrl: './eclipse-button.component.html'
})

export class EclipseButtonComponent implements OnInit, OnDestroy {

  /**
   * Emits when the button is clicked
   */
  @Output() clicked = new EventEmitter<void>();

  /**
   * Provide a label for the button, if set the icon is hidden
   */
  @Input() label: string;
  @Input() isLabelVisible = false;
  isFeatureActive: boolean;
  reachable: boolean;
  isDestroyed = false;
  defaultTooltipPlacement = 'bottom';

  constructor(
    private deepLinkService: DeepLinkService,
    private translateService: TranslateService
  ) {}

  /**
   * Set the placement odf the tooltip, default value is bottom
   */
  @Input() set tooltipPlacement(direction: string) {
    this.defaultTooltipPlacement = direction;
  }

  ngOnInit(): void {
    this.deepLinkService.featureIsActive().subscribe(resp => {
      this.isFeatureActive = resp;
      if (this.isFeatureActive) {
        this.checkReachable();
      }
    });
  }

  /**
   * Get the icon reflecting the current state of the button
   */
  getIconType(): string {
    return this.reachable ? pathIconOnline : pathIconOffline;
  }

  /**
   * Get the text for the tooltip reflecting the current state of the button
   */
  getTooltipText(): string {
    return this.reachable ? this.translateService.instant('eclipseDeeplink.tooltipOn') : this.translateService.instant('eclipseDeeplink.tooltipOff');
  }

  /**
   * Emit the an event for the parent component when the button is clicked
   */
  onClick(): void {
    this.clicked.emit();
  }

  ngOnDestroy(): void {
    this.isDestroyed = true;
  }

  private checkReachable(): void {
    this.deepLinkService.heartbeat().then(res => {
      this.reachable = res;
    }).finally(() => {
      if ( ! this.isDestroyed) {
        setTimeout(() => this.checkReachable(), 5000);
      }
    });
  }
}
