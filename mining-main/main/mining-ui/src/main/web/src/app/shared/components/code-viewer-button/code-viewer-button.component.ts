import { Component, EventEmitter, Inject, Input, Output } from '@angular/core';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { WindowToken } from '@app/core/utils/window';
import { openInNewTab } from '@app/core/utils/window-open.utils';
import { ModulePojo } from '@innowake/mining-api-angular-client';

declare const java__callback__openMiningUiPage: (path: string) => Promise<string>;

export const handleCodeViewerButton = (moduleDetails: ModulePojo): { disableButton: boolean; toolTip: string } => {
  if ( ! moduleDetails?.sourceCodeAvailable) {
    if (moduleDetails?.identification === ModulePojo.IdentificationEnum.MISSING) {
      return { disableButton: true, toolTip: 'module.codeViewerMissingTooltip' };
    } else {
      return { disableButton: true, toolTip: 'module.codeViewerNotAvailableTooltip' };
    }
  } else if (moduleDetails?.storage === ModulePojo.StorageEnum.FILE_SECTION && moduleDetails?.parent !== null) {
    return { disableButton: false, toolTip: 'module.codeViewerAnotherModuleTooltip' };
  }
  return { disableButton: false, toolTip: 'iconToolTip.codeViewer' };
};

@Component({
    selector: 'mn-code-viewer-button',
    templateUrl: './code-viewer-button.component.html'
})
export class CodeViewerButtonComponent {

  @Input() label: string;
  @Input() moduleDetails: ModulePojo;
  @Input() projectId: number;
  @Input() contextMenu = false;
  @Input() eclipseView: boolean;
  @Input() offset = 0;
  @Input() isDefaultTooltip = false;
  @Output() codeViewerClick: EventEmitter<MouseEvent> = new EventEmitter();
  codeViewerTooltip: string;
  disableCodeViewer = false;

  constructor(@Inject(WindowToken) private $window: Window) {}

  /**
   * Method that redirects to the code viewer in new tab.
   * @param path route to be navigated to.
   */
  openInNewBrowserTab(path: string): void {
    if (this.eclipseView) {
      void java__callback__openMiningUiPage('/#' + RouteBuilder.buildModuleRoute(this.projectId, this.moduleDetails.linkHash, path));
    } else {
      if (this.offset !== 0) {
        path = 'code-viewer?offset=' + this.offset;
      }
      if (this.moduleDetails.storage === ModulePojo.StorageEnum.FILE_SECTION && this.moduleDetails.parentId !== null) {
        openInNewTab(this.projectId, this.moduleDetails.parentId, path, this.$window);
      } else {
        openInNewTab(this.projectId, this.moduleDetails.id, path, this.$window);
      }
    }
  }

  /**
   * Method to handle code viewer button for different modules
   * @returns tooltip text for code viewer button.
   */
  handleCodeViewerButton(): string {
    this.codeViewerTooltip = 'iconToolTip.codeViewer';
    if (this.isDefaultTooltip) {
      this.disableCodeViewer = false;
    } else {
      this.disableCodeViewer = false;
      const codeViewerData = handleCodeViewerButton(this.moduleDetails);
      this.disableCodeViewer = codeViewerData.disableButton;
      this.codeViewerTooltip = codeViewerData.toolTip;
    }
    return this.codeViewerTooltip;
  }
}
