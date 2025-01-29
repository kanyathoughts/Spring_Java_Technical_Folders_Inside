import { Component } from '@angular/core';
import { getBasePath } from '@app/core/utils/base-path.utils';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';

const maxIndex = 2;

@Component({
  selector: 'functional-block-ui',
  templateUrl: './functional-block-ui-product-vision.component.html'
})
export class FunctionalBlockUiProductVisionComponent {

  path = ['Business Applications', 'Demo Project A', 'New Business', 'Provide agent hierarchy validation', 'Users'];
  imagePath = getBasePath() + '/assets/block-img/level-1.png';
  imagePaths = [
    getBasePath() + '/assets/block-img/level-1.png',
    getBasePath() + '/assets/block-img/level-2.png',
    getBasePath() + '/assets/block-img/level-3.png'
  ];
  metricsPath = getBasePath() + '/assets/block-img/side-panel-metrics.png';
  functionalGroupsPath = getBasePath() + '/assets/block-img/side-panel-functional-groups.png';
  codeViewerPaths = [
    getBasePath() + '/assets/block-img/code-viewer-1.png',
    getBasePath() + '/assets/block-img/code-viewer-2.png',
    getBasePath() +'/assets/block-img/code-viewer-3.png',
    getBasePath() + '/assets/block-img/code-viewer-4.png'
  ];
  max = 2;
  imageIndex = 0;
  projectId: number;
  clientProjectRelationship: ClientProjectRelationship;
  isCollapsed = true;
  showCode = false;
  codeEnabled = false;

  public onImgClick(): void {
    this.goDeeper();
  }

  public goDeeper(): void {
    if (this.imageIndex < maxIndex) {
      this.imageIndex++;
      this.max++;
      this.imagePath = this.imagePaths[this.imageIndex];
      this.closeSidePanel();
    }
    this.evalCodeEnabled();
  }

  public onBreadCrumbClick(i: number): void {
    if (i > 0) {
      this.imageIndex = i - 1;
      this.imagePath = this.imagePaths[this.imageIndex];
      this.max = i + 1;
      this.closeSidePanel();
    }
    this.evalCodeEnabled();
  }

  public openCodeViewer(): void {
    this.showCode = true;
  }

  public toggleCodeViewer(): void {
    this.showCode = ! this.showCode;
  }

  public toggleSidePanel(): void {
    this.isCollapsed = ! this.isCollapsed;
  }

  public openSidePanel(): void {
    this.isCollapsed = false;
  }

  public closeSidePanel(): void {
    this.isCollapsed = true;
    this.showCode = false;
  }

  private evalCodeEnabled(): void {
    this.codeEnabled = this.imageIndex === this.imagePaths.length - 1;
  }
}
