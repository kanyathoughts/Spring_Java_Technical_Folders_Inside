
import { Component, Input, OnInit } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { GraphComponent, GraphMLIOHandler, IGraph, ILabel, Insets, Rect, Size, SvgExport } from 'yfiles';
import FileSaveSupport from '../../../core/utils/file-save-support.utils';
import { Canvg } from 'canvg';
import { jsPDF } from 'jspdf';
import 'svg2pdf.js';
import { Logger } from '@app/core';
import { Svg2pdfOptions } from 'svg2pdf.js';

const log = new Logger('GraphExportComponent');

@Component({
  selector: 'mining-graphs-export',
  templateUrl: './mining-graphs-export.component.html'
})
export class MiningGraphsExportComponent implements OnInit {

  static SEVERITY_WARN = 'warn';
  static SEVERITY_ERROR = 'error';
  @Input()
  public graphComponent: GraphComponent;
  @Input()
  public moduleName: string;
  @Input()
  public projectId: string;
  @Input()
  public moduleId: string;
  @Input()
  isGraphEmpty: boolean;
  modalVisible = false;
  emptyGraphExportErrorMsg: string;
  dropdownList: string[] = ['GraphML', 'PDF', 'SVG'];
  private graphMLIOHandler = new GraphMLIOHandler();

  constructor(
    private translateService: TranslateService,
    private messageService: NzMessageService) { }

  ngOnInit(): void {
    this.emptyGraphExportErrorMsg = this.translateService.instant('controlFlowGraph.emptyGraphExportError');
  }

  /**
   * Creates an error message.
   * @param errorMessage
   * @param severityLevel
   */
   displayErrorMessage(errorMessage: string, severityLevel: string): void {
    this.messageService.create(severityLevel, `${this.translateService.instant(errorMessage)}`);
  }

  /**
   * Saves graph in different format as per selected menu.
   * @returns void
   */
  public download(menu: string): void {
    switch (menu) {
      case 'GraphML':
        void this.saveAsGraphml();
        break;
      case 'PDF':
        void this.exportPDF();
        break;
      case 'SVG':
        this.saveAsSVG();
        break;
      case 'PNG':
        void this.saveAsPNG();
        break;
    }
  }

  handleModalConfirm(): void {
    this.saveAsSVG();
    this.modalVisible = false;
  }

  handleModalCancel(): void {
    this.modalVisible = false;
  }

  /**
   * Saves graph (Dependency/CFG) in SVG format.
   * @returns void
   */
   private saveAsSVG(): void {
    if (this.isGraphEmpty) {
      this.displayErrorMessage(this.emptyGraphExportErrorMsg, MiningGraphsExportComponent.SEVERITY_WARN);
      return;
    }
    this.graphComponent.selection.clear();
    const fileContent = this.getGraphSvgString();
    void FileSaveSupport.save(fileContent, this.moduleName + '.svg');
  }

  /**
   * Extracts SVG string for a given SVG graph using SvgExport
   */
   private getGraphSvgString(): string {
    const exportComponent = this.createExportComponentForSave();
    const targetRect: Rect = exportComponent.contentRect;
    exportComponent.zoomTo(targetRect);
    this.graphComponent.updateContentRect();
    const exporter = new SvgExport(
      {
        x: this.graphComponent.contentRect.x,
        y: this.graphComponent.contentRect.y,
        width: this.graphComponent.contentRect.width + 10,
        height: this.graphComponent.contentRect.height + 10
      }, 1);
    exporter.inlineSvgImages = true;
    const svgElement = exporter.exportSvg(this.graphComponent);
    return SvgExport.exportSvgString(svgElement);
  }

  /**
   * Creates a graph component from existing graph.
   * @returns GraphComponent
   */
   private createExportComponentForSave(): GraphComponent {
    const exportComponent = new GraphComponent();
    exportComponent.graph = this.graphComponent.graph;
    exportComponent.selection.clear();
    exportComponent.updateContentRect();
    return exportComponent;
  }

  /**
   * Saves graph (Dependency/CFG) in PNG format.
   * @returns Promise<void>
   */
   private async saveAsPNG(): Promise<void> {
    if (this.isGraphEmpty) {
      this.displayErrorMessage(this.emptyGraphExportErrorMsg, MiningGraphsExportComponent.SEVERITY_WARN);
      return;
    }
    const exportComponent = this.createExportComponentForSave();
    const targetRect = exportComponent.contentRect;

    if (targetRect.width > 15000 || targetRect.height > 15000) {
      this.modalVisible = true;
    } else {
      exportComponent.zoomTo(targetRect);
      const exporter = new SvgExport({
        x: targetRect.x,
        y: targetRect.y,
        width: targetRect.width + 10,
        height: targetRect.height + 10
      }, 1);

      if (window.btoa != null) {
        exporter.encodeImagesBase64 = true;
        exporter.inlineSvgImages = true;
      }
      const svgElement = exporter.exportSvg(exportComponent) as SVGElement;
      const image = await this.renderSvgToPng(svgElement, new Size(exporter.viewWidth, exporter.viewHeight), exporter.margins);
      if (image !== null) {
        void FileSaveSupport.save(image.src, this.moduleName + '.png');
      } else {
        this.displayErrorMessage(`${this.translateService.instant('controlFlowGraph.pngExportError')}`, MiningGraphsExportComponent.SEVERITY_ERROR);
      }
    }
  }

  /**
   * Convert an SVG image to PNG format.
   * @param SVGElement
   * @param Size
   * @param Insets
   * @returns Promise<HTMLImageElement>
   */
   private async renderSvgToPng(svgElement: SVGElement, size: Size, margins: Insets): Promise<HTMLImageElement> {
    const targetCanvas = document.createElement('canvas');
    const svgString = SvgExport.exportSvgString(svgElement);
    // eslint-disable-next-line @typescript-eslint/no-misused-promises
    return new Promise(async resolve => {
      targetCanvas.width = size.width + (margins.left + margins.right);
      targetCanvas.height = size.height + (margins.top + margins.bottom);
      const renderedCanvas = await Canvg.from(targetCanvas.getContext('2d'), svgString);
      await renderedCanvas.render();
        try {
          const pngImage = new Image();
          pngImage.src = targetCanvas.toDataURL('image/png');
          resolve(pngImage);
        } catch (error) {
          log.error(`
            ProjectID(${this.projectId})
            ModuleID(${this.moduleId})
            Error Occured while converting SVG to PNG: ${error.message}`);
          resolve(null);
        }
    });
  }

  /**
   * Saves graph (Dependency/CFG) in PDF format.
   */
   private async exportPDF(): Promise<void> {
    if (this.isGraphEmpty) {
      this.displayErrorMessage(this.emptyGraphExportErrorMsg, MiningGraphsExportComponent.SEVERITY_WARN);
      return;
    }
    const { raw } = await this.getPdfUri();
    void FileSaveSupport.save(raw, this.moduleName + '.pdf');
  }

  /**
   * Intermediate method to convert GraphComponent to PDf using jsPdf
   * @return A promise resolving to the jsPDF document.
   */
   private async getPdfUri() {
    const exportComponent = this.createExportComponentForSave();
    const targetRect = exportComponent.contentRect;
    exportComponent.zoomTo(targetRect);
    const exporter = new SvgExport({
      x: targetRect.x,
      y: targetRect.y,
      width: targetRect.width + 10,
      height: targetRect.height + 10
    }, 1);
    if (window.btoa != null) {
      // Don't use base 64 encoding if btoa is not available and don't inline images as-well.
      // Otherwise canvg will throw an exception.
      exporter.encodeImagesBase64 = true;
      exporter.inlineSvgImages = true;
    }
    const svgElement = await exporter.exportSvgAsync(exportComponent) as SVGElement;
    const maxPdfUnit = 14400;
    if (exporter.viewWidth > maxPdfUnit || exporter.viewHeight > maxPdfUnit) {
      // Scaling down the SVGElement width and height, so that it can fit in a PDF
      const originalWidth = exporter.viewWidth;
      const originalHeight = exporter.viewHeight;
      const margin = 10;
      let scale;
      if (originalWidth > originalHeight) {
        scale = maxPdfUnit / (originalWidth + 2.5 * margin);
      } else {
        scale = maxPdfUnit/  (originalHeight + 2.5 * margin);
      }
      const roundedScale = Math.floor(scale * 10) / 10;
      scale = parseFloat(roundedScale.toFixed(1));
      svgElement.setAttribute('transform', `scale(${scale})`);
      const newWidth = Number(originalWidth) * scale;
      const newHeight = Number(originalHeight) * scale;

      svgElement.setAttribute('width', newWidth + 'px');
      svgElement.setAttribute('height', newHeight + 'px');
      return this.convertSvgToPdf(svgElement, new Size(newWidth, newHeight), new Insets(margin));
    }
    return this.convertSvgToPdf(svgElement, new Size(exporter.viewWidth, exporter.viewHeight), new Insets(5));
  }

  /**
   * Converts a SVG image to PDF format using jsPdf.
   * @param SVGElement
   * @param Size
   * @param Insets
   * @return Promise<{ raw: string; uri: string; }>
   * @see jsPdf#svg
   */
   private convertSvgToPdf(svgElement: SVGElement, size: Size, margins: Insets): Promise<{ raw: string; uri: string; }> {
    const margin = margins ? Math.max(margins.left, margins.right, margins.top, margins.bottom) : 5;
    const sizeArray = new Array(2);
    sizeArray[0] = size.width + 2.5 * margin;
    sizeArray[1] = size.height + 2.5 * margin;
    const jsPdf = new jsPDF({
      orientation: sizeArray[0] > sizeArray[1] ? 'l' : 'p',
      unit: 'pt',
      format: sizeArray,
      compress: true,
    });
    const offsets: Svg2pdfOptions = {};
    offsets.x = margin + 10;
    offsets.y = margin + 10;
    return jsPdf.svg(svgElement, offsets).then(() => ({ raw: jsPdf.output(), uri: jsPdf.output('datauristring') }));
  }

  /**
   * Saves graph (Dependency/CFG) in GraphMl format.
   */
  private async saveAsGraphml(): Promise<void> {
    if (this.isGraphEmpty) {
      this.displayErrorMessage(this.emptyGraphExportErrorMsg, MiningGraphsExportComponent.SEVERITY_WARN);
      return;
    }
    const graph: IGraph =  this.graphComponent.graph;
    const textLabel: string[] = [];
    graph.nodeLabels.forEach((label: ILabel) => {
      textLabel.push(label.text);
      const labelText = label.text.replace(/(.{35})/g, '$1\n');
      graph.setLabelText(label, labelText);
    });
    const result = await this.graphMLIOHandler.write(graph);
    void FileSaveSupport.save(result, this.moduleName + '.graphml');
    // This is done to ensure original graph label remains same.
    graph.nodeLabels.forEach((label: ILabel, index: number) => {
      graph.setLabelText(label, textLabel[index]);
    });
  }
}
