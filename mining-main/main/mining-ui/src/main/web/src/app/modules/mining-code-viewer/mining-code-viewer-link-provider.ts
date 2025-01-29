import { FeatureToggleService } from '@app/core/services/feature-toggle/feature-toggle.service';
import { CodeViewerLink, CodeViewerLinkModel, CodeViewerRange, EntityId, ModuleControllerService } from '@innowake/mining-api-angular-client';
import * as monaco from 'monaco-editor';
import { firstValueFrom } from 'rxjs';

export const COMMAND_OPEN_CODE_VIEWER_LINK = 'OPEN_CODE_VIEWER_LINK';
export const COMMAND_PEEK_DATAFLOW_REFERENCES = 'PEEK_DATAFLOW_REFERENCES';

/**
 * LinkProvider and ReferenceProvider for the Code Viewer component.
 *
 * This class acts as both LinkProvider and ReferenceProvider.
 *
 * The links provided via the `provideLinks()` method will show up as links in the code viewer. The user
 * can hover over them to display a popup and Ctrl+Click on them to follow the link.
 *
 * Additionally, the references provided via the `provideReferences()` method allows to use the
 * "Peek References" and "Go To References" features of Monaco. This is enabled only for references within the same file.
 */
export class LinkAndReferenceProvider implements monaco.languages.LinkProvider, monaco.languages.ReferenceProvider {

    /** static variable set to true during execution of COMMAND_PEEK_DATAFLOW_REFERENCES  */
    static provideDataFlowLinks: boolean;

    private linkModelPromise: Promise<CodeViewerLinkModel>;

    constructor(private readonly featureToggleService: FeatureToggleService,
                private readonly moduleController: ModuleControllerService,
                private readonly projectId: number,
                private readonly moduleId: EntityId,
                private readonly assembled: boolean) {

        this.linkModelPromise = this.loadModel();
    }

    async provideLinks(): Promise<monaco.languages.ILinksList> {
        const linkModel = await this.linkModelPromise;
        return {
            links: linkModel.links.map(codeViewerLink => ({
                range: codeViewerLink.fromRange as monaco.IRange,
                tooltip: codeViewerLink.relationshipLabel + ' ' + codeViewerLink.targetLabel ,
                url: this.createUrl(codeViewerLink)
            }))
        };
    }

    async provideReferences(model: monaco.editor.ITextModel, position: monaco.Position): Promise<monaco.languages.Location[]> {
        if (LinkAndReferenceProvider.provideDataFlowLinks) {
            /* clear the flag */
            LinkAndReferenceProvider.provideDataFlowLinks = false;
            const linkModel = await firstValueFrom(this.moduleController.getCodeViewerDataFlowLinks(this.projectId, this.moduleId,
                model.getOffsetAt(position), this.assembled));
            return linkModel.links.map(codeViewerLink => ({
                uri: model.uri,
                range: codeViewerLink.toRange as monaco.IRange
            }));
        } else {
            const linkModel = await this.linkModelPromise;
            const matchingLinks = linkModel.links.filter(codeViewerLink => codeViewerLink.linkTargetType === 'LOCAL'
                    && this.matchesRange(codeViewerLink.fromRange, position.lineNumber, position.column));

            return matchingLinks.map(codeViewerLink => ({
                uri: model.uri,
                range: codeViewerLink.toRange as monaco.IRange
            }));
        }
    }

    private async loadModel(): Promise<CodeViewerLinkModel> {
        const enabled = await firstValueFrom(this.featureToggleService.isActive('codeViewerHyperlinking'));
        if (enabled) {
            return firstValueFrom(this.moduleController.getCodeViewerLinks(this.projectId, this.moduleId, this.assembled));
        } else {
            /* return empty model when feature is disabled */
            return { links: [] };
        }
    }

    private createUrl(codeViewerLink: CodeViewerLink): string {
        return 'command:' + COMMAND_OPEN_CODE_VIEWER_LINK + '?' + encodeURIComponent(JSON.stringify(codeViewerLink));
    }

    private matchesRange(range: CodeViewerRange, lineNumber: number, column: number): boolean {
        if (lineNumber > range.startLineNumber && lineNumber < range.endLineNumber) {
            return true;
        }
        if (lineNumber === range.startLineNumber) {
            return column >= range.startColumn;
        }
        if (lineNumber === range.endLineNumber) {
            return column <= range.endColumn;
        }
        return false;
    }
}
