import { AssembledContent, CodeViewerLink } from '@innowake/mining-api-angular-client';
import { COMMAND_OPEN_CODE_VIEWER_LINK } from './mining-code-viewer-link-provider';
import { IRange, languages } from 'monaco-editor/esm/vs/editor/editor.api';

/**
 * CodeLens provider for the Code Viewer component.
 *
 * This class adds CodeLenses (small line links) above each included area
 * in the assembled view. Clicking the links opens the module from which the code was included.
 */
export class CodeLensProvider implements languages.CodeLensProvider {

    constructor(private readonly assembledContent: AssembledContent | undefined) {}

    provideCodeLenses(): languages.CodeLensList {
        if (this.assembledContent === undefined) {
            return {
                lenses: [],
                dispose: () => {}
            };
        }

        return {
            lenses: this.assembledContent.inclusions.map(inclusion => ({
                range: inclusion.assembledRange as IRange,
                command: {
                  id: COMMAND_OPEN_CODE_VIEWER_LINK,
                  title: 'Included from: ' + inclusion.originModule.name,
                  arguments: /* as CodeViewerLink */ [{
                    linkTargetType: CodeViewerLink.LinkTargetTypeEnum.EXTERNAL,
                    toModuleId: inclusion.originModule.id
                  }]
                }
            })),
            dispose: () => {}
        };
    }
}
