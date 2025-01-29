import { Injectable } from '@angular/core';
import { MiningCodeViewerComponent } from './mining-code-viewer.component';
import { DataDictionaryPojo } from '@innowake/mining-api-angular-client';
import { languages, editor } from 'monaco-editor/esm/vs/editor/editor.api';

export const COMMAND_OPEN_DICT = 'OPEN_DATA_DICTIONARY';
export const COMMAND_REMOVE_DICT = 'REMOVE_DATA_DICTIONARY';

@Injectable({
    providedIn: 'root'
})
export class HoverProviderService implements languages.HoverProvider {

    public codeViewer: MiningCodeViewerComponent;

    provideHover(model: editor.ITextModel, position: {
        lineNumber: number;
        column: number
    }): languages.Hover {
        const hoverLine = position.lineNumber;
        const hoverColumn = position.column;
        let contents = null;
        const realEditorOffset = this.codeViewer.calculateRealOffset(hoverLine, hoverColumn);
        this.codeViewer.dataDictionaryEntryMap.forEach((entry: DataDictionaryPojo) => {
            const offset = entry.location.offset;
            const length = entry.location.length;
            if (realEditorOffset >= offset && realEditorOffset < offset + length) {
                this.codeViewer.currentEntryId = entry.id;
                contents = [
                    { isTrusted: true, value: '**Data dictionary entry**' },
                    { isTrusted: true, value: entry.description }
                ];
                if (this.codeViewer.hasAstNodes) {
                    contents.push({
                        isTrusted: true, value: '[Edit](command:' + COMMAND_OPEN_DICT + ') | [Remove](command:' + COMMAND_REMOVE_DICT + ')'
                    });
                } else {
                    contents.push({ isTrusted: true, value: '[View](command:' + COMMAND_OPEN_DICT + ')' });
                }
            }
        });
        return { contents };
    }
}
