import { Component, EventEmitter, forwardRef, Input, Output } from '@angular/core';
import { NG_VALUE_ACCESSOR } from '@angular/forms';

/**
 * Those class mock the behavior of the MonacoEditorModule for the MiningCodeViewerComponent
 */
@Component({
    selector: 'mining-monaco-editor',
    template: `<div>
        <div id="line1"></div>
        <div id="code-annotation-editor-component-1"></div>

        <div id="line1"></div>
        <svg id="lineSvg" class="annotation-editor-container__monaco-editor-sidebar--svg">
            <line id="svgLineStart1" x1="0" y1="76" x2="15" y2="76" stroke="#0095FF" stroke-width="1"></line>
            <line id="svgLine1" x1="15" y1="76" x2="75" y2="76" stroke="#0095FF" stroke-width="1"></line>
            <line id="svgLineEnd1" x1="75" y1="76" x2="90" y2="76" stroke="#0095FF" stroke-width="1"></line>
        </svg>

    </div>`,
    providers: [
        {
            provide: NG_VALUE_ACCESSOR,
            useExisting: forwardRef(() => MonacoEditorMockComponent),
            multi: true
        }
    ]
})
export class MonacoEditorMockComponent {
    @Input() ngModel: string;
    @Output() ngModelChange = new EventEmitter<string>();
    code: string;
    model: NgxEditorModelMock = {
        code: 'Mock'
    };
    constructor() { }

    writeValue(value: string): void {
        this.code = value ? value : '';
    }

    registerOnChange(fn: any): void { }

    registerOnTouched(fn: any): void { }

    setDisabledState?(isDisabled: boolean): void { }

    deltaDecorations(array: [], array2: []) { }

    addAction(obj: any) { }

    getSelection(): MonacoSelectionMockComponent {
        return new MonacoSelectionMockComponent();
    }

    onDidChangeCursorSelection() { }

    getScrollHeight() { }

    getModel() {
        return new EditorModelMock(this.model.code);
    }
    changeViewZones() { }

    onDidScrollChange() {}

    revealLineInCenter() {}
}

export interface NgxEditorModelMock {
    code: string;
    language?: string;
    uri?: any;
}

export class MonacoSelectionMockComponent {
    position: MonacoPositionMock = { lineNumber: 1, column: 1 };
    constructor() { }
    getStartPosition() {
        return this.position;
    }
    getEndPosition() {
        return this.position;
    }
}

export interface MonacoPositionMock {
    lineNumber: number;
    column: number;
}

export class EditorModelMock {
    value: string;
    constructor(arg: string) { this.value = arg; }
    getValueInRange() {
        return this.value;
    }
}
