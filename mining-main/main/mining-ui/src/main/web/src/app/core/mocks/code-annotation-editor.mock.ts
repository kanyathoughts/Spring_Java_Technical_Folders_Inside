import { Component, EventEmitter, Input, Output } from "@angular/core";
import { AnnotationElementData } from "@app/modules/mining-code-viewer/mining-code-viewer";

@Component({
  selector: 'code-annotation-editor-component',
  template: ''
})
export class CodeAnnotationEditorMock {
  @Input() data: AnnotationElementData;
  @Input() annotationsOfModule: any[] = [];
  @Input() projectId: number;
  @Input() deletedCallback?: () => void;
  @Input() isCfg?: boolean;
  @Input() openEditor?: () => void;
  @Input() showError: (message: string) => void;
  @Input() disableActionButtons?: boolean;
  @Input() projectLevelFgs: number;
  @Output() currentAnnotationElement = new EventEmitter<{ type: string, annotation: AnnotationElementData }>();
}