import { Component, Input } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

const headerTemplate = `<div>{{ translatedTitle }}</div>
<span class="shared-annotation-header__path">
    {{ translatedPath }}
</span>`;

export const getHeaderTemplate = (translatedTitle: string, translatedPath: string): string =>
  headerTemplate.replace('{{ translatedTitle }}', translatedTitle).replace('{{ translatedPath }}', translatedPath);

@Component({
  selector: 'app-shared-editor-header',
  template: headerTemplate
})
export class SharedEditorHeaderComponent {

  translatedTitle: string;

  translatedPath: string;

  constructor(private translationService: TranslateService) {}

  @Input() set moduleTitle(title: string) {
    this.translatedTitle = title;
  }

  @Input() set modulePath(path: string) {
    path = path ?? this.translationService.instant('notAvailable');
    this.translatedPath = this.translationService.instant('annotationReporting.sharedAnnotationEditorPath', { modulePath: path });
  }
}
