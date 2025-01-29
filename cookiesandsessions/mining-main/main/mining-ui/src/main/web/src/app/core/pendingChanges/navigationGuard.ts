import { Injectable } from '@angular/core';

import { SharedAnnotationEditorService } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.service';
import { Observable } from 'rxjs';

@Injectable({
    providedIn: 'root'
})
export class NavigationGuard  {
    constructor(private sharedAnnotationEditorService: SharedAnnotationEditorService) { }
    canDeactivate(): Observable<boolean> | Promise<boolean> | boolean {
        if (this.sharedAnnotationEditorService.getEditorFormState()) {
            return confirm('There are unsaved changes. Are you sure you want to leave?');
        }
        return true;
    }
}
